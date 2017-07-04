use lexer;
use token;
use ast;

#[derive(Debug)]
pub struct Parser {
    lexer: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
    errors: Vec<ParseError>,
}

#[derive(PartialEq, Debug)]
struct ParseError {
    expected: token::TokenType,
    actual: token::TokenType,
    pos: usize,
}

#[derive(PartialEq, PartialOrd, Debug)]
enum Precedence {
    LOWEST = 0,
    EQUALS = 1,
    LESSGREATER = 2,
    SUM = 3,
    PRODUCT = 4,
    PREFIX = 5,
    CALL = 6,
}

impl Precedence {
    pub fn from_token(t: token::TokenType) -> Precedence {
        match t {
            token::EQ | token::NOT_EQ => Precedence::EQUALS,
            token::LT | token::GT => Precedence::LESSGREATER,
            token::PLUS | token::MINUS => Precedence::SUM,
            token::SLASH | token::ASTERISK => Precedence::PRODUCT,
            token::LPAREN => Precedence::CALL,
            _ => Precedence::LOWEST,
        }
    }
}

impl Parser {
    pub fn new(mut l: lexer::Lexer) -> Parser {
        let cur = l.next_token();
        let peek = l.next_token();
        Parser {
            lexer: l,
            cur_token: cur,
            peek_token: peek,
            errors: vec![],
        }
    }

    fn next_token(&mut self) -> token::Token {
        use std::mem;
        let peek = self.lexer.next_token();

        let cur = mem::replace(&mut self.peek_token, peek);
        mem::replace(&mut self.cur_token, cur)
    }

    fn cur_token_is(&self, t: token::TokenType) -> bool {
        self.cur_token.typ == t
    }

    fn peek_token_is(&self, t: token::TokenType) -> bool {
        self.peek_token.typ == t
    }

    fn expect_peek(&mut self, t: token::TokenType) -> bool {
        if self.peek_token_is(t) {
            true
        } else {
            self.errors.push(ParseError {
                expected: t,
                actual: self.peek_token.typ,
                pos: self.lexer.pos,
            });
            false
        }
    }


    fn peek_precedence(&self) -> Precedence {
        Precedence::from_token(self.peek_token.typ)
    }

    fn cur_precedence(&self) -> Precedence {
        Precedence::from_token(self.cur_token.typ)
    }


    fn parse_expression(&mut self, p: Precedence) -> Option<ast::Expression> {
        let mut left = match self.cur_token.typ {
            token::IDENT => self.parse_identifier(),
            token::INT => self.parse_int(),
            token::BANG => self.parse_prefix_expression(),
            token::MINUS => self.parse_prefix_expression(),
            token::TRUE | token::FALSE => self.parse_boolean(),
            token::LPAREN => self.parse_grouped_expression(),
            token::IF => self.parse_if_expression(),
            x => {
                println!("No prefix token: {}", x);
                None
            }
        };

        if left.is_none() {
            return None;
        }

        while !self.peek_token_is(token::SEMICOLON) && p < self.peek_precedence() {
            println!("left: {:?}, self: {:?}", left, self.peek_token);
            let new = left.clone().and_then(|exp| match self.peek_token.typ {
                token::PLUS | token::MINUS => {
                    self.next_token();
                    self.parse_infix_expression(exp)
                }
                token::SLASH | token::ASTERISK => {
                    self.next_token();
                    self.parse_infix_expression(exp)
                }
                token::EQ | token::NOT_EQ => {
                    self.next_token();
                    self.parse_infix_expression(exp)
                }
                token::LT | token::GT => {
                    self.next_token();
                    self.parse_infix_expression(exp)
                }
                x => None,
            });

            if new.is_none() {
                return left;
            }
            left = new;
        }
        left
    }

    fn parse_if_expression(&mut self) -> Option<ast::Expression> {
        if !self.expect_peek(token::LPAREN) {
            return None;
        }

        let tok = self.next_token();

        self.next_token();

        let cond = self.parse_expression(Precedence::LOWEST);

        if !self.expect_peek(token::RPAREN) {
            return None;
        }
        self.next_token();

        if !self.expect_peek(token::LBRACE) {
            return None;
        }

        cond.and_then(|cond_exp| {
            self.parse_block_statement().map(|block| {

                let alt = if self.peek_token_is(token::ELSE) {
                    self.next_token();

                    if !self.expect_peek(token::LBRACE) {
                       None
                    } else {
                        self.next_token();
                        self.parse_block_statement()
                    }
                } else { None };

                ast::Expression::If(ast::IfExpression {
                    token: tok,
                    condition: Box::new(cond_exp),
                    consequence: Box::new(block),
                    alternative: alt.map(|a| Box::new(a)),
                })
            })
        })
    }

    fn parse_block_statement(&mut self) -> Option<ast::Statement> {
        let tok = self.next_token();
        let mut statements: Vec<ast::Statement> = vec![];

        while !self.cur_token_is(token::RBRACE) {
            self.parse_statement().map(|stmt| statements.push(stmt));
            self.next_token();
        }

        Some(ast::Statement::Block(ast::BlockStatement {
            token: tok,
            statements: statements,
        }))
    }
    fn parse_grouped_expression(&mut self) -> Option<ast::Expression> {
        self.next_token();

        let exp = self.parse_expression(Precedence::LOWEST);

        if !self.expect_peek(token::RPAREN) {
            return None;
        }
        self.next_token();
        exp
    }

    fn parse_int(&self) -> Option<ast::Expression> {
        let tok = self.cur_token.clone();
        Some(ast::Expression::Integer(ast::IntegerLiteral {
            value: tok.literal.parse().unwrap(),
            token: tok,
        }))
    }

    fn parse_boolean(&self) -> Option<ast::Expression> {
        let tok = self.cur_token.clone();
        Some(ast::Expression::Boolean(ast::BooleanExpression {
            token: tok,
            value: self.cur_token_is(token::TRUE),
        }))
    }

    fn parse_identifier(&self) -> Option<ast::Expression> {
        let tok = self.cur_token.clone();
        Some(ast::Expression::Identifier(ast::IdentifierExpression {
            value: tok.literal.clone(),
            token: tok,
        }))
    }

    fn parse_return_statement(&mut self) -> Option<ast::Statement> {
        let stmt = ast::Statement::Return(ast::ReturnStatement {
            token: self.next_token(),
            value: ast::Expression::Unit(ast::UnitExpression()),
        });

        while !self.cur_token_is(token::SEMICOLON) {
            self.next_token();
        }
        Some(stmt)
    }

    fn parse_prefix_expression(&mut self) -> Option<ast::Expression> {
        let tok = self.next_token();

        self.parse_expression(Precedence::PREFIX).map(|right| {
            ast::Expression::Prefix(ast::PrefixExpression {
                operator: tok.literal.clone(),
                token: tok,
                right: Box::new(right),
            })
        })
    }

    fn parse_infix_expression(&mut self, left: ast::Expression) -> Option<ast::Expression> {
        let precedence = self.cur_precedence();
        let tok = self.next_token();

        self.parse_expression(precedence).map(|right| {
            ast::Expression::Infix(ast::InfixExpression {
                operator: tok.literal.clone(),
                token: tok,
                left: Box::new(left),
                right: Box::new(right),
            })
        })
    }

    fn parse_let_statement(&mut self) -> Option<ast::Statement> {

        if !self.expect_peek(token::IDENT) {
            return None;
        }
        let let_tok = self.next_token();

        if !self.expect_peek(token::ASSIGN) {
            return None;
        }

        let ident_tok = self.next_token();
        let name = ast::IdentifierExpression::new(ident_tok);

        while !self.cur_token_is(token::SEMICOLON) {
            self.next_token();
        }

        Some(ast::Statement::Let(ast::LetStatement {
            token: let_tok,
            name: name,
            value: ast::Expression::Unit(ast::UnitExpression()),
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<ast::Statement> {
        let tok = self.cur_token.clone();
        let exp = self.parse_expression(Precedence::LOWEST);


        if self.peek_token_is(token::SEMICOLON) {
            self.next_token();
        }

        exp.map(|ex| {
            ast::Statement::Expression(ast::ExpressionStatement {
                token: tok,
                value: ex,
            })
        })

    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.cur_token.typ {
            token::LET => self.parse_let_statement(),
            token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse(&mut self) -> ast::Program {
        let mut statements: Vec<ast::Statement> = vec![];


        while self.cur_token.typ != token::EOF {
            let stmt = self.parse_statement();
            match stmt {
                Some(s) => statements.push(s),
                None => (),
            }
            self.next_token();
        }
        ast::Program { statements: statements }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn make_parser(s: &'static str) -> Parser {
        Parser::new(make_lexer(s))
    }

    fn make_lexer(s: &'static str) -> lexer::Lexer {
        lexer::Lexer::new(String::from(s))
    }

    struct test_case(&'static str);

    fn test_let_statement(s: &ast::Statement, t: &test_case) {
        use ast::Node;
        assert_eq!(s.token_literal(), "let");
        match *s {
            ast::Statement::Let(ref stmt) => {
                assert_eq!(stmt.name.value, t.0);
            }
            _ => assert!(false),
        }
    }

    fn test_no_errors(p: &Parser) {
        if p.errors.len() > 0 {
            println!("Expected no errors, got {:?}", p.errors);
            assert!(false)
        }
    }
    fn test_error(p: &Parser, err: ParseError) {
        let b = p.errors.iter().any(|e| *e == err);
        if !b {
            let err = format!("Expected {:?} to be in {:?}", err, p.errors);
            assert!(b, err);
        }

    }

    #[test]
    fn test_parse_let() {
        let mut p = make_parser(
            "
let x = 5;
let y = 10;
let foobar = 838383;
let = 83833;
let y 83353;
let 8331254;
",
        );
        let program = p.parse();
        assert_eq!(program.statements.len(), 6);


        let tests = vec![test_case("x"), test_case("y"), test_case("foobar")];

        for (idx, t) in tests.iter().enumerate() {
            let stmt = &program.statements[idx];
            test_let_statement(stmt, t);
        }
        test_error(
            &p,
            ParseError {
                expected: token::IDENT,
                actual: token::ASSIGN,
                pos: 50,
            },
        );
        test_error(
            &p,
            ParseError {
                expected: token::ASSIGN,
                actual: token::INT,
                pos: 69,
            },
        );
        test_error(
            &p,
            ParseError {
                expected: token::IDENT,
                actual: token::INT,
                pos: 82,
            },
        );
    }

    #[test]
    fn test_parse_return() {
        let mut p = make_parser(
            "
return 5;
return 10;
return 993322;
",
        );
        let program = p.parse();
        assert_eq!(program.statements.len(), 3);


        for stmt in program.statements.iter() {
            match *stmt {
                ast::Statement::Return(_) => (),
                _ => assert!(false, "Expected Return statement"),
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let mut p = make_parser(
            "
foobar;
",
        );
        let program = p.parse();
        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        use ast::Node;
        match *stmt {
            ast::Statement::Expression(ref exp) => {
                match &exp.value {
                    &ast::Expression::Identifier(ref ident) => {
                        assert_eq!(ident.value, "foobar");
                        assert_eq!(ident.token_literal(), "foobar");
                    }
                    _ => assert!(false, "Expected identifier expression"),
                }
            }
            _ => assert!(false, "Expected expression statement"),
        }
    }

    fn test_statement_expression<F>(stmt: &ast::Statement, f: F)
    where
        F: Fn(&ast::Expression),
    {
        match stmt {
            &ast::Statement::Expression(ref exp) => f(&exp.value),
            _ => assert!(false, "Expected expression statement"),
        }
    }

    fn test_block_statement<F>(stmt: &ast::Statement, f: F)
        where
        F: Fn(&Vec<ast::Statement>),
    {
        match stmt {
            &ast::Statement::Block(ref exp) => f(&exp.statements),
            _ => assert!(false, "Expected block statement"),
        }
    }


    fn test_integer_literal(exp: &ast::Expression, value: i64) {
        match exp {
            &ast::Expression::Integer(ref int_lit) => {
                assert_eq!(int_lit.value, value);
            }
            _ => assert!(false, "Expected integer literal"),
        }
    }

    fn test_infix_expression<F>(exp: &ast::Expression, f: F)
    where
        F: Fn(&ast::InfixExpression),
    {
        match exp {
            &ast::Expression::Infix(ref infix) => f(infix),
            _ => assert!(false, "Expected infix expression"),
        }
    }

    fn test_if_expression<F>(exp: &ast::Expression, f: F)
    where
        F: Fn(&ast::IfExpression),
    {
        match exp {
            &ast::Expression::If(ref ifexp) => f(ifexp),
            _ => assert!(false, "Expected if expression"),
        }
    }

    fn test_prefix_expression<F>(exp: &ast::Expression, f: F)
    where
        F: Fn(&ast::PrefixExpression),
    {
        match exp {
            &ast::Expression::Prefix(ref prefix) => f(&*prefix),
            _ => assert!(false, "Expected prefix expression"),
        }
    }

    fn test_identifier_literal(exp: &ast::Expression, value: &str) {
        match exp {
            &ast::Expression::Identifier(ref ident) => {
                assert_eq!(ident.value.as_str(), value);
            }
            _ => assert!(false, "Expected identifier expression"),
        }
    }

    #[test]
    fn test_integer_expression() {
        use ast::Node;
        let mut p = make_parser(
            "
5;
",
        );
        let program = p.parse();
        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];

        test_statement_expression(stmt, |exp| test_integer_literal(&exp, 5));
    }

    #[test]
    fn test_prefix_expressions() {
        let tests = vec![("!5;", "!", 5), ("-15;", "-", 15)];

        for t in tests {
            let mut p = make_parser(t.0);
            let program = p.parse();
            assert_eq!(1, program.statements.len());

            test_statement_expression(&program.statements[0], |exp| {
                test_prefix_expression(exp, |prefix| {
                    assert_eq!(prefix.operator, t.1);
                    test_integer_literal(&prefix.right, t.2)
                })
            });
        }

    }

    #[test]
    fn test_infix_expressions() {
        let tests = vec![
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for t in tests {
            let mut p = make_parser(t.0);
            let program = p.parse();
            assert_eq!(1, program.statements.len());

            test_statement_expression(&program.statements[0], |exp| {
                test_infix_expression(exp, |infix| {
                    test_integer_literal(&infix.left, t.1);
                    assert_eq!(infix.operator, String::from(t.2));
                    test_integer_literal(&infix.right, t.3);
                })
            });
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b -c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
        ];

        for t in tests {
            let mut p = make_parser(t.0);
            let program = p.parse();

            test_no_errors(&p);
            assert_eq!(t.1, format!("{}", program));
        }
    }

    #[test]
    fn test_if_expressions() {
        let input = "if (x < y) { x }";
        let mut parser = make_parser(input);
        let program = parser.parse();
        test_no_errors(&parser);

        test_statement_expression(&program.statements[0], |exp| {
            test_if_expression(exp, |ifexp| {
                test_infix_expression(&ifexp.condition, |infix| {
                    test_identifier_literal(&infix.left, "x");
                    assert_eq!(infix.operator, "<");
                    test_identifier_literal(&infix.right, "y");
                });


                test_block_statement(&*ifexp.consequence, |stmts| {
                    test_statement_expression(&stmts[0], |stmt| {
                        test_identifier_literal(stmt, "x");
                    });
                });
            })
        })
    }

    #[test]
    fn test_if_else_expressions() {
        let input = "if (x < y) { x } else { y }";
        let mut parser = make_parser(input);
        let program = parser.parse();
        test_no_errors(&parser);

        test_statement_expression(&program.statements[0], |exp| {
            test_if_expression(exp, |ifexp| {
                test_infix_expression(&ifexp.condition, |infix| {
                    test_identifier_literal(&infix.left, "x");
                    assert_eq!(infix.operator, "<");
                    test_identifier_literal(&infix.right, "y");
                });


                test_block_statement(&*ifexp.consequence, |stmts| {
                    test_statement_expression(&stmts[0], |stmt| {
                        test_identifier_literal(stmt, "x");
                    });
                });

                let alt = ifexp.alternative.as_ref().unwrap();
                    test_block_statement(&alt, |stmts| {
                        test_statement_expression(&stmts[0], |stmt| {
                            test_identifier_literal(stmt, "y");
                        });
                    });

            })
        })
    }

}
