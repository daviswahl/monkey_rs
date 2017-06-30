use lexer;
use token;
use ast;
use ast::Node;
use ast::Expression;
use ast::Statement;
use std::collections::HashMap;

struct Parser {
    lexer: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
    errors: Vec<ParseError>,

    prefix_parse_fns: HashMap<token::TokenType, Box<PrefixParseFn>>,
    infix_parse_fns: HashMap<token::TokenType, Box<InfixParseFn>>,
}

#[derive(PartialEq, Debug)]
struct ParseError {
    expected: token::TokenType,
    actual: token::TokenType,
    pos: usize,
}

enum Precedence {
    LOWEST = 0,
    EQUALS = 1,
    LESSGREATER = 2,
    SUM = 3,
    PRODUCT = 4,
    PREFIX = 5,
    CALL = 6,
}

type PrefixParseFn = Fn(&Parser) -> ast::Expression;
type InfixParseFn = Fn(&Parser, ast::Expression) -> ast::Expression;

impl Parser {
    fn new(mut l: lexer::Lexer) -> Parser {
        let cur = l.next_token();
        let peek = l.next_token();
        let mut p = Parser {
            lexer: l,
            cur_token: cur,
            peek_token: peek,
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        p.register_prefix(token::IDENT, Box::new(|pa| Parser::parse_identifier(pa)));
        p.register_prefix(token::INT, Box::new(|pa| Parser::parse_int(pa)));
        p
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

    fn register_prefix(&mut self, t: token::TokenType, f: Box<PrefixParseFn>) {
        self.prefix_parse_fns.insert(t, f);
    }

    fn parse_expression(&mut self, p: Precedence) -> Option<ast::Expression> {
        self.prefix_parse_fns.get(self.cur_token.typ).map(
            |f| f(&self),
        )
    }

    fn parse_int(p: &Parser) -> ast::Expression {
        let tok = p.cur_token.clone();
        ast::Expression::Integer(ast::IntegerLiteral {
            value: tok.literal.parse().unwrap(),
            token: tok,
        })
    }

    fn parse_identifier(p: &Parser) -> ast::Expression {
        let tok = p.cur_token.clone();
        ast::Expression::Identifier(ast::IdentifierExpression {
            value: tok.literal.clone(),
            token: tok,
        })
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
        let exp = self.parse_expression(Precedence::LOWEST);

        if !self.expect_peek(token::SEMICOLON) {
            return None;
        }

        exp.map(|ex| {
            ast::Statement::Expression(ast::ExpressionStatement {
                token: self.next_token(),
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

    fn parse<'b>(&'b mut self) -> ast::Program {
        let mut statements: Vec<ast::Statement> = vec![];


        while self.cur_token.typ != token::EOF {
            let stmt: Option<ast::Statement> = self.parse_statement();
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
        assert_eq!(s.token_literal(), "let");
        match *s {
            Statement::Let(ref stmt) => {
                assert_eq!(stmt.name.value, t.0);
            }
            _ => assert!(false),
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


        for (idx, stmt) in program.statements.iter().enumerate() {
            match *stmt {
                ast::Statement::Return(ref stmt) => (),
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

    #[test]
    fn test_integer_expression() {
        let mut p = make_parser(
            "
5;
",
        );
        let program = p.parse();
        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        match *stmt {
            ast::Statement::Expression(ref exp) => {
                match &exp.value {
                    &ast::Expression::Integer(ref int_lit) => {
                        assert_eq!(int_lit.value, 5);
                        assert_eq!(int_lit.token_literal(), "5");
                    }
                    _ => assert!(false, "Expected integer literal"),
                }
            }
            _ => assert!(false, "Expected expression statement"),
        }
    }

}
