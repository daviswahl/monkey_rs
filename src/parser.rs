use lexer;
use token;
use ast;

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
    cur_token: token::Token,
    peek_token: token::Token,
    errors: Vec<ParseError>,
}

#[derive(PartialEq, Debug)]
struct ParseError {
    expected: token::Token,
    actual: token::Token,
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
    pub fn from_token(t: &token::Token) -> Precedence {
        use token::Token::*;
        match t {
            &EQ | &NOT_EQ => Precedence::EQUALS,
            &LT | &GT => Precedence::LESSGREATER,
            &PLUS | &MINUS => Precedence::SUM,
            &SLASH | &ASTERISK => Precedence::PRODUCT,
            &LPAREN => Precedence::CALL,
            _ => Precedence::LOWEST,
        }
    }
}

pub fn parse(s: &str) -> ast::Node {
    let lexer = lexer::Lexer::new(s);
    Parser::new(lexer).parse()
}


impl<'a> Parser<'a> {
    pub fn new(mut l: lexer::Lexer<'a>) -> Parser {
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




    fn cur_token_is(&self, t: &token::Token) -> bool {
        token::compare_tokens(&self.cur_token, t)
    }

    fn peek_token_is(&self, t: &token::Token) -> bool {
        token::compare_tokens(&self.peek_token, t)
    }

    fn expect_peek(&mut self, t: &token::Token) -> bool {
        if self.peek_token_is(t) {
            true
        } else {
            self.errors.push(ParseError {
                expected: t.clone(),
                actual: self.peek_token.clone(),
                pos: self.lexer.pos,
            });
            false
        }
    }


    fn peek_precedence(&self) -> Precedence {
        Precedence::from_token(&self.peek_token)
    }

    fn cur_precedence(&self) -> Precedence {
        Precedence::from_token(&self.cur_token)
    }


    fn parse_expression(&mut self, p: Precedence) -> Option<ast::Expression> {
        use token::Token::*;
        let mut left = match self.cur_token {
            IDENT(_) => self.parse_identifier(),
            INT(_) => self.parse_int(),
            BANG => self.parse_prefix_expression(),
            MINUS => self.parse_prefix_expression(),
            TRUE | FALSE => self.parse_boolean(),
            LPAREN => self.parse_grouped_expression(),
            IF => self.parse_if_expression(),
            FUNCTION => self.parse_fn_literal(),
            _ => None,
        };

        if left.is_none() {
            return None;
        }

        while !self.peek_token_is(&SEMICOLON) && p < self.peek_precedence() {
            left = left.and_then(|exp| match self.peek_token {
                PLUS | MINUS | SLASH | ASTERISK | EQ |
                NOT_EQ | LT | GT => {
                    self.next_token();
                    self.parse_infix_expression(exp)
                }
                LPAREN => {
                    self.next_token();
                    self.parse_call_expression(exp)
                }
                _ => Some(exp),
            });
        }
        left
    }

    fn parse_call_expression(&mut self, func: ast::Expression) -> Option<ast::Expression> {
        Some(ast::Expression::Call(ast::CallExpression {
            token: self.next_token(),
            function: Box::new(func),
            arguments: self.parse_call_arguments()
                .into_iter()
                .map(Box::new)
                .collect(),
        }))
    }

    fn parse_call_arguments(&mut self) -> Vec<ast::Expression> {
        use token::Token::*;
        if self.peek_token_is(&RPAREN) && self.cur_token_is(&LPAREN) {
            self.next_token();
            return vec![];
        }
        let mut args = vec![];

        self.parse_expression(Precedence::LOWEST).map(|exp| {
            args.push(exp)
        });

        while self.peek_token_is(&COMMA) {
            self.next_token();
            self.next_token();
            self.parse_expression(Precedence::LOWEST).map(|exp| {
                args.push(exp)
            });
        }

        if !self.expect_peek(&RPAREN) {
            return vec![];
        }
        self.next_token();
        args
    }

    fn parse_fn_literal(&mut self) -> Option<ast::Expression> {
        if !self.expect_peek(&token::Token::LPAREN) {
            return None;
        }
        let tok = self.next_token();

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(&token::Token::LBRACE) {
            return None;
        }
        self.next_token();

        self.parse_block_statement().map(|body| {
            ast::Expression::Function(ast::FunctionLiteral {
                token: tok,
                parameters: parameters.into_iter().map(Box::new).collect(),
                body: Box::new(body),
            })
        })
    }

    fn parse_function_parameters(&mut self) -> Vec<ast::Expression> {
        if self.peek_token_is(&token::Token::RPAREN) {
            self.next_token();
            return vec![];
        }

        self.next_token();
        let mut params = vec![];

        self.parse_identifier().map(|exp| params.push(exp));

        while self.peek_token_is(&token::Token::COMMA) {
            self.next_token();
            self.next_token();
            self.parse_identifier().map(|exp| params.push(exp));
        }

        if !self.expect_peek(&token::Token::RPAREN) {
            return vec![];
        }
        self.next_token();

        params
    }

    fn parse_if_expression(&mut self) -> Option<ast::Expression> {
        if !self.expect_peek(&token::Token::LPAREN) {
            return None;
        }

        let tok = self.next_token();

        self.next_token();

        let cond = self.parse_expression(Precedence::LOWEST);

        if !self.expect_peek(&token::Token::RPAREN) {
            return None;
        }
        self.next_token();

        if !self.expect_peek(&token::Token::LBRACE) {
            return None;
        }

        cond.and_then(|cond_exp| {
            self.parse_block_statement().map(|block| {

                let alt = if self.peek_token_is(&token::Token::ELSE) {
                    self.next_token();

                    if !self.expect_peek(&token::Token::LBRACE) {
                        None
                    } else {
                        self.next_token();
                        self.parse_block_statement()
                    }
                } else {
                    None
                };

                ast::Expression::If(ast::IfExpression {
                    token: tok,
                    condition: Box::new(cond_exp),
                    consequence: Box::new(block),
                    alternative: alt.map(Box::new),
                })
            })
        })
    }

    fn parse_block_statement(&mut self) -> Option<ast::Statement> {
        let tok = self.next_token();
        let mut statements: Vec<ast::Node> = vec![];

        while !self.cur_token_is(&token::Token::RBRACE) {
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

        if !self.expect_peek(&token::Token::RPAREN) {
            return None;
        }
        self.next_token();
        exp
    }

    fn parse_int(&self) -> Option<ast::Expression> {
        let tok = self.cur_token.clone();
        Some(ast::Expression::Integer(ast::IntegerLiteral {
            value: tok.literal().parse().unwrap(),
            token: tok,
        }))
    }

    fn parse_boolean(&self) -> Option<ast::Expression> {
        let tok = self.cur_token.clone();
        Some(ast::Expression::Boolean(ast::BooleanExpression {
            token: tok,
            value: self.cur_token_is(&token::Token::TRUE),
        }))
    }

    fn parse_identifier(&self) -> Option<ast::Expression> {
        let tok = self.cur_token.clone();
        Some(ast::Expression::Identifier(ast::IdentifierExpression {
            value: tok.literal(),
            token: tok,
        }))
    }

    fn parse_return_statement(&mut self) -> Option<ast::Node> {
        let tok = self.next_token();


        let stmt = self.parse_expression(Precedence::LOWEST).map(|exp| {
            ast::Node::Statement(ast::Statement::Return(ast::ReturnStatement {
                token: tok,
                value: Box::new(exp),
            }))
        });

        if self.peek_token_is(&token::Token::SEMICOLON) {
            self.next_token();
        }
        stmt
    }

    fn parse_prefix_expression(&mut self) -> Option<ast::Expression> {
        let tok = self.next_token();

        self.parse_expression(Precedence::PREFIX).map(|right| {
            ast::Expression::Prefix(ast::PrefixExpression {
                operator: tok.literal(),
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
                operator: tok.literal(),
                token: tok,
                left: Box::new(left),
                right: Box::new(right),
            })
        })
    }

    fn parse_let_statement(&mut self) -> Option<ast::Node> {

        if !self.expect_peek(&token::Token::IDENT("".to_string())) {
            return None;
        }
        let let_tok = self.next_token();

        if !self.expect_peek(&token::Token::ASSIGN) {
            return None;
        }

        let ident_tok = self.next_token();
        let name = ast::IdentifierExpression::new(ident_tok);

        self.next_token();
        let exp = self.parse_expression(Precedence::LOWEST).map(|exp| {
            ast::Node::Statement(ast::Statement::Let(ast::LetStatement {
                token: let_tok,
                name: name,
                value: Box::new(exp),
            }))
        });

        if self.peek_token_is(&token::Token::SEMICOLON) {
            self.next_token();
        }

        exp
    }

    fn parse_expression_statement(&mut self) -> Option<ast::Node> {
        let tok = self.cur_token.clone();
        let exp = self.parse_expression(Precedence::LOWEST);


        if self.peek_token_is(&token::Token::SEMICOLON) {
            self.next_token();
        }

        exp.map(|ex| {
            ast::Node::Statement(ast::Statement::Expression(ast::ExpressionStatement {
                token: tok,
                value: ex,
            }))
        })

    }

    fn parse_statement(&mut self) -> Option<ast::Node> {
        use token::Token::*;
        match self.cur_token {
            LET => self.parse_let_statement(),
            RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse(&mut self) -> ast::Node {
        let mut statements: Vec<ast::Node> = vec![];


        while self.cur_token != token::Token::EOF {
            if let Some(s) = self.parse_statement() {
                statements.push(s);
            }
            self.next_token();
        }
        ast::Node::Program(ast::Program {
            statements: statements
        })
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    use token::Token::*;
    fn make_parser(s: &str) -> Parser {
        Parser::new(make_lexer(s))
    }

    fn make_lexer(s: &str) -> lexer::Lexer {
        lexer::Lexer::new(s)
    }

    fn assert_statement<F>(n: &ast::Node, f: F)
    where
        F: Fn(&ast::Statement),
    {
        match *n {
            ast::Node::Statement(ref stmt) => f(stmt),
            _ => assert!(false, "Expected statement"),
        }

    }

    fn assert_let<F>(s: &ast::Node, t: &str, f: F)
    where
        F: Fn(&ast::LetStatement),
    {
        use ast::HasToken;
        assert_statement(s, |stmt| {
            assert_eq!(stmt.token_literal(), "LET");
            match *stmt {
                ast::Statement::Let(ref l) => {
                    assert_eq!(l.name.value, t);
                    f(l)
                }
                _ => assert!(false, "Expected let statement"),
            }
        })
    }

    fn assert_return<F>(s: &ast::Node, f: F)
    where
        F: Fn(&ast::ReturnStatement),
    {
        use ast::HasToken;
        assert_statement(s, |stmt| {
            assert_eq!(stmt.token_literal(), "RETURN");
            match *stmt {
                ast::Statement::Return(ref ret) => f(ret),
                _ => assert!(false, "Expected return statement"),
            }
        })
    }

    fn test_no_errors(p: &Parser) {
        if p.errors.len() > 0 {
            println!("Expected no errors, got {:?}", p.errors);
            assert!(false)
        }
    }
    fn assert_error(p: &Parser, err: ParseError) {
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

        assert_program(&p.parse(), |program| {
            assert_eq!(program.statements.len(), 6);

            let tests = vec![("x"), ("y"), ("foobar")];

            for (idx, t) in tests.iter().enumerate() {
                let stmt = &program.statements[idx];
                assert_let(stmt, t, |_| {});
            }
            assert_error(
                &p,
                ParseError {
                    expected: IDENT("".to_string()),
                    actual: ASSIGN,
                    pos: 50,
                },
            );
            assert_error(
                &p,
                ParseError {
                    expected: ASSIGN,
                    actual: INT("83353".to_string()),
                    pos: 69,
                },
            );
            assert_error(
                &p,
                ParseError {
                    expected: IDENT("".to_string()),
                    actual: INT("8331254".to_string()),
                    pos: 82,
                },
            );
        })
    }

    #[test]
    fn test_parse_return() {
        let tests = vec![
            ("return 5'", "5"),
            ("return 10;", "10"),
            ("return 993322;", "993322"),
            ("return 4 + 5;", "(4 + 5)"),
        ];

        for test in tests {
            assert_program(&make_parser(test.0).parse(), |program| {
                assert_eq!(program.statements.len(), 1);
                assert_return(&program.statements[0], |stmt| {
                    assert_eq!(stmt.value.to_string(), test.1);
                })
            })
        }
    }

    #[test]
    fn test_identifier_expression() {
        let mut p = make_parser(
            "
foobar;
",
        );
        assert_program(&p.parse(), |program| {
            assert_eq!(program.statements.len(), 1);

            assert_statement(&program.statements[0], |stmt| {

                use ast::HasToken;
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
            })
        })
    }

    fn assert_statement_expression<F>(node: &ast::Node, f: F)
    where
        F: Fn(&ast::Expression),
    {
        assert_statement(node, |stmt| match stmt {
            &ast::Statement::Expression(ref exp) => f(&exp.value),
            _ => assert!(false, "Expected expression statement"),
        })
    }

    fn assert_block<F>(stmt: &ast::Statement, f: F)
    where
        F: Fn(&Vec<ast::Node>),
    {
        match stmt {
            &ast::Statement::Block(ref exp) => f(&exp.statements),
            _ => assert!(false, "Expected block statement"),
        }
    }


    fn assert_integer(exp: &ast::Expression, value: i64) {
        match exp {
            &ast::Expression::Integer(ref int_lit) => {
                assert_eq!(int_lit.value, value);
            }
            _ => assert!(false, "Expected integer literal"),
        }
    }

    fn assert_program<F>(node: &ast::Node, f: F)
    where
        F: Fn(&ast::Program),
    {
        match node {
            &ast::Node::Program(ref p) => f(p),
            x => assert!(false, format!("Expected program node, got {:?}", x)),
        }
    }

    fn assert_infix<F>(exp: &ast::Expression, f: F)
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

    fn assert_fn_expression<F>(exp: &ast::Expression, f: F)
    where
        F: Fn(&ast::FunctionLiteral),
    {
        match exp {
            &ast::Expression::Function(ref func) => f(func),
            _ => assert!(false, "Expected function literal"),
        }
    }

    fn assert_call<F>(exp: &ast::Expression, f: F)
    where
        F: Fn(&ast::CallExpression),
    {
        match exp {
            &ast::Expression::Call(ref call) => f(call),
            _ => assert!(false, "Expected call expression"),
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

    fn assert_ident(exp: &ast::Expression, value: &str) {
        match exp {
            &ast::Expression::Identifier(ref ident) => {
                assert_eq!(ident.value.as_str(), value);
            }
            _ => assert!(false, "Expected identifier expression"),
        }
    }

    #[test]
    fn test_integer_expression() {
        let mut p = make_parser(
            "
5;
",
        );
        assert_program(&p.parse(), |program| {
            assert_eq!(program.statements.len(), 1);
            assert_statement_expression(&program.statements[0], |exp| { assert_integer(&exp, 5); })
        })
    }

    #[test]
    fn test_prefix_expressions() {
        let tests = vec![("!5;", "!", 5), ("-15;", "-", 15)];

        for t in tests {
            let mut p = make_parser(t.0);
            assert_program(&p.parse(), |program| {
                assert_eq!(1, program.statements.len());

                assert_statement_expression(&program.statements[0], |exp| {
                    test_prefix_expression(exp, |prefix| {
                        assert_eq!(prefix.operator, t.1);
                        assert_integer(&prefix.right, t.2)
                    })
                });
            })
        }

    }

    #[test]
    fn assert_infixs() {
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
            assert_program(&p.parse(), |program| {
                assert_eq!(1, program.statements.len());

                assert_statement_expression(&program.statements[0], |exp| {
                    assert_infix(exp, |infix| {
                        assert_integer(&infix.left, t.1);
                        assert_eq!(infix.operator, String::from(t.2));
                        assert_integer(&infix.right, t.3);
                    })
                });
            })
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
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"
            ),
        ];

        for t in tests {
            let mut p = make_parser(t.0);
            assert_program(&p.parse(), |program| {

                test_no_errors(&p);
                assert_eq!(t.1, format!("{}", program));
            })
        }
    }

    #[test]
    fn test_if_expressions() {
        let input = "if (x < y) { x }";
        let mut parser = make_parser(input);
        let prog = parser.parse();
        test_no_errors(&parser);
        assert_program(&prog, |program| {


            assert_statement_expression(&program.statements[0], |exp| {
                test_if_expression(exp, |ifexp| {
                    assert_infix(&ifexp.condition, |infix| {
                        assert_ident(&infix.left, "x");
                        assert_eq!(infix.operator, "<");
                        assert_ident(&infix.right, "y");
                    });


                    assert_block(&*ifexp.consequence, |stmts| {
                        assert_statement_expression(&stmts[0], |stmt| { assert_ident(stmt, "x"); });
                    });
                })
            })
        })
    }

    #[test]
    fn test_if_else_expressions() {
        let input = "if (x < y) { x } else { y }";
        let mut parser = make_parser(input);
        let prog = parser.parse();
        test_no_errors(&parser);
        assert_program(&prog, |program| {
            assert_statement_expression(&program.statements[0], |exp| {
                test_if_expression(exp, |ifexp| {
                    assert_infix(&ifexp.condition, |infix| {
                        assert_ident(&infix.left, "x");
                        assert_eq!(infix.operator, "<");
                        assert_ident(&infix.right, "y");
                    });


                    assert_block(&*ifexp.consequence, |stmts| {
                        assert_statement_expression(&stmts[0], |stmt| { assert_ident(stmt, "x"); });
                    });

                    let alt = ifexp.alternative.as_ref().unwrap();
                    assert_block(&alt, |stmts| {
                        assert_statement_expression(&stmts[0], |stmt| { assert_ident(stmt, "y"); });
                    });

                })
            })
        })
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { x  + y }";
        let mut parser = make_parser(input);
        let prog = parser.parse();
        test_no_errors(&parser);
        assert_program(&prog, |program| {
            assert_statement_expression(&program.statements[0], |exp| {
                assert_fn_expression(exp, |fnexp| {
                    assert_ident(&fnexp.parameters[0], "x");
                    assert_ident(&fnexp.parameters[1], "y");

                    assert_block(&*fnexp.body, |stmts| {
                        assert_statement_expression(&stmts[0], |stmt| {
                            assert_infix(&stmt, |infix| {
                                assert_ident(&infix.left, "x");
                                assert_eq!(infix.operator, "+");
                                assert_ident(&infix.right, "y");
                            })
                        });
                    });
                })
            })
        })
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let mut parser = make_parser(input);
        let prog = parser.parse();

        test_no_errors(&parser);

        assert_program(&prog, |program| {
            assert_statement_expression(&program.statements[0], |exp| {
                assert_call(exp, |callexp| {
                    assert_integer(&callexp.arguments[0], 1);
                    assert_infix(&callexp.arguments[1], |infix| {
                        assert_integer(&*infix.left, 2);
                        assert_eq!(infix.operator, "*");
                        assert_integer(&*infix.right, 3);
                    });
                    assert_infix(&callexp.arguments[2], |infix| {
                        assert_integer(&*infix.left, 4);
                        assert_eq!(infix.operator, "+");
                        assert_integer(&*infix.right, 5);
                    });

                })
            })
        })
    }
    #[test]
    fn test_call_expression_2() {
        let input = "add(1);";
        let mut parser = make_parser(input);
        let prog = parser.parse();

        test_no_errors(&parser);

        assert_program(&prog, |program| {
            assert_statement_expression(&program.statements[0], |exp| {
                assert_call(exp, |callexp| {
                    assert_integer(&callexp.arguments[0], 1);
                })
            })
        })
    }

    #[test]
    fn assert_lets() {
        let tests = vec![
            ("let x = 5;", "x", "5"),
            ("let y = true;", "y", "true"),
            ("let foobar = y", "foobar", "y"),
        ];

        for test in tests {
            let prog = make_parser(test.0).parse();

            assert_program(&prog, |program| {
                assert_let(&program.statements[0], test.1, |let_stmt| {
                    assert_eq!(let_stmt.value.to_string(), test.2);
                })
            })
        }
    }
}
