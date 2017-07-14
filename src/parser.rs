use lexer;
use token;
use token::Token;
use ast;

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<ParseError>,
}

#[derive(PartialEq, Debug)]
pub struct ParseError {
    expected: Token,
    actual: Token,
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
    INDEX = 7,
}

impl Precedence {
    pub fn from_token(t: &Token) -> Precedence {
        match t {
            &Token::ASSIGN | &Token::EQ | &Token::NOT_EQ => Precedence::EQUALS,
            &Token::LT | &Token::GT => Precedence::LESSGREATER,
            &Token::PLUS | &Token::MINUS | &Token::CONCAT => Precedence::SUM,
            &Token::SLASH | &Token::ASTERISK => Precedence::PRODUCT,
            &Token::LPAREN => Precedence::CALL,
            &Token::LBRACKET => Precedence::INDEX,
            _ => Precedence::LOWEST,
        }
    }
}

pub fn parse(s: &str) -> Result<ast::Node, Vec<ParseError>> {
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

    fn next_token(&mut self) -> Token {
        use std::mem;
        let peek = self.lexer.next_token();

        let cur = mem::replace(&mut self.peek_token, peek);
        mem::replace(&mut self.cur_token, cur)
    }

    fn cur_token_is(&self, t: &Token) -> bool {
        token::cmp(&self.cur_token, t)
    }

    fn peek_token_is(&self, t: &Token) -> bool {
        token::cmp(&self.peek_token, t)
    }

    fn expect_peek(&mut self, t: &Token) -> bool {
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
            STRING(_) => self.parse_string(),
            BANG => self.parse_prefix_expression(),
            MINUS => self.parse_prefix_expression(),
            TRUE | FALSE => self.parse_boolean(),
            LPAREN => self.parse_grouped_expression(),
            LBRACKET => self.parse_array(),
            IF => self.parse_if_expression(),
            FUNCTION => self.parse_fn_literal(),
            BUILTIN(_) => self.parse_builtin(),
            _ => None,
        };

        if left.is_none() {
            return None;
        }

        while !self.peek_token_is(&SEMICOLON) && p < self.peek_precedence() {
            left = left.and_then(|exp| match self.peek_token {
                ASSIGN | PLUS | MINUS | SLASH | ASTERISK | EQ | NOT_EQ | LT | GT | CONCAT => {
                    self.next_token();
                    self.parse_infix_expression(exp)
                }
                LPAREN => {
                    self.next_token();
                    self.parse_call_expression(exp)
                }

                LBRACKET => {
                    self.next_token();
                    self.parse_index_expression(exp)
                }

                _ => Some(exp),
            });
        }
        left
    }

    fn parse_index_expression(&mut self, exp: ast::Expression) -> Option<ast::Expression> {
        let tok = self.next_token();
        let exp = self.parse_expression(Precedence::LOWEST).map(|index| {
            ast::Expression::Index(ast::IndexExpression {
                token: tok,
                left: Box::new(exp),
                index: Box::new(index),
            })
        });

        if !self.expect_peek(&Token::RBRACKET) {
            self.next_token();
            return None;
        }
        self.next_token();

        exp
    }

    fn parse_array(&mut self) -> Option<ast::Expression> {
        Some(ast::Expression::Array(ast::ArrayLiteral {
            token: self.next_token(),
            elements: self.parse_array_elements(),
        }))
    }

    fn parse_array_elements(&mut self) -> Vec<ast::Expression> {
        if self.cur_token_is(&Token::LBRACKET) && self.peek_token_is(&Token::RBRACKET) {
            return vec![];
        }

        let mut args: Vec<ast::Expression> = vec![];

        self.parse_expression(Precedence::LOWEST).map(|exp| {
            args.push(exp)
        });

        while self.peek_token_is(&Token::COMMA) {
            self.next_token();
            self.next_token();
            self.parse_expression(Precedence::LOWEST).map(|exp| {
                args.push(exp)
            });
        }

        if !self.expect_peek(&Token::RBRACKET) {
            return vec![];
        }
        self.next_token();
        args
    }

    fn parse_builtin(&mut self) -> Option<ast::Expression> {
        let tok = self.cur_token.clone();
        Some(ast::Expression::Builtin(tok))
    }

    fn parse_call_expression(&mut self, func: ast::Expression) -> Option<ast::Expression> {
        Some(ast::Expression::Call(ast::CallExpression {
            token: self.next_token(),
            function: Box::new(func),
            arguments: self.parse_call_arguments(),
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
        if !self.expect_peek(&Token::LPAREN) {
            return None;
        }
        let tok = self.next_token();

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(&Token::LBRACE) {
            return None;
        }
        self.next_token();

        self.parse_block_statement().map(|body| {
            ast::Expression::Function(ast::FunctionLiteral {
                token: tok,
                parameters: parameters,
                body: Box::new(body),
            })
        })
    }

    fn parse_function_parameters(&mut self) -> Vec<ast::Expression> {
        if self.peek_token_is(&Token::RPAREN) {
            self.next_token();
            return vec![];
        }

        self.next_token();
        let mut params = vec![];

        self.parse_identifier().map(|exp| params.push(exp));

        while self.peek_token_is(&Token::COMMA) {
            self.next_token();
            self.next_token();
            self.parse_identifier().map(|exp| params.push(exp));
        }

        if !self.expect_peek(&Token::RPAREN) {
            return vec![];
        }
        self.next_token();

        params
    }

    fn parse_if_expression(&mut self) -> Option<ast::Expression> {
        if !self.expect_peek(&Token::LPAREN) {
            return None;
        }

        let tok = self.next_token();

        self.next_token();

        let cond = self.parse_expression(Precedence::LOWEST);

        if !self.expect_peek(&Token::RPAREN) {
            return None;
        }
        self.next_token();

        if !self.expect_peek(&Token::LBRACE) {
            return None;
        }

        cond.and_then(|cond_exp| {
            self.parse_block_statement().map(|block| {

                let alt = if self.peek_token_is(&Token::ELSE) {
                    self.next_token();

                    if !self.expect_peek(&Token::LBRACE) {
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

        while !self.cur_token_is(&Token::RBRACE) {
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

        if !self.expect_peek(&Token::RPAREN) {
            return None;
        }
        self.next_token();
        exp
    }

    fn parse_string(&self) -> Option<ast::Expression> {
        let tok = self.cur_token.clone();
        Some(ast::Expression::String(ast::StringLiteral { token: tok }))
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
            value: self.cur_token_is(&Token::TRUE),
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

        if self.peek_token_is(&Token::SEMICOLON) {
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

        if !self.expect_peek(&Token::IDENT("".to_string())) {
            return None;
        }
        let let_tok = self.next_token();

        if !self.expect_peek(&Token::ASSIGN) {
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

        if self.peek_token_is(&Token::SEMICOLON) {
            self.next_token();
        }

        exp
    }

    fn parse_expression_statement(&mut self) -> Option<ast::Node> {
        let tok = self.cur_token.clone();
        let exp = self.parse_expression(Precedence::LOWEST);


        if self.peek_token_is(&Token::SEMICOLON) {
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
        match self.cur_token {
            Token::LET => self.parse_let_statement(),
            Token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse(mut self) -> Result<ast::Node, Vec<ParseError>> {
        let mut statements: Vec<ast::Node> = vec![];

        if !self.errors.is_empty() {
            return Err(self.errors);
        }
        while self.cur_token != Token::EOF {
            if let Some(s) = self.parse_statement() {
                statements.push(s);
            }
            self.next_token();
        }
        Ok(ast::Node::Program(ast::Program { statements: statements }))
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use token::Token::*;

    #[test]
    fn test_update_expression() {
        let p = make_parser("i = i + 1");
        let prog = assert_no_errors(p.parse());
        assert_program(&prog, |program| {
            assert_statement_expression(&program.statements[0], |stmt| {
                assert_infix(&stmt, |infix1| {
                    assert_ident(&*infix1.left, "i");
                    assert_infix(&*infix1.right, |infix2| {
                        assert_ident(&*infix2.left, "i");
                        assert_integer(&*infix2.right, 1);
                        assert_eq!(infix2.operator.as_str(), "+");
                    })
                })
            })
        })
    }

    #[test]
    fn test_parse_index_expressions() {
        let p = make_parser("myArray[1 + 1];");
        let prog = assert_no_errors(p.parse());
        assert_program(&prog, |program| {
            assert_statement_expression(&program.statements[0], |stmt| {
                assert_index(&stmt, |index| {
                    assert_ident(&*index.left, "myArray");
                    assert_infix(&*index.index, |infix| {
                        assert_integer(&*infix.left, 1);
                        assert_integer(&*infix.right, 1);
                        assert_eq!(infix.operator.as_str(), "+");
                    })
                })
            })
        })

    }

    #[test]
    fn test_parse_array_literals() {
        let p = make_parser("[1, 2 * 2, 3 + 3]");

        let prog = assert_no_errors(p.parse());
        assert_program(&prog, |program| {
            assert_statement_expression(&program.statements[0], |stmt| {
                assert_array(stmt, |array| {
                    assert_integer(&array.elements[0], 1);
                    assert_infix(&array.elements[1], |infix| {
                        assert_integer(&infix.left, 2);
                        assert_eq!(infix.operator.as_str(), "*");
                        assert_integer(&infix.right, 2);
                    });
                    assert_infix(&array.elements[2], |infix| {
                        assert_integer(&infix.left, 3);
                        assert_eq!(infix.operator.as_str(), "+");
                        assert_integer(&infix.right, 3);
                    });
                })
            })
        })
    }


    #[test]
    fn test_parse_builtin() {
        let p = make_parser("len(4,5);");
        let program = assert_no_errors(p.parse());
        assert_program(&program, |prog| {
            assert_statement_expression(&prog.statements[0], |stmt| {
                assert_call(&stmt, |call| {
                    assert_integer(&call.arguments[0], 4);
                    assert_integer(&call.arguments[1], 5);
                    assert_builtin(&*call.function, "len")
                })
            })

        })
    }

    #[test]
    fn test_parse_let() {
        let p = make_parser(
            "
let x = 5;
let y = 10;
let foobar = 838383;
let batz = \"batz\";
let = 83833;
let y 83353;
let 8331254;
",
        );
        let result = p.parse();
        assert_program(result.as_ref().unwrap(), |program| {
            assert_eq!(program.statements.len(), 7);

            let tests = vec![("x"), ("y"), ("foobar"), ("batz")];

            for (idx, t) in tests.iter().enumerate() {
                let stmt = &program.statements[idx];
                assert_let(stmt, t, |_| {});
            }
            assert_error(
                &result,
                ParseError {
                    expected: IDENT("".to_string()),
                    actual: ASSIGN,
                    pos: 69,
                },
            );
            assert_error(
                &result,
                ParseError {
                    expected: ASSIGN,
                    actual: INT("83353".to_string()),
                    pos: 88,
                },
            );
            assert_error(
                &result,
                ParseError {
                    expected: IDENT("".to_string()),
                    actual: INT("8331254".to_string()),
                    pos: 101,
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
            ("return \"foobar\"", "foobar"),
        ];

        for test in tests {
            assert_program(&make_parser(test.0).parse().unwrap(), |program| {
                assert_eq!(program.statements.len(), 1);
                assert_return(&program.statements[0], |stmt| {
                    assert_eq!(stmt.value.to_string(), test.1);
                })
            })
        }
    }

    #[test]
    fn test_parse_string() {

        let p = make_parser("\"foobar\"");
        assert_program(&p.parse().unwrap(), |program| {
            assert_statement_expression(&program.statements[0], |exp| {
                assert_string(exp, "foobar");
            })

        })
    }

    #[test]
    fn test_identifier_expression() {
        let p = make_parser(
            "
foobar;
",
        );
        assert_program(&p.parse().unwrap(), |program| {
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


    #[test]
    fn test_integer_expression() {
        let p = make_parser(
            "
5;
",
        );
        assert_program(&p.parse().unwrap(), |program| {
            assert_eq!(program.statements.len(), 1);
            assert_statement_expression(&program.statements[0], |exp| { assert_integer(&exp, 5); })
        })
    }

    #[test]
    fn test_prefix_expressions() {
        let tests = vec![("!5;", "!", 5), ("-15;", "-", 15)];

        for t in tests {
            let p = make_parser(t.0);
            assert_program(&p.parse().unwrap(), |program| {
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
            let p = make_parser(t.0);
            assert_program(&p.parse().unwrap(), |program| {
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
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)"
            ),
            (
                "add(a * b[2], b[1], 2 * [1,2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))"
            ),
        ];

        for t in tests {
            let prog = assert_no_errors(make_parser(t.0).parse());
            assert_program(&prog, |program| {

                assert_eq!(t.1, format!("{}", program));
            })
        }
    }

    #[test]
    fn assert_if_expressions() {
        let input = "if (x < y) { x }";
        let parser = make_parser(input);
        let prog = assert_no_errors(parser.parse());
        assert_program(&prog, |program| {


            assert_statement_expression(&program.statements[0], |exp| {
                assert_if_expression(exp, |ifexp| {
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
        let parser = make_parser(input);
        let prog = assert_no_errors(parser.parse());
        assert_program(&prog, |program| {
            assert_statement_expression(&program.statements[0], |exp| {
                assert_if_expression(exp, |ifexp| {
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
        let parser = make_parser(input);
        let prog = assert_no_errors(parser.parse());
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
        let parser = make_parser(input);
        let prog = assert_no_errors(parser.parse());

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
        let parser = make_parser(input);
        let prog = assert_no_errors(parser.parse());

        assert_program(&prog, |program| {
            assert_statement_expression(&program.statements[0], |exp| {
                assert_call(exp, |callexp| { assert_integer(&callexp.arguments[0], 1); })
            })
        })
    }

    #[test]
    fn test_lets() {
        let tests = vec![
            ("let x = 5;", "x", "5"),
            ("let y = true;", "y", "true"),
            ("let foobar = y", "foobar", "y"),
        ];

        for test in tests {
            let prog = make_parser(test.0).parse();

            assert_program(&prog.unwrap(), |program| {
                assert_let(&program.statements[0], test.1, |let_stmt| {
                    assert_eq!(let_stmt.value.to_string(), test.2);
                })
            })
        }
    }
    fn make_parser(s: &str) -> Parser {
        Parser::new(make_lexer(s))
    }

    fn make_lexer(s: &str) -> lexer::Lexer {
        lexer::Lexer::new(s)
    }

    fn assert_index<F>(n: &ast::Expression, f: F)
    where
        F: Fn(&ast::IndexExpression),
    {
        match *n {
            ast::Expression::Index(ref index) => f(index),
            ref x => assert!(false, "expected index expression, got: {}", x),
        }
    }

    fn assert_builtin(n: &ast::Expression, s: &str) {
        match *n {
            ast::Expression::Builtin(ref builtin) => assert_eq!(builtin.literal().as_str(), s),
            _ => assert!(false, "Expected builtin"),
        }
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

    fn assert_no_errors(p: Result<ast::Node, Vec<ParseError>>) -> ast::Node {
        assert!(p.is_ok());
        p.unwrap()
    }

    fn assert_error(p: &Result<ast::Node, Vec<ParseError>>, err: ParseError) {
        if let &Err(ref errors) = p {
            let b = errors.iter().any(|e| *e == err);
            if !b {
                let err = format!("Expected {:?} to be in {:?}", err, errors);
                assert!(b, err);
            }
        }

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

    fn assert_string(exp: &ast::Expression, value: &str) {
        match exp {
            &ast::Expression::String(ref string) => {
                assert_eq!(string.token.literal(), value.to_string());
            }
            _ => assert!(false, "Expected string literal"),
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
            x => assert!(false, "Expected infix expression, got {:?}", x),
        }
    }

    fn assert_if_expression<F>(exp: &ast::Expression, f: F)
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
            x => assert!(false, format!("Expected call expression, got: {}", x)),
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

    fn assert_array<F>(exp: &ast::Expression, f: F)
    where
        F: Fn(&ast::ArrayLiteral),
    {
        match exp {
            &ast::Expression::Array(ref array) => f(array),
            exp => assert!(false, format!("Expected array expression, got: {:?}", exp)),
        }
    }

}
