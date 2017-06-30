use lexer;
use token;
use ast;
use ast::Node;
use ast::Expression;
use ast::Statement;

struct Parser {
    lexer: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
}

fn new(mut l: lexer::Lexer) -> Parser {
    let cur = l.next_token();
    let peek = l.next_token();
    Parser {
        lexer: l,
        cur_token: cur,
        peek_token: peek,
    }
}

impl Parser {
    fn next_token(&mut self) {
        let cur = self.peek_token.clone();
        let peek = self.lexer.next_token();

        self.peek_token = peek;
        self.cur_token = cur;
    }


    fn parse_let_statement(&mut self) -> Option<Box<ast::Statement>> {
        Some(Box::new(ast::LetStatement {
            token: token::make(token::LET, "let"),
            name: ast::Identifier {
                token: token::make(token::IDENT, "foo"),
                value: String::from("foo"),
            },
            value: Box::new(ast::Unit {}),
        }))
    }

    fn parse_statement(&mut self) -> Option<Box<ast::Statement>> {
        match self.cur_token.typ {
            token::LET => self.parse_let_statement(),
            _ => None,
        }
    }

    fn parse<'a>(&'a mut self) -> ast::Program {
        let mut statements: Vec<Box<ast::Statement>> = vec![];


        while self.cur_token.typ != token::EOF {
            let stmt: Option<Box<ast::Statement>> = self.parse_statement();
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
    fn script() -> String {
        String::from(
            "
let x = 5;
let y = 10;
let foobar = 838383;
",
        )
    }

    fn makeLexer() -> lexer::Lexer {
        lexer::new(script())
    }

    struct test_case(&'static str);

    fn test_let_statement(s: &Box<ast::Statement>, t: &test_case) {
        assert_eq!(s.token_literal(), "let");
    }

    #[test]
    fn test_make_parser() {
        let mut p = new(makeLexer());
        assert_eq!(
            p.cur_token,
            token::Token {
                typ: token::LET,
                literal: String::from("let"),
            }
        );
        assert_eq!(
            p.peek_token,
            token::Token {
                typ: token::IDENT,
                literal: String::from("x"),
            }
        );

        let program = p.parse();
        assert_eq!(program.statements.len(), 3);


        let tests = vec![test_case("x"), test_case("y"), test_case("foobar")];

        for (idx, t) in tests.iter().enumerate() {
            let stmt = &program.statements[idx];
            test_let_statement(stmt, t);
        }
    }
}
