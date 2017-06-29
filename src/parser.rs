use lexer;
use token;
use ast;

struct Parser {
    lexer: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
}
fn new(mut l: lexer::Lexer) -> Parser {
    let cur = l.next_token();
    let peek = l.next_token();
    Parser{lexer: l, cur_token: cur, peek_token: peek}
}

impl Parser {
    fn next_token(&mut self) {
        let cur = self.peek_token.clone();
        let peek = self.lexer.next_token();

        self.peek_token = peek;
        self.cur_token = cur;
    }

    fn parse<'a>(&'a mut self) -> ast::Program {
        ast::Program{statements: vec![]}
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    fn script() -> String {
        String::from("
let x = 5;
let y = 10;
let foobar = 838383;
")
    }

    fn makeLexer() -> lexer::Lexer {
        lexer::new(script())
    }

    #[test]
    fn test_make_parser() {
        let mut p = new(makeLexer());
        assert_eq!(p.cur_token, token::Token{typ: token::LET, literal: String::from("let")});
        assert_eq!(p.peek_token, token::Token{typ: token::IDENT, literal: String::from("x")});

        p.parse();
    }
}
