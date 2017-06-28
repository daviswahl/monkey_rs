
use token;

struct Lexer {
    input: Vec<u8>,
    pos: usize,
    read_pos: usize,
    ch: u8,
    len: usize,
}

fn is_letter(ch: char) -> bool {
    match ch {
        x if 'a' <= x && x <= 'z' => true,
        x if 'A' <= x && x <= 'Z' => true,
        '_' => true,
        _ => false,
    }
}

fn is_whitespace(ch: char) -> bool {
    match ch {
        ' ' => true,
        '\t' => true,
        '\n' => true,
        '\r' => true,
        _ => false
    }
}

impl Lexer {
    fn new(s: String) -> Lexer {
        let input = s.into_bytes();
        let len: usize = input.len();
        let mut l = Lexer {
            input: input,
            pos: 0,
            read_pos: 0,
            ch: 0,
            len: len,
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_pos >= self.len {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_pos];
        }

        self.pos = self.read_pos;
        self.read_pos = self.read_pos + 1;
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.pos;

        while is_letter(self.ch as char) {
            self.read_char();
        }
        String::from_utf8(self.input[pos..self.pos].to_vec()).unwrap()
    }

    fn skip_whitespace(&mut self) {
        while is_whitespace(self.ch as char) {
            self.read_char();
        }
    }

    fn next_token(&mut self) -> token::Token {
        self.skip_whitespace();
        let token = match self.ch as char {
            '=' => token::make(token::ASSIGN, "="),
            '+' => token::make(token::PLUS, "+"),
            '(' => token::make(token::LPAREN, "("),
            ')' => token::make(token::RPAREN, ")"),
            '{' => token::make(token::LBRACE, "{"),
            '}' => token::make(token::RBRACE, "}"),
            ',' => token::make(token::COMMA, ","),
            ';' => token::make(token::SEMICOLON, ";"),
            '\0' => token::make(token::EOF, ""),
            x if is_letter(x) => {
                token::Token {
                    typ: token::IDENT,
                    literal: self.read_identifier(),
                }
            },
            _ => token::make(token::ILLEGAL, "ILLEGAL")
        };
        self.read_char();
        token
    }
}

#[cfg(test)]
mod tests {
    use lexer::*;

    struct test(::token::TokenType, String);
    fn tok(t: token::TokenType, s: &'static str) -> test {
        test(t, String::from(s))
    }

    #[test]
    fn lexing_1() {
        let input = String::from("=+(){},;");

        let mut l = Lexer::new(input);


        let tests = vec![
            tok(token::ASSIGN, "="),
            tok(::token::PLUS, "+"),
            tok(::token::LPAREN, "("),
            tok(::token::RPAREN, ")"),
            tok(::token::LBRACE, "{"),
            tok(::token::RBRACE, "}"),
            tok(::token::COMMA, ","),
            tok(::token::SEMICOLON, ";"),
            tok(::token::EOF, ""),
        ];

        for t in tests {
            let tok = l.next_token();
            assert_eq!(tok.typ, t.0);
            assert!(tok.literal == t.1);
        }
    }

    #[test]
    fn lexing_2() {
        let input = String::from(
            "
let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, 10);
",
        );


        let mut l = Lexer::new(input);

        let tests = vec![
            tok(token::LET, "let"),
            tok(token::IDENT, "five"),
            tok(token::ASSIGN, "="),
            tok(token::LET, "let"),
            tok(token::IDENT, "five"),
            tok(token::ASSIGN, "="),
            tok(token::INT, "5"),
            tok(token::SEMICOLON, ";"),
            tok(token::LET, "let"),
            tok(token::IDENT, "ten"),
            tok(token::ASSIGN, "="),
            tok(token::INT, "10"),
            tok(token::SEMICOLON, ";"),
            tok(token::LET, "let"),
            tok(token::IDENT, "add"),
            tok(token::ASSIGN, "="),
            tok(token::FUNCTION, "fn"),
            tok(token::LPAREN, "("),
            tok(token::IDENT, "x"),
            tok(token::COMMA, ","),
            tok(token::IDENT, "y"),
            tok(token::RPAREN, ")"),
            tok(token::LBRACE, "{"),
            tok(token::IDENT, "x"),
            tok(token::PLUS, "+"),
            tok(token::IDENT, "y"),
            tok(token::SEMICOLON, ";"),
            tok(token::RBRACE, "}"),
            tok(token::SEMICOLON, ";"),
            tok(token::LET, "let"),
            tok(token::IDENT, "result"),
            tok(token::ASSIGN, "="),
            tok(token::IDENT, "add"),
            tok(token::LPAREN, "("),
            tok(token::IDENT, "five"),
            tok(token::COMMA, ","),
            tok(token::IDENT, "ten"),
            tok(token::RPAREN, ")"),
            tok(token::SEMICOLON, ";"),
            tok(token::EOF, ""),
        ];

        for t in tests {
            let tok = l.next_token();
            assert_eq!(tok.typ, t.0);
            assert!(tok.literal == t.1);
        }
    }

    #[test]
    fn test_is_letter() {
        assert!(is_letter('z'));
        assert!(is_letter('A'));
        assert!(!is_letter('1'));
        assert!(is_letter('_'));
        assert!(!is_letter(' '));
        assert!(!is_letter('{'));
    }

}
