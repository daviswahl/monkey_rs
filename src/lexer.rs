
use token;

struct Lexer {
    input: Vec<u8>,
    pos: usize,
    read_pos: usize,
    ch: u8,
    len: usize,
}

fn is_digit(ch: char) -> bool {
    match ch {
        x if '0' <= x && x <= '9' => true,
        _ => false,
    }
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
        _ => false,
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


    fn peek(&self) -> u8 {
        if self.read_pos >= self.len {
            0
        } else {
            self.input[self.read_pos]
        }
    }

    fn next_eq(&self, next: char) -> bool {
        self.peek() == next as u8
    }

    fn read_char(&mut self) {
        if self.read_pos >= self.len {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_pos];
            self.pos = self.read_pos;
            self.read_pos = self.read_pos + 1;
        }
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.pos;

        while is_letter(self.ch as char) {
            self.read_char();
        }
        String::from_utf8(self.input[pos..self.pos].to_vec()).unwrap()
    }

    fn read_digit(&mut self) -> String {
        let pos = self.pos;

        while is_digit(self.ch as char) {
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

            '=' if self.next_eq('=') => {
                self.read_char();
                token::make(token::EQ, "==")
            },

            '=' => token::make(token::ASSIGN, "="),

            '+' => token::make(token::PLUS, "+"),
            '-' => token::make(token::MINUS, "-"),
            '/' => token::make(token::SLASH, "/"),
            '*' => token::make(token::ASTERISK, "*"),

            '!' if self.next_eq('=') => {
                self.read_char();
                token::make(token::NOT_EQ, "!=")
            },
            '!' => token::make(token::BANG, "!"),

            '<' => token::make(token::LT, "<"),
            '>' => token::make(token::GT, ">"),
            '(' => token::make(token::LPAREN, "("),
            ')' => token::make(token::RPAREN, ")"),
            '{' => token::make(token::LBRACE, "{"),
            '}' => token::make(token::RBRACE, "}"),
            ',' => token::make(token::COMMA, ","),
            ';' => token::make(token::SEMICOLON, ";"),
            '\0' => token::make(token::EOF, ""),

            x if is_letter(x) => {
                let literal = self.read_identifier();
                let tkn = token::lookup_ident(&literal);
                return token::Token{typ: tkn, literal: literal}
            }

            x if is_digit(x) => {
                let digit = self.read_digit();
                return token::Token{typ: token::INT, literal: digit}
            }
            _ => token::make(token::ILLEGAL, "ILLEGAL"),
        };
        self.read_char();
        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
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

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
  return true;
} else {
  return false;
}

10 == 10;
10 != 9;
",
        );


        let mut l = Lexer::new(input);

        let tests = vec![
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

            tok(token::BANG, "!"),
            tok(token::MINUS, "-"),
            tok(token::SLASH, "/"),
            tok(token::ASTERISK, "*"),
            tok(token::INT, "5"),
            tok(token::SEMICOLON, ";"),

            tok(token::INT, "5"),
            tok(token::LT, "<"),
            tok(token::INT, "10"),
            tok(token::GT, ">"),
            tok(token::INT, "5"),
            tok(token::SEMICOLON, ";"),

            tok(token::IF, "if"),
            tok(token::LPAREN, "("),
            tok(token::INT, "5"),
            tok(token::LT, "<"),
            tok(token::INT, "10"),
            tok(token::RPAREN, ")"),
            tok(token::LBRACE, "{"),
            tok(token::RETURN, "return"),
            tok(token::TRUE, "true"),
            tok(token::SEMICOLON, ";"),
            tok(token::RBRACE, "}"),
            tok(token::ELSE, "else"),
            tok(token::LBRACE, "{"),
            tok(token::RETURN, "return"),
            tok(token::FALSE, "false"),
            tok(token::SEMICOLON, ";"),
            tok(token::RBRACE, "}"),

            tok(token::INT, "10"),
            tok(token::EQ, "=="),
            tok(token::INT, "10"),
            tok(token::SEMICOLON, ";"),

            tok(token::INT, "10"),
            tok(token::NOT_EQ, "!="),
            tok(token::INT, "9"),
            tok(token::SEMICOLON, ";"),
            tok(token::EOF, ""),
        ];

        for t in tests {
            let tok = l.next_token();
            println!("testing: {:?}", tok);
            assert_eq!(tok.typ, t.0);
            assert_eq!(tok.literal, t.1);
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
