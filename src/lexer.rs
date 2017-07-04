use token;

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    pub pos: usize,
    read_pos: usize,
    ch: u8,
    len: usize,
}

fn is_digit(ch: u8) -> bool {
    match ch {
        x if b'0' <= x && x <= b'9' => true,
        _ => false,
    }
}

fn is_letter(ch: u8) -> bool {
    match ch {
        x if b'a' <= x && x <= b'z' => true,
        x if b'A' <= x && x <= b'Z' => true,
        b'_' => true,
        _ => false,
    }
}

fn is_whitespace(ch: u8) -> bool {
    match ch {
        b' ' | b'\t' | b'\n' | b'\r' => true,
        _ => false,
    }
}


impl <'a>Lexer<'a> {
    pub fn new(s:  &'a str) -> Lexer {
        let len: usize = s.len();
        let mut l = Lexer {
            input: s,
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
           b'\0'
        } else {
            self.input.as_bytes()[self.read_pos]
        }
    }

    fn next_eq(&self, next: u8) -> bool {
        self.peek() == next
    }

    fn read_char(&mut self) {
        if self.read_pos >= self.len {
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_pos];
        }
        self.pos = self.read_pos;
        self.read_pos += 1;
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.pos;

        while is_letter(self.ch) {
            self.read_char();
        }
        self.input[pos..self.pos].to_string()
    }

    fn read_digit(&mut self) -> String {
        let pos = self.pos;

        while is_digit(self.ch) {
            self.read_char();
        }
        self.input[pos..self.pos].to_string()
    }

    fn skip_whitespace(&mut self) {
        while is_whitespace(self.ch) {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> token::Token {
        self.skip_whitespace();
        let token = match self.ch {

            b'=' if self.next_eq(b'=') => {
                self.read_char();
                token::make(token::EQ, "==")
            },

            b'=' => token::make(token::ASSIGN, "="),

            b'+' => token::make(token::PLUS, "+"),
            b'-' => token::make(token::MINUS, "-"),
            b'/' => token::make(token::SLASH, "/"),
            b'*' => token::make(token::ASTERISK, "*"),

            b'!' if self.next_eq(b'=') => {
                self.read_char();
                token::make(token::NOT_EQ, "!=")
            },
            b'!' => token::make(token::BANG, "!"),

            b'<' => token::make(token::LT, "<"),
            b'>' => token::make(token::GT, ">"),
            b'(' => token::make(token::LPAREN, "("),
            b')' => token::make(token::RPAREN, ")"),
            b'{' => token::make(token::LBRACE, "{"),
            b'}' => token::make(token::RBRACE, "}"),
            b',' => token::make(token::COMMA, ","),
            b';' => token::make(token::SEMICOLON, ";"),
            b'\0' => token::make(token::EOF, ""),

            x if is_letter(x) => {
                let literal = self.read_identifier();
                let tkn = token::lookup_ident(&literal);
                return token::Token{typ: tkn, literal: literal}
            }

            x if is_digit(x) => {
                return token::Token{typ: token::INT, literal: self.read_digit()}
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

    fn tok(t: token::TokenType, s: &str) -> (::token::TokenType, String)  {
        (t, String::from(s))
    }

    #[test]
    fn lexing_1() {
        let input = "=+(){},;";

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
        let input = "
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
";



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
            assert_eq!(tok.typ, t.0);
            assert_eq!(tok.literal, t.1);
        }
    }

    #[test]
    fn test_is_letter() {
        assert!(is_letter(b'z'));
        assert!(is_letter(b'A'));
        assert!(!is_letter(b'1'));
        assert!(is_letter(b'_'));
        assert!(!is_letter(b' '));
        assert!(!is_letter(b'{'));
    }
}
