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

    fn read_string(&mut self) -> String {
        self.read_char();
        let pos = self.pos;
        while self.ch != b'"' {
            self.read_char();
        }
        let s = self.input[pos..self.pos].to_string();
        self.read_char();
        s

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
        use token::Token::*;
        self.skip_whitespace();
        let token = match self.ch {

            b'=' if self.next_eq(b'=') => {
                self.read_char();
                EQ
            },

            b'=' => ASSIGN,

            b'+' => PLUS,
            b'-' => MINUS,
            b'/' => SLASH,
            b'*' => ASTERISK,

            b'!' if self.next_eq(b'=') => {
                self.read_char();
                NOT_EQ
            },
            b'!' => BANG,

            b'<' => LT,
            b'>' => GT,
            b'(' => LPAREN,
            b')' => RPAREN,
            b'{' => LBRACE,
            b'}' => RBRACE,
            b',' => COMMA,
            b';' => SEMICOLON,
            b'\0' => EOF,

            b'"' => return STRING(self.read_string()),
            x if is_letter(x) => return token::assign_ident(self.read_identifier()),

            x if is_digit(x) => {
                return INT(self.read_digit());
            }
            _ => ILLEGAL,
        };
        self.read_char();
        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tok(t: token::Token, s: &str) -> (token::Token, String)  {
        (t, String::from(s))
    }

    #[test]
    fn lexing_1() {
        let input = "=+(){},;";

        let mut l = Lexer::new(input);

        let tests = vec![
            tok(token::Token::ASSIGN, "="),
            tok(::token::Token::PLUS, "+"),
            tok(::token::Token::LPAREN, "("),
            tok(::token::Token::RPAREN, ")"),
            tok(::token::Token::LBRACE, "{"),
            tok(::token::Token::RBRACE, "}"),
            tok(::token::Token::COMMA, ","),
            tok(::token::Token::SEMICOLON, ";"),
            tok(::token::Token::EOF, "EOF"),
        ];

        for t in tests {
            let tok = l.next_token();
            assert!(token::cmp(&tok, &t.0));
            assert_eq!(tok.literal(), t.1);
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

\"foo\";
\"foo-bar\";";
        let mut l = Lexer::new(input);

        let tests = vec![
            tok(token::Token::LET, "LET"),
            tok(token::Token::IDENT("five".to_string()), "five"),
            tok(token::Token::ASSIGN, "="),
            tok(token::Token::INT("5".to_string()), "5"),
            tok(token::Token::SEMICOLON, ";"),
            tok(token::Token::LET, "LET"),
            tok(token::Token::IDENT("ten".to_string()), "ten"),
            tok(token::Token::ASSIGN, "="),
            tok(token::Token::INT("10".to_string()), "10"),
            tok(token::Token::SEMICOLON, ";"),
            tok(token::Token::LET, "LET"),
            tok(token::Token::IDENT("add".to_string()), "add"),
            tok(token::Token::ASSIGN, "="),
            tok(token::Token::FUNCTION, "FUNCTION"),
            tok(token::Token::LPAREN, "("),
            tok(token::Token::IDENT("x".to_string()), "x"),
            tok(token::Token::COMMA, ","),
            tok(token::Token::IDENT("y".to_string()), "y"),
            tok(token::Token::RPAREN, ")"),
            tok(token::Token::LBRACE, "{"),
            tok(token::Token::IDENT("x".to_string()), "x"),
            tok(token::Token::PLUS, "+"),
            tok(token::Token::IDENT("y".to_string()), "y"),
            tok(token::Token::SEMICOLON, ";"),
            tok(token::Token::RBRACE, "}"),
            tok(token::Token::SEMICOLON, ";"),
            tok(token::Token::LET, "LET"),
            tok(token::Token::IDENT("result".to_string()), "result"),
            tok(token::Token::ASSIGN, "="),
            tok(token::Token::IDENT("add".to_string()), "add"),
            tok(token::Token::LPAREN, "("),
            tok(token::Token::IDENT("five".to_string()), "five"),
            tok(token::Token::COMMA, ","),
            tok(token::Token::IDENT("ten".to_string()), "ten"),
            tok(token::Token::RPAREN, ")"),
            tok(token::Token::SEMICOLON, ";"),

            tok(token::Token::BANG, "!"),
            tok(token::Token::MINUS, "-"),
            tok(token::Token::SLASH, "/"),
            tok(token::Token::ASTERISK, "*"),
            tok(token::Token::INT("5".to_string()), "5"),
            tok(token::Token::SEMICOLON, ";"),

            tok(token::Token::INT("5".to_string()), "5"),
            tok(token::Token::LT, "<"),
            tok(token::Token::INT("10".to_string()), "10"),
            tok(token::Token::GT, ">"),
            tok(token::Token::INT("5".to_string()), "5"),
            tok(token::Token::SEMICOLON, ";"),

            tok(token::Token::IF, "IF"),
            tok(token::Token::LPAREN, "("),
            tok(token::Token::INT("5".to_string()), "5"),
            tok(token::Token::LT, "<"),
            tok(token::Token::INT("10".to_string()), "10"),
            tok(token::Token::RPAREN, ")"),
            tok(token::Token::LBRACE, "{"),
            tok(token::Token::RETURN, "RETURN"),
            tok(token::Token::TRUE, "TRUE"),
            tok(token::Token::SEMICOLON, ";"),
            tok(token::Token::RBRACE, "}"),
            tok(token::Token::ELSE, "ELSE"),
            tok(token::Token::LBRACE, "{"),
            tok(token::Token::RETURN, "RETURN"),
            tok(token::Token::FALSE, "FALSE"),
            tok(token::Token::SEMICOLON, ";"),
            tok(token::Token::RBRACE, "}"),

            tok(token::Token::INT("10".to_string()), "10"),
            tok(token::Token::EQ, "=="),
            tok(token::Token::INT("10".to_string()), "10"),
            tok(token::Token::SEMICOLON, ";"),

            tok(token::Token::INT("10".to_string()), "10"),
            tok(token::Token::NOT_EQ, "!="),
            tok(token::Token::INT("9".to_string()), "9"),
            tok(token::Token::SEMICOLON, ";"),

            tok(token::Token::STRING("foo".to_string()), "foo"),
            tok(token::Token::SEMICOLON, ";"),
            tok(token::Token::STRING("foo-bar".to_string()), "foo-bar"),
            tok(token::Token::SEMICOLON, ";"),
            tok(token::Token::EOF, "EOF"),
        ];

        for t in tests {
            let tok = l.next_token();
            println!("compare: {:?} : {:?}", &tok, &t.0);
            assert!(token::cmp(&tok, &t.0));
            assert_eq!(tok.literal(), t.1);
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
