use builtin::is_builtin;

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    ILLEGAL,
    EOF,
    IDENT(String),
    STRING(String),
    INT(String),

    BUILTIN(String),

    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    CONCAT,

    LT,
    GT,

    COMMA,
    SEMICOLON,
    QUOTE,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,

    EQ,
    NOT_EQ,
}

impl Token {
    pub fn literal(&self) -> String {
        let s = match self {
            &Token::ILLEGAL => "ILLEGAL",
            &Token::EOF => "EOF",
            &Token::IDENT(ref i) => return i.clone(),

            &Token::INT(ref i) => return i.clone(),
            &Token::STRING(ref i) => return i.clone(),
            &Token::BUILTIN(ref i) => return i.clone(),

            &Token::ASSIGN => "=",
            &Token::PLUS => "+",
            &Token::MINUS => "-",
            &Token::BANG => "!",
            &Token::ASTERISK => "*",
            &Token::SLASH => "/",
            &Token::CONCAT => "++",

            &Token::LT => "<",
            &Token::GT => ">",

            &Token::COMMA => ",",
            &Token::SEMICOLON => ";",
            &Token::QUOTE => "\"",

            &Token::LPAREN => "(",
            &Token::RPAREN => ")",
            &Token::LBRACE => "{",
            &Token::RBRACE => "}",
            &Token::FUNCTION => "FUNCTION",
            &Token::LET => "LET",
            &Token::TRUE => "TRUE",
            &Token::FALSE => "FALSE",
            &Token::IF => "IF",
            &Token::ELSE => "ELSE",
            &Token::RETURN => "RETURN",


            &Token::EQ => "==",
            &Token::NOT_EQ => "!=",
        };
        String::from(s)
    }
}

const KEYWORDS: [(&'static str, Token); 7] = [
    ("fn", Token::FUNCTION),
    ("let", Token::LET),
    ("true", Token::TRUE),
    ("false", Token::FALSE),
    ("return", Token::RETURN),
    ("if", Token::IF),
    ("else", Token::ELSE),
];

pub fn assign_ident(s: String) -> Token {
    match KEYWORDS.iter().find(|x| x.0 == s.as_str()).map(|s| s.1.clone()) {
        Some(s) => s.clone(),
        None if is_builtin(&s) => Token::BUILTIN(s),
        None => Token::IDENT(s),
    }
}

pub fn cmp(l: &Token, r: &Token) -> bool {
    use self::Token::*;
    match (l, r) {
        (&IDENT(_), &IDENT(_)) => true,
        (&INT(_), &INT(_)) => true,
        (&STRING(_), &STRING(_)) => true,
        _ => l == r,
    }
}
