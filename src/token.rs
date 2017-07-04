
pub type TokenType = &'static str;


pub const ILLEGAL: TokenType = "ILLEGAL";
pub const EOF: TokenType = "EOF";
pub const IDENT: TokenType = "IDENT";

pub const INT: TokenType = "INT";

// Operators
pub const ASSIGN: TokenType = "=";
pub const PLUS: TokenType = "+";
pub const MINUS: TokenType = "-";
pub const BANG: TokenType = "!";
pub const ASTERISK: TokenType = "*";
pub const SLASH: TokenType = "/";

pub const LT: TokenType = "<";
pub const GT: TokenType = ">";

pub const COMMA: TokenType = ",";
pub const SEMICOLON: TokenType = ";";

// Grouping
pub const LPAREN: TokenType = "(";
pub const RPAREN: TokenType = ")";
pub const LBRACE: TokenType = "{";
pub const RBRACE: TokenType = "}";

// Keywords
pub const FUNCTION: TokenType = "FUNCTION";
pub const LET: TokenType = "LET";
pub const TRUE: TokenType = "TRUE";
pub const FALSE: TokenType = "FALSE";
pub const IF: TokenType = "IF";
pub const ELSE: TokenType = "ELSE";
pub const RETURN: TokenType = "RETURN";

// Boolean
pub const EQ: TokenType = "==";
pub const NOT_EQ: TokenType = "!=";

pub fn make(typ: TokenType, lit: &'static str) -> Token {
    Token {
        typ: typ,
        literal: String::from(lit),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub typ: TokenType,
    pub literal: String,
}


const KEYWORDS: [(&'static str, TokenType); 7] = [
    ("fn", FUNCTION),
    ("let", LET),
    ("true", TRUE),
    ("false", FALSE),
    ("return", RETURN),
    ("if", IF),
    ("else", ELSE),
];

pub fn lookup_ident(s: &str) -> TokenType {
    match KEYWORDS.iter().find(|x| x.0 == s) {
        Some(s) => s.1,
        None => IDENT
    }
}
