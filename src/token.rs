
pub type TokenType = &'static str;


pub const ILLEGAL: TokenType = "ILLEGAL";
pub const EOF: TokenType = "EOF";
pub const IDENT: TokenType = "IDENT";

pub const INT: TokenType = "INT";
pub const ASSIGN: TokenType = "=";
pub const PLUS: TokenType = "+";
pub const COMMA: TokenType = ",";
pub const SEMICOLON: TokenType = ";";
pub const LPAREN: TokenType = "(";

pub const RPAREN: TokenType = ")";
pub const LBRACE: TokenType = "{";
pub const RBRACE: TokenType = "}";

pub const FUNCTION: TokenType = "FUNCTION";
pub const LET: TokenType = "LET";

pub fn make(typ: TokenType, lit: &'static str) -> Token {
    Token {
        typ: typ,
        literal: String::from(lit),
    }
}

#[derive(Debug)]
pub struct Token {
    pub typ: TokenType,
    pub literal: String,
}


const KEYWORDS: [(&'static str, TokenType); 2] = [("fn", FUNCTION), ("let", LET)];

pub fn lookup_ident(s: &String) -> TokenType {
    match KEYWORDS.iter().find(|x| x.0 == s.as_str()) {
        Some(s) => s.1,
        None => IDENT
    }
}
