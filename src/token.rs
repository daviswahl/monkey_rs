
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
    Token{typ: typ, literal: String::from(lit)}
}
pub struct Token {
    pub typ: TokenType,
    pub literal: String,
}
