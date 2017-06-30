#![cfg_attr(feature="clippy", feature(plugin))]

#![cfg_attr(feature="clippy", plugin(clippy))]

mod token;
mod lexer;
mod ast;
mod parser;

pub fn parse(s: String) -> ast::Program {
    let l = lexer::Lexer::new(s);
    let mut p = parser::Parser::new(l);
    p.parse()
}

mod tests {
    #[test]
    fn it_works() {}
}
