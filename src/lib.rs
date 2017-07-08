#![cfg_attr(feature="clippy", feature(plugin))]

#![cfg_attr(feature="clippy", plugin(clippy))]

mod token;
mod lexer;
mod ast;
mod parser;
mod object;
mod evaluator;

pub fn eval(s: &str) {
    let p = parse(s);
    evaluator::eval(&p);
}
pub fn parse(s: &str) -> ast::Node {
    parser::parse(s)
}

mod tests {
    #[test]
    fn it_works() {}
}
