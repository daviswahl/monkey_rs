#![cfg_attr(feature="clippy", feature(plugin))]

#![cfg_attr(feature="clippy", plugin(clippy))]

mod token;
mod lexer;
mod ast;
mod parser;
mod object;
mod evaluator;
mod environment;

pub fn eval(s: &str) {
    let p = parse(s);
    let mut env = environment::Environment::new();
    evaluator::eval(&p, &mut env);
}
pub fn parse(s: &str) -> ast::Node {
    parser::parse(s)
}

mod tests {
    #[test]
    fn it_works() {}
}
