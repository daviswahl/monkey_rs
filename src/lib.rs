#![cfg_attr(feature="clippy", feature(plugin))]

#![cfg_attr(feature="clippy", plugin(clippy))]

mod token;
mod lexer;
mod ast;
mod parser;
mod object;
mod evaluator;
pub mod environment;
use object::Object;
use std::rc::Rc;

pub fn eval(s: &str, env: &mut environment::Environment) -> Result<Rc<Object>, String> {
    let p = parse(s);
    evaluator::eval(&p, env)
}

pub fn parse(s: &str) -> ast::Node {
    parser::parse(s)
}

mod tests {
    #[test]
    fn it_works() {}
}
