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
    match parse(s) {
        Ok(p) => evaluator::eval(&p, env),
        Err(errs) => Err(format!("{:?}", errs)),
    }
}

pub fn parse(s: &str) -> Result<ast::Node, Vec<parser::ParseError>> {
    parser::parse(s)
}

mod tests {
    #[test]
    fn it_works() {}
}
