#![cfg_attr(feature="clippy", feature(plugin))]

#![cfg_attr(feature="clippy", plugin(clippy))]

pub mod repl;
pub mod environment;
mod evaluator;
mod parser;

mod token;
mod lexer;
mod ast;
mod builtin;
mod runtime;
mod object;

use object::Object;

use std::rc::Rc;
use std::cell::RefCell;

pub fn eval(s: &str, env: Rc<RefCell<environment::Environment>>) -> Result<Object, String> {
    let evaluator = evaluator::Evaluator { runtime: runtime::new() };

    match parse(s) {
        Ok(p) => {
            evaluator::eval(p, env.clone(), &evaluator).map(|o| o.unwrap_value(file!(), line!()))
        }
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
