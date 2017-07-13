extern crate monkey_parser;

use std::rc::Rc;
use std::cell::RefCell;
use monkey_parser::environment::Environment;
use monkey_parser::object;

fn main() {
    repl()
}

fn repl() {
    use std::io;

    let mut input = String::new();
    let mut env = Environment::new();

    while let Ok(_) = io::stdin().read_line(&mut input) {
        match monkey_parser::eval(input.as_str(), env.clone()) {
            Ok(result) => {
                match *result {
                    object::Object::Null => (),
                    _ => println!("{}", result),
                }
            }
            Err(e) => println!("Error: {}", e),
        }
        input.clear();
    }
}
