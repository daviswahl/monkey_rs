extern crate monkey_parser;

use monkey_parser::environment::Environment;

fn main() {
    repl()
}

fn repl() {
    use std::io;

    let mut input = String::new();
    let mut env = Environment::new();

    while let Ok(_) = io::stdin().read_line(&mut input) {
        match monkey_parser::eval(input.as_str(), &mut env) {
            Ok(result) => println!("{}", result),
            Err(e) => println!("Error: {}", e),
        }

    }
}
