extern crate monkey_parser;

use monkey_parser::environment::Environment;
use monkey_parser::object;

fn main() {
    repl()
}

fn repl() {
    use std::io;

    use std::io::Write;
    let mut input = String::new();
    let env = Environment::new();

    print!("> ");
    io::stdout().flush();

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

        print!("> ");
        io::stdout().flush();
    }
}
