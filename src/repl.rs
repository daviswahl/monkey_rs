use environment::Environment;
use object;
use evaluator;
use parser;

pub fn run() {
    use std::io;

    use std::io::Write;
    let mut input = String::new();
    let env = Environment::new();

    print!("> ");
    match io::stdout().flush() {
        Ok(_) => (),
        Err(_) => return

    }

    while let Ok(_) = io::stdin().read_line(&mut input) {
        match parser::parse(input.as_str()) {
            Ok(node) => match evaluator::eval(node, env.clone()) {
                Ok(result) => {
                    match * result {
                        object::Object::Null => (),
                        _ => println ! ("{}", result),
                    }
                },
                Err(e) => println ! ("Error: {}", e),
            },
            Err(e) => println!("error: {:?}", e)
        }
        input.clear();

        print!("> ");
        match io::stdout().flush() {
            Ok(_) => (),
            Err(e) => println!("error: {}", e),
        }
    }
}
