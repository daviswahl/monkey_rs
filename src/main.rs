extern crate monkey_parser;

use std::env;
use std::fs;
use std::path;
use std::io::Read;

fn main() {
    for arg in env::args().skip(1) {
        match arg.as_str() {
            "repl" => monkey_parser::repl::run(),
            _ => eval_file(arg),
        }
    }
}

fn eval_file(s: String) {
    let p = path::PathBuf::from(s);
    let f = fs::File::open(p);
    let mut buf: String = String::new();
    let environment = monkey_parser::environment::Environment::new();

    match f {
        Ok(mut file) => {
            match file.read_to_string(&mut buf).map(|_| {
                match monkey_parser::eval(buf.as_str(), environment) {
                    Ok(r) => println!("{}", r),
                    Err(e) => println!("Error: {}", e),
                }
            }) {
                Ok(_) => (),
                Err(e) => println!("{}", e),
            }
        }
        Err(e) => println!("Error: {}", e),
    };
}
