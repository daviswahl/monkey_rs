use object;
use object::{ObjectsResult, ObjectResult, ObjectRcResult, Object};
use token;
use environment;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Builtin {
    Len
}


fn check_arity_0(args: Vec<Rc<object::Object>>) -> Result<(), String> {
    if args.len() == 0 {
        return Ok(())
    }
    Err(format!("expected 0 arguments, got {}", args.len()))
}

fn check_arity_1(args: Vec<Rc<object::Object>>) -> Result<(Rc<Object>), String> {
    if args.len() == 1 {
        return Ok((args[0].clone()))
    }
    Err(format!("expected 1 argument, got {}", args.len()))
}

impl Builtin {
    fn call(&self, args: Vec<Rc<Object>>, env: &environment::Environment) -> ObjectRcResult {
        match self{
            Len => {
                check_arity_1(args).and_then(|arg1| len(arg1))
            }
        }
    }
}


pub static BUILTINS: [&'static str; 1] = [
    "len",
];


pub fn from_token(s: &token::Token) -> Result<Builtin, String> {
   match s.literal().as_str() {
       "len" => Ok(Builtin::Len),
       ref x => Err(format!("Unknown builtin function: {}", x))
    }
}

pub fn is_builtin(s: &str) -> bool {
    BUILTINS.iter().any(|b| b == &s)
}

pub fn call(tok: &token::Token, env: &environment::Environment, args: Vec<Rc<object::Object>>) -> ObjectRcResult {
    from_token(tok).and_then(|builtin| {
        builtin.call(args, env)
    })
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Builtin::*;
        let t = match *self {
            Len => format!("len")
        };
        write!(f, "{}", t)
    }
}

fn len(arg: Rc<Object>) -> ObjectRcResult {
    match *arg {
        Object::StringLiteral(ref s) => Ok(Rc::new(Object::Integer(s.len() as i64))),
        ref x => Err(format!("len: unsupported type {}", x))
    }
}
