use object;
use object::{ObjectRcResult, Object};
use token;
use environment;
use runtime;
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Builtin {
    Len,
    Print,
    First,
    Last,
    Rest,
    Push,
    Eval,
    Stats,
    While,
}

impl Builtin {
    fn call(
        &self,
        args: Vec<Rc<Object>>,
        env: Rc<RefCell<environment::Environment>>,
        runtime: &runtime::Runtime,
    ) -> ObjectRcResult {
        match self {
            &Builtin::Len => check_arity_1(args).and_then(|arg1| len(arg1)),
            &Builtin::Print => check_arity_1(args).and_then(|arg1| print(arg1, runtime)),
            &Builtin::First => check_arity_1(args).and_then(|arg1| first(arg1)),
            &Builtin::Last => check_arity_1(args).and_then(|arg1| last(arg1)),
            &Builtin::Rest => check_arity_1(args).and_then(|arg1| rest(arg1)),
            &Builtin::Eval => check_arity_1(args).and_then(|arg1| eval(arg1, env)),
            &Builtin::Push => check_arity_2(args).and_then(|(arg1, arg2)| push(arg1, arg2)),
            &Builtin::Stats => check_arity_0(args).and_then(|_| stats(env, runtime)),
            &Builtin::While => check_arity_0(args).and_then(|_| stats(env, runtime)),
        }
    }
}

pub static BUILTINS: [(&'static str, Builtin); 9] = [
    ("len", Builtin::Len),
    ("print", Builtin::Print),
    ("first", Builtin::First),
    ("last", Builtin::Last),
    ("rest", Builtin::Rest),
    ("push", Builtin::Push),
    ("eval", Builtin::Eval),
    ("stats", Builtin::Stats),
    ("while", Builtin::While),
];

fn check_arity_0(args: Vec<Rc<object::Object>>) -> Result<(), String> {
    if args.len() == 0 {
        return Ok(());
    }
    Err(format!("expected 0 arguments, got {}", args.len()))
}

fn check_arity_1(args: Vec<Rc<object::Object>>) -> Result<(Rc<Object>), String> {
    if args.len() == 1 {
        return Ok((args[0].clone()));
    }
    Err(format!("expected 1 argument, got {}", args.len()))
}

fn check_arity_2(args: Vec<Rc<object::Object>>) -> Result<(Rc<Object>, Rc<Object>), String> {
    if args.len() == 2 {
        return Ok((args[0].clone(), args[1].clone()));
    }
    Err(format!("expected 2 arguments, got {}", args.len()))
}


fn to_str(b: &Builtin) -> &'static str {
    BUILTINS
        .iter()
        .find(|&&(_, ref v)| v == b)
        .map(|&(k, _)| k)
        .unwrap()
}

fn lookup(s: &str) -> Result<Builtin, String> {
    BUILTINS
        .iter()
        .find(|&&(ref k, _)| k == &s)
        .map(|&(_, b)| b)
        .ok_or(format!("could not locate builtin: {}", s))
}

pub fn from_token(t: &token::Token) -> Result<Builtin, String> {
    lookup(t.literal().as_str())
}

pub fn is_builtin(s: &str) -> bool {
    BUILTINS.iter().any(|&(ref b, _)| b == &s)
}

pub fn call(
    tok: &token::Token,
    env: Rc<RefCell<environment::Environment>>,
    runtime: &runtime::Runtime,
    args: Vec<Rc<object::Object>>,
) -> ObjectRcResult {
    from_token(tok).and_then(|builtin| builtin.call(args, env, runtime))
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", to_str(self))
    }
}

fn len(arg: Rc<Object>) -> ObjectRcResult {
    match *arg {
        Object::StringLiteral(ref s) => Ok(Rc::new(Object::Integer(s.len() as i64))),
        Object::ArrayLiteral(ref array) => Ok(Rc::new(Object::Integer(array.len() as i64))),
        ref x => Err(format!("len: unsupported type {}", x)),
    }
}

fn print(arg: Rc<Object>, runtime: &runtime::Runtime) -> ObjectRcResult {
    println!("{}", arg);
    Ok(runtime.NULL())
}

fn last(arg: Rc<Object>) -> ObjectRcResult {
    match *arg {
        Object::ArrayLiteral(ref array) => Ok(array.get(array.len() - 1).unwrap().clone()),
        ref x => Err(format!("first: unsupported type {}", x)),
    }
}

fn first(arg: Rc<Object>) -> ObjectRcResult {
    match *arg {
        Object::ArrayLiteral(ref array) => Ok(array.get(0).unwrap().clone()),
        ref x => Err(format!("first: unsupported type {}", x)),
    }
}

fn push(array: Rc<Object>, value: Rc<Object>) -> ObjectRcResult {
    match *array {
        Object::ArrayLiteral(ref array) => {
            let mut new = array.clone();
            new.push(value);
            Ok(Rc::new(Object::ArrayLiteral(new)))
        }
        ref x => Err(format!("first: unsupported type {}", x)),
    }
}

fn eval(arg: Rc<Object>, env: Rc<RefCell<environment::Environment>>) -> ObjectRcResult {

    use parser;
    use evaluator;
    match *arg {
        Object::StringLiteral(ref string) => {
            let program = parser::parse(string).expect("could not parse");
            evaluator::eval(program, env)
        }
        ref x => Err(format!("first: unsupported tpye {}", x)),
    }
}

fn stats(env: Rc<RefCell<environment::Environment>>, runtime: &runtime::Runtime) -> ObjectRcResult {
    runtime.stats();
    env.borrow().stats(0);
    Ok(runtime.NULL())
}

fn rest(arg: Rc<Object>) -> ObjectRcResult {
    match *arg {
        Object::ArrayLiteral(ref array) => {
            array
                .split_first()
                .map(|(_, tail)| Rc::new(Object::ArrayLiteral(tail.to_owned())))
                .ok_or("could not split vec".to_string())
        }
        ref x => Err(format!("first: unsupported type {}", x)),
    }
}
