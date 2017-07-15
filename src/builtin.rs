use object;
use object::{Object};
use lazy;
use token;
use environment;
use runtime;
use evaluator;
use evaluator::EvalResult;
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
    Yield,
}

impl Builtin {
    fn call<'a>(
        &self,
        args: &Vec<Object>,
        env: Rc<RefCell<environment::Environment>>,
        evaluator: &'a evaluator::Evaluator,
    ) -> EvalResult<'a> {
        match self {
            &Builtin::Len => check_arity_1(args).and_then(|arg1| len(arg1)),
            &Builtin::Print => check_arity_1(args).and_then(|arg1| print(arg1, &evaluator.runtime)),
            &Builtin::First => check_arity_1(args).and_then(|arg1| first(arg1)),
            &Builtin::Last => check_arity_1(args).and_then(|arg1| last(arg1)),
            &Builtin::Rest => check_arity_1(args).and_then(|arg1| rest(arg1)),
            &Builtin::Eval => check_arity_1(args).and_then(|arg1| eval(arg1, env, evaluator)),
            &Builtin::Push => check_arity_2(args).and_then(|(arg1, arg2)| push(arg1, arg2)),
            &Builtin::Stats => check_arity_0(args).and_then(|_| stats(env, &evaluator.runtime)),
            &Builtin::While => check_arity_0(args).and_then(|_| stats(env, &evaluator.runtime)),
            &Builtin::Yield => _yield(args, env, evaluator),
        }
    }
}

pub static BUILTINS: [(&'static str, Builtin); 10] = [
    ("len", Builtin::Len),
    ("print", Builtin::Print),
    ("first", Builtin::First),
    ("last", Builtin::Last),
    ("rest", Builtin::Rest),
    ("push", Builtin::Push),
    ("eval", Builtin::Eval),
    ("stats", Builtin::Stats),
    ("while", Builtin::While),
    ("yield", Builtin::Yield),
];

fn check_arity_0(args: &Vec<Object>) -> Result<(), String> {
    if args.len() == 0 {
        return Ok(());
    }
    Err(format!("expected 0 arguments, got {}", args.len()))
}

fn check_arity_1(args: &Vec<Object>) -> Result<(&Object), String> {
    if args.len() == 1 {
        return Ok((&args[0]));
    }
    Err(format!("expected 1 argument, got {}", args.len()))
}

fn check_arity_2(args: &Vec<Object>) -> Result<(&Object, &Object), String> {
    if args.len() == 2 {
        return Ok((&args[0], &args[1]));
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

pub fn call<'a>(
    tok: &token::Token,
    env: Rc<RefCell<environment::Environment>>,
    evaluator: &'a evaluator::Evaluator,
    args: &Vec<object::Object>,
) -> EvalResult<'a> {
    from_token(tok).and_then(|builtin| builtin.call(args, env, evaluator))
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", to_str(self))
    }
}

fn len<'a>(arg: &Object) -> EvalResult<'a> {
    match *arg {
        Object::StringLiteral(ref s) => Ok(Object::Integer(s.len() as i64).into()),
        Object::ArrayLiteral(ref array) => Ok(Object::Integer(array.len() as i64).into()),
        ref x => Err(format!("len: unsupported type {}", x)),
    }
}

fn print<'a>(arg: &Object, runtime: &'a runtime::Runtime) -> EvalResult<'a> {
    println!("{}", arg);
    Ok(runtime.NULL().into())
}

fn last<'a>(arg: &Object) -> EvalResult<'a> {
    match *arg {
        Object::ArrayLiteral(ref array) => Ok(array.get(array.len() - 1).unwrap().clone().into()),
        ref x => Err(format!("first: unsupported type {}", x)),
    }
}

fn first<'a>(arg: &Object) -> EvalResult<'a> {
    match *arg {
        Object::ArrayLiteral(ref array) => Ok(array.get(0).unwrap().clone().into()),
        ref x => Err(format!("first: unsupported type {}", x)),
    }
}

fn push<'a>(array: &Object, value: &Object) -> EvalResult<'a> {
    match *array {
        Object::ArrayLiteral(ref array) => {
            let mut new = array.clone();
            new.push(value.clone());
            Ok(Object::ArrayLiteral(new).into())
        }
        ref x => Err(format!("first: unsupported type {}", x)),
    }
}

fn eval<'a>(arg: &Object, env: Rc<RefCell<environment::Environment>>, evaluator: &'a evaluator::Evaluator) -> EvalResult<'a> {

    use parser;
    use evaluator;
    match *arg {
        Object::StringLiteral(ref string) => {
            let program = parser::parse(string).expect("could not parse");
            evaluator::eval(program, env, evaluator)
        }
        ref x => Err(format!("first: unsupported tpye {}", x)),
    }
}

fn stats<'a>(
    env: Rc<RefCell<environment::Environment>>,
    runtime: &'a runtime::Runtime,
) -> EvalResult<'a> {
    runtime.stats();
    env.borrow().stats(0);
    Ok(runtime.NULL().into())
}

fn rest<'a>(arg: &Object) -> EvalResult<'a> {
    match arg {
        &Object::ArrayLiteral(ref array) => {
            array
                .split_first()
                .map(|(_, tail)| Object::ArrayLiteral(tail.to_owned()).into())
                .ok_or("could not split vec".to_string())
        }
        ref x => Err(format!("first: unsupported type {}", x)),
    }
}

fn _yield<'a>(
    args: &Vec<Object>,
    env: Rc<RefCell<environment::Environment>>,
    evaluator: &'a evaluator::Evaluator,
) -> EvalResult<'a> {
    let borrow = env.borrow();
    let block = borrow.block();
    match block.as_ref().unwrap() {
        &Object::BlockArgument(ref params, ref stmt, ref block_env) => {
            let extended_env =
                environment::extend_function_env(&params, block_env.clone(), args, None)?;
            evaluator.visit_statement(*stmt.clone(), Rc::new(RefCell::new(extended_env)))
        }
        ref x => Err(format!("Expected block argument, got: {}", x)),
    }

}
