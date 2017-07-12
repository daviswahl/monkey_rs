use object;
use std::fmt;
#[derive(Debug, PartialEq, Clone)]
pub enum Builtin {
    Len(Box<object::Object>)
}

pub static BUILTINS: [&'static str; 1] = [
    "len",
];

pub fn is_builtin(s: &str) -> bool {
    BUILTINS.iter().any(|b| b == &s)
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Builtin::*;
        let t = match *self {
            Len(ref arg) => format!("len({})", arg)
        };
        write!(f, "{}", t)
    }
}
