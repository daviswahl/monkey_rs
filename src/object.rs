use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Rc<Object>),
    Null
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Object::*;
        let t = match *self {
            Integer(_) => "INTEGER",
            Boolean(_) => "BOOLEAN",
            Return(_) => "RETURN",
            Null => "NULL",
        };
        write!(f, "{}", t)
    }
}
