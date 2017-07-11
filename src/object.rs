use std::fmt;
use std::rc::Rc;
use environment::Environment;
use ast;

#[derive(Debug, PartialEq, Clone)]
pub enum Object<'a> {
    Integer(i64),
    Boolean(bool),
    Return(Rc<Object<'a>>),
    Function(Vec<Box<ast::IdentifierExpression>>, Box<ast::BlockStatement>, Rc<Environment<'a>>),
    Null
}

impl <'a>fmt::Display for Object<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Object::*;
        let t = match *self {
            Integer(_) => "INTEGER",
            Boolean(_) => "BOOLEAN",
            Return(_) => "RETURN",
            Function(..) => "FUNCTION",
            Null => "NULL",
        };
        write!(f, "{}", t)
    }
}
