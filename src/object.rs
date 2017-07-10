use std::fmt;
use ast;
use environment::Environment;

#[derive(Debug, PartialEq, Clone)]
pub enum Object<'a> {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object<'a>>),
    Function(Vec<ast::IdentifierExpression>, Box<ast::BlockStatement>, Box<Environment<'a>>),
    Null
}

impl <'a>fmt::Display for Object<'a> {
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
