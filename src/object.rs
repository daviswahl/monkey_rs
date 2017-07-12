use std::fmt;
use builtin;
use std::rc::Rc;
use environment::Environment;
use ast;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    StringLiteral(String),
    Return(Rc<Object>),
    Function(Vec<ast::IdentifierExpression>, Box<ast::Statement>, Rc<Environment>),
    BuiltinFunction(builtin::Builtin),
    Null
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Object::*;
        let t = match *self {
            Integer(i) => i.to_string(),
            Boolean(b) => b.to_string(),
            Return(ref r) =>  r.to_string(),
            Function(ref params, ref body, _) => {
                let params = params.iter().map(|p| p.to_string()).collect::<Vec<String>>().join(",");
                format!("fn ({}) {{ {} }}", params, body)
            },
            StringLiteral(ref s) => s.to_string(),
            Null => String::from("NULL"),
            BuiltinFunction(ref b) => b.to_string(),
        };
        write!(f, "{}", t)
    }
}
