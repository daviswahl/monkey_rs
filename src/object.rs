use std::fmt;
use builtin;
use std::rc::Rc;
use environment::Environment;
use token;
use ast;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    StringLiteral(String),
    Return(Rc<Object>),
    Function(Vec<ast::IdentifierExpression>, Box<ast::Statement>, Rc<Environment>),
    BuiltinFunction(token::Token),
    ArrayLiteral(Vec<Rc<Object>>),
    Null
}

pub type ObjectRcResult = Result<Rc<Object>, String>;
pub type ObjectsResult = Result<Vec<Rc<Object>>, String>;
pub type ObjectResult = Result<Object, String>;

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
            Null => return Ok(()),
            BuiltinFunction(ref b) => b.literal(),
            ArrayLiteral(ref elements) => {
                let elems = elements.iter().map(|p| p.to_string()).collect::<Vec<String>>().join(",");
                format!("[{}]", elems)
            }
        };
        write!(f, "{}", t)
    }
}
