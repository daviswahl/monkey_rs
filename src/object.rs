use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use environment::Environment;
use token;
use ast;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    StringLiteral(String),
    Return(Box<Object>),
    Function(Vec<ast::IdentifierExpression>, Box<ast::Statement>, Rc<RefCell<Environment>>),
    BlockArgument(Vec<ast::IdentifierExpression>, Box<ast::Statement>, Rc<RefCell<Environment>>),
    BuiltinFunction(token::Token),
    ArrayLiteral(Vec<Object>),
    Null,
}


pub enum ObjectResult<'a> {
    Ref(Result<&'a Object, String>),
    Value(Result<Object, String>),
    MultiRef(Result<Vec<&'a Object>, String>),
    MultiValue(Result<Vec<Object>, String>)
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Object::*;
        let t = match *self {
            Integer(i) => i.to_string(),
            Boolean(b) => b.to_string(),
            Return(ref r) => r.to_string(),
            Function(ref params, ref body, _) => {
                let params = params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(",");
                format!("fn ({}) {{ {} }}", params, body)
            }
            StringLiteral(ref s) => s.to_string(),
            Null => return Ok(()),
            BuiltinFunction(ref b) => b.literal(),
            ArrayLiteral(ref elements) => {
                let elems = elements
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(",");
                format!("[{}]", elems)
            }
            BlockArgument(ref params, ref block, _) => {
                let parameters = params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("{{ |{}| {} }}", parameters, block)
            }
        };
        write!(f, "{}", t)
    }
}
