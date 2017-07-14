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

#[derive(Debug, Clone)]
pub enum ObjectResult<'a> {
    Ref(&'a Object),
    Value(Object),
    MultiRef(Vec<&'a Object>),
    MultiValue(Vec<Object>)
}

impl <'a>ObjectResult<'a> {
    pub fn unwrap_ref(self, s: &str, l: u32) -> &'a Object {
        match self {
            ObjectResult::Ref(r) => r,
            o => panic!("not a ref: file: {}, line: {}", s, l)
        }
    }

    pub fn unwrap_value(self, s: &str, l: u32) -> Object {
        match self {
            ObjectResult::Value(object) => object,
            o => panic!("not an object: file: {}, line: {}", s, l)
        }
    }
}

impl <'a>From<ObjectResult<'a>> for Vec<Object> {
    fn from(result: ObjectResult) -> Self {
        match result {
           ObjectResult::MultiValue(v) => v,
            x => panic!("could not unwrap {:?} into Vec<Object>", x)
        }
    }
}

impl <'a>From<Object> for ObjectResult<'a> {
    fn from(obj: Object) -> Self {
        ObjectResult::Value(obj)
    }
}

impl <'a>From<&'a Object> for ObjectResult<'a> {
    fn from(obj: &'a Object) -> Self {
        ObjectResult::Ref(obj)
    }
}

impl <'a>From<Vec<Object>> for ObjectResult<'a> {
    fn from(obj: Vec<Object>) -> Self {
        ObjectResult::MultiValue(obj)
    }
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
