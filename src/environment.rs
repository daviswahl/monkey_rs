use std::collections::HashMap;
use object::Object;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment<'a> {
    outer: Option<&'a Environment<'a>>,
    env: HashMap<String, Rc<Object<'a>>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Environment<'a> {
        Environment {
            outer: None,
            env: HashMap::new(),
        }
    }

    pub fn get(&self, ident: String) -> Option<Rc<Object<'a>>> {
        self.env.get(&ident).map(|e| e.clone()).or(
            self.outer.and_then(|e| e.get(ident)),
        )
    }

    pub fn set(&mut self, ident: String, obj: Rc<Object<'a>>) {
        self.env.insert(ident, obj.clone());
    }

    pub fn extend(&'a self) -> Environment<'a> {
        Environment {
            outer: Some(&self),
            env: HashMap::new(),
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extend() {
        let mut env = Environment::new();

        env.set(String::from("foo"), Rc::new(Object::Null));

        {
            let mut env2 = env.extend();

            env2.set(String::from("bar"), Rc::new(Object::Boolean(false)));

            assert_eq!(
                env2.get(String::from("bar")).unwrap(),
                Rc::new(Object::Boolean(false))
            );
            assert_eq!(env2.get(String::from("foo")).unwrap(), Rc::new(Object::Null));
        }

        assert_eq!(env.get(String::from("foo")).unwrap(), Rc::new(Object::Null));
    }
}
