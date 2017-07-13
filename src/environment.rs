use std::collections::HashMap;
use object::Object;
use std::cell::{RefCell};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    outer: Option<Rc<RefCell<Environment>>>,
    env: HashMap<String, Rc<Object>>,
}

impl Environment {
    pub fn new() -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            outer: None,
            env: HashMap::new(),
        }))
    }

    pub fn get(&self, ident: String) -> Option<Rc<Object>> {
        let outer = self.outer.clone();
        self.env.get(&ident).map(|e| e.clone()).or(
            outer.and_then(|e| {
                e.borrow().get(ident)
            }),
        )
    }

    pub fn set(&mut self, ident: String, obj: Rc<Object>) {
        self.env.insert(ident, obj.clone());
    }

    pub fn extend(env: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            outer: Some(env),
            env: HashMap::new(),
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extend() {
        use std::borrow::Borrow;
        let mut env = Environment::new();

        env.borrow_mut().set(
            String::from("foo"),
            Rc::new(Object::Null),
        );

        {
            let mut env2 = Environment::extend(env.clone());

            env2.set(String::from("bar"), Rc::new(Object::Boolean(false)));

            assert_eq!(
                env2.get(String::from("bar")).unwrap(),
                Rc::new(Object::Boolean(false))
            );
            assert_eq!(
                env2.borrow().get(String::from("foo")).unwrap(),
                Rc::new(Object::Null)
            );
        }

        assert_eq!(
            env.as_ref().borrow().get(String::from("foo")).unwrap(),
            Rc::new(Object::Null)
        );
    }
}
