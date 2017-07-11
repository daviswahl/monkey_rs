use std::collections::HashMap;
use object::Object;
use std::rc::Rc;
use std::marker::PhantomData;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    outer: Option<Rc<Environment>>,
    env: HashMap<String, Rc<Object>>,
}


impl Environment {
    pub fn new() -> Environment {
        Environment {
            outer: None,
            env: HashMap::new(),
        }
    }

    pub fn get(&self, ident: String) -> Option<Rc<Object>> {
        self.env.get(&ident).map(|e| e.clone()).or(
            self.outer.clone().and_then(|e| e.get(ident)),
        )
    }

    pub fn set(&mut self, ident: String, obj: Rc<Object>) {
        self.env.insert(ident, obj.clone());
    }

    pub fn extend(env: &Rc<Environment>) -> Environment {
        Environment {
            outer: Some(env.clone()),
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

        let env = Rc::new(env);
        {
            let mut env2 = Environment::extend(&env);

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
