use std::collections::HashMap;
use object::Object;
use std::cell::RefCell;
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
        self.env.get(&ident).map(|e| e.clone()).or(
            self.outer.as_ref().and_then(|e| e.borrow().get(ident)),
        )
    }

    pub fn set(&mut self, ident: String, obj: Rc<Object>) {
        self.env.insert(ident, obj);
    }

    pub fn update(&mut self, ident: String, obj: Rc<Object>) -> Result<(), String> {
        if self.env.contains_key(&ident) {
            self.env.insert(ident, obj);
            return Ok(());
        }
        self.outer
            .as_ref()
            .ok_or(format!("unknown identifier: {}", ident))
            .and_then(|outer| outer.borrow_mut().update(ident, obj))
    }

    pub fn extend(env: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            outer: Some(env),
            env: HashMap::new(),
        }
    }

    pub fn stats(&self, indent: i32) {
        let mut indentation: String = String::from("");
        let mut i = 0;
        while i < indent {
            indentation.push(' ');
            i += 1;
        }

        for (key, value) in self.env.iter() {
            println!("{}{}: {}", indentation, key, Rc::strong_count(value))
        }

        self.outer.as_ref().map(
            |outer| outer.borrow().stats(indent + 2),
        );
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extend() {
        use std::borrow::Borrow;
        let env = Environment::new();

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
