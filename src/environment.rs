use std::collections::HashMap;
use object::Object;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment<'a> {
    outer: Option<&'a Environment<'a>>,
    env: HashMap<String, Object<'a>>,
}

impl <'a>Environment<'a> {

    pub fn new() -> Environment<'a> {
        Environment{outer: None, env: HashMap::new()}
    }

    pub fn get(&self, ident: String) -> Option<&Object> {
        let item = self.env.get(&ident);
        if item.is_none() {
            if self.outer.is_some() {
                return self.outer.unwrap().get(ident)
            }
            return None;
        }
        item
    }

    pub fn set(&mut self, ident: String, obj: Object<'a>) {
        self.env.insert(ident, obj);
    }

    pub fn extend(&'a self) -> Environment<'a> {
        Environment{outer: Some(&self), env: HashMap::new()}
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extend() {
        let mut env = Environment::new();

        env.set(String::from("foo"), Object::Null);

        {
            let mut env2 = env.extend();

            env2.set(String::from("bar"), Object::Boolean(false));

            assert_eq!(env2.get(String::from("bar")).unwrap(), &Object::Boolean(false));
            assert_eq!(env2.get(String::from("foo")).unwrap(), &Object::Null);
        }

        assert_eq!(env.get(String::from("foo")).unwrap(), &Object::Null);
    }
}
