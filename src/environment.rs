use std::collections::HashMap;
use object::Object;
use std::cell::RefCell;
use std::rc::Rc;
use ast;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    outer: Option<Rc<RefCell<Environment>>>,
    env: HashMap<String, Rc<Object>>,
    block: Option<Rc<Object>>,
}

pub fn extend_function_env(
    parameters: &Vec<ast::IdentifierExpression>,
    env: Rc<RefCell<Environment>>,
    args: Vec<Rc<Object>>,
    block: Option<Rc<Object>>
) -> Result<Environment, String> {
    let mut extended = extend(env, block);

    if parameters.len() != args.len() {
        return Err(format!(
            "function expected {} arguments, got {}",
            parameters.len(),
            args.len()
        ));
    }

    for (i, param) in parameters.into_iter().enumerate() {
        extended.set(param.value.clone(), args[i].clone())
    }

    Ok(extended)
}

pub fn extend(env: Rc<RefCell<Environment>>, block: Option<Rc<Object>>) -> Environment {
        Environment {
            block: block,
            outer: Some(env),
            env: HashMap::new(),
        }
}
impl Environment {
    pub fn new() -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            outer: None,
            block: None,
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



    pub fn block(&self) -> Option<Rc<Object>> {
        self.block.as_ref().map(|b| b.clone()).or(
            self.outer.as_ref().and_then(|e| e.borrow().block())
        )
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
            let mut env2 = extend(env.clone(), None);

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
