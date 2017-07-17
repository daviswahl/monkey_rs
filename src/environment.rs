use std::collections::HashMap;
use object::Object;
use std::cell::RefCell;
use std::rc::Rc;
use ast;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    outer: Option<Rc<RefCell<Environment>>>,
    env: HashMap<String, Object>,
    block: Option<Object>,
}

pub fn extend_function_env(
    parameters: Vec<ast::IdentifierExpression>,
    env: Rc<RefCell<Environment>>,
    args: Vec<Object>,
    block: Option<Object>
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

pub fn extend(env: Rc<RefCell<Environment>>, block: Option<Object>) -> Environment {
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

    pub fn get(&self, ident: String) -> Option<Object> {
        self.env.get(&ident).map(|e| e.clone()).or(
            self.outer.as_ref().and_then(|e| e.borrow().get(ident)),
        )
    }

    pub fn set(&mut self, ident: String, obj: Object) {
        self.env.insert(ident, obj);
    }

    pub fn update(&mut self, ident: String, obj: Object) -> Result<(), String> {
        if self.env.contains_key(&ident) {
            self.env.insert(ident, obj);
            return Ok(());
        }
        self.outer
            .as_ref()
            .ok_or(format!("unknown identifier: {}", ident))
            .and_then(|outer| outer.borrow_mut().update(ident, obj))
    }



    pub fn block(&mut self) -> &mut Option<Object> {
        &mut self.block
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
            Object::Null,
        );

        {
            let mut env2 = extend(env.clone(), None);

            env2.set(String::from("bar"), Object::Boolean(false));

            assert_eq!(
                env2.get(String::from("bar")).unwrap(),
                Object::Boolean(false)
            );
            assert_eq!(
                env2.borrow().get(String::from("foo")).unwrap(),
                Object::Null
            );
        }

        assert_eq!(
            env.as_ref().borrow().get(String::from("foo")).unwrap(),
            Object::Null
        );
    }

}
