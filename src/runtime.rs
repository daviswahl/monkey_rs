use std::rc::Rc;
use object::Object;

pub struct Runtime {
    globals: [Rc<Object>; 3],
}

pub fn new() -> Runtime {
        let globals = [
            Rc::new(Object::Null),
            Rc::new(Object::Boolean(true)),
            Rc::new(Object::Boolean(false)),
        ];
        Runtime{globals}
}

#[allow(non_snake_case)]
impl Runtime {
    pub fn NULL(&self) -> Rc<Object> {
        self.globals[0].clone()
    }

    pub fn TRUE(&self) -> Rc<Object> {
        self.globals[1].clone()
    }

    pub fn FALSE(&self) -> Rc<Object> {
        self.globals[2].clone()
    }

    pub fn bool(&self, b: bool) -> Rc<Object> {
        if b { self.TRUE() } else { self.FALSE() }
    }
}
