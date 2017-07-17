use object::Object;

pub struct Runtime {
    globals: [Object; 3],
}

pub fn new() -> Runtime {
        let globals = [
           Object::Null,
            Object::Boolean(true),
            Object::Boolean(false),
        ];
        Runtime{globals}
}

#[allow(non_snake_case)]
impl Runtime {
    pub fn NULL(&self) -> &Object {
        &self.globals[0]
    }

    pub fn TRUE(&self) -> &Object {
        &self.globals[1]
    }

    pub fn FALSE(&self) -> &Object {
        &self.globals[2]
    }

    pub fn bool(&self, b: bool) -> &Object {
        if b { self.TRUE() } else { self.FALSE() }
    }
}
