
pub type ObjectType = &'static str;

#[derive(Debug)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null)
}

const INTEGER_OBJ: ObjectType = "INTEGER";
const BOOLEAN_OBJ: ObjectType = "BOOLEAN";
const NULL_OBJ: ObjectType = "NULL";

pub trait Obj : Into<Object> {
    fn typ(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Debug)]
pub struct Integer {
    pub value: i64,
}


impl From<Integer> for Object {
    fn from(i: Integer) -> Self {
        Object::Integer(i)
    }
}

impl Obj for Integer {

    fn typ(&self) -> ObjectType {
        INTEGER_OBJ
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug)]
struct Boolean {
    value: bool,
}

impl From<Boolean> for Object {
    fn from(b: Boolean) -> Self {
        Object::Boolean(b)
    }
}

impl Obj for Boolean {
    fn typ(&self) -> ObjectType {
        BOOLEAN_OBJ
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}


#[derive(Debug)]
struct Null {}

impl From<Null> for Object {
    fn from(n: Null) -> Self {
        Object::Null(n)
    }
}

impl Obj for Null {
    fn typ(&self) -> ObjectType {
        NULL_OBJ
    }

    fn inspect(&self) -> String {
        String::from("null")
    }
}
