
pub type ObjectType = &'static str;

#[derive(Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null
}

pub static TRUE: Object = Object::Boolean(true);
pub static FALSE: Object = Object::Boolean(true);

#[derive(Debug)]
pub struct Integer {
    pub value: i64,
}

#[derive(Debug)]
pub struct Boolean {
    value: bool,
}
