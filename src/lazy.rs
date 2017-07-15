use std::fmt::Debug;
use std::ops::Deref;

#[derive(Debug)]
pub enum Lazy<'a, T>
where
    T: Clone + Debug + 'a,
{
    Ref(&'a T),
    Val(T),
}


impl<'a, T> Lazy<'a, T>
where
    T: Clone + Debug + 'a,
{
    pub fn clone(self) -> T {
        match self {
            Lazy::Val(v) => v,
            Lazy::Ref(v) => v.clone(),
        }
    }

    pub fn unwrap_ref(self) -> &'a T {
        match self {
            Lazy::Ref(r) => r,
            Lazy::Val(v) => panic!("not a ref: {:?}", v),
        }
    }

    pub fn unwrap_value(self) -> T {
        match self {
            Lazy::Val(object) => object,
            v => panic!("not a value: {:?}", v),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_from_t() {}
}
