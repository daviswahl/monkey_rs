use std::fmt::Debug;

#[derive(Debug, Clone)]
pub enum Lazy<'a, T>
where
    T: Clone + Debug + 'a,
{
    Ref(&'a T),
    Val(T),
}

pub fn new<'a, T>(t: T) -> Lazy<'a, T>
where
    T: Debug + Clone + 'a,
{
    Lazy::Val(t)
}

impl<'a, T> Lazy<'a, T>
where
    T: Clone + Debug + 'a,
{
    pub fn to_owned(self) -> T {
        match self {
            Lazy::Val(v) => v,
            Lazy::Ref(v) => {
                println!("cloned a ref: {:?}", v);
                v.clone()
            }
        }
    }

    pub fn as_ref(&'a self) -> &'a T {
        match self {
            &Lazy::Val(ref v) => v,
            &Lazy::Ref(r) => r,
        }
    }

    pub fn unwrap(self) -> T {
        match self {
            Lazy::Val(object) => object,
            v => panic!("not a value: {:?}", v),
        }
    }

    pub fn try_value(self) -> Result<T, &'static str> {
        match self {
            Lazy::Val(v) => Ok(v),
            Lazy::Ref(_) => Err("could not unwrap value"),
        }
    }

    pub fn is_ref(&'a self) -> bool {
        match self {
            &Lazy::Val(_) => false,
            &Lazy::Ref(_) => true,
        }
    }

    pub fn is_val(&'a self) -> bool {
        !self.is_ref()
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_from_t() {}
}
