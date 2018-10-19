use std::{any::Any, fmt};

pub trait Value: fmt::Debug + Send + Sync { }

pub trait OwnedValue: Value + Any {
}

impl<T> Value for T
where
    T: fmt::Debug + Send + Sync,
{

}

pub trait Duplicate: Value {
    // like `Clone`, but "different"
    fn duplicate(&self) -> Box<dyn OwnedValue>;
}

impl<T> OwnedValue for T
where
    T: Any + Value,
{ }

impl<T> Duplicate for T
where
    T: Value + ToOwned,
    <T as ToOwned>::Owned: OwnedValue,
{
    fn duplicate(&self) -> Box<dyn OwnedValue> {
        Box::new(self.to_owned())
    }
}
