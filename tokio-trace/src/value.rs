use std::{any::Any, fmt};

pub trait Value: fmt::Debug + Send + Sync { }

pub trait OwnedValue: Value + Any {
}

impl<T> Value for T
where
    T: fmt::Debug + Send + Sync,
{ }

// TODO: should `IntoValue` also be `Value`?
pub trait IntoValue {
    // like `Clone`, but "different"
    fn into_value(&self) -> Box<dyn OwnedValue>;
}

impl<T> OwnedValue for T
where
    T: Any + Value,
{ }

impl<T> IntoValue for T
where
    T: ToOwned,
    <T as ToOwned>::Owned: OwnedValue,
{
    fn into_value(&self) -> Box<dyn OwnedValue> {
        Box::new(self.to_owned())
    }
}
