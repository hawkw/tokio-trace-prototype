use std::{any::Any, fmt};

pub trait Value: fmt::Debug + Send + Sync { }

pub struct OwnedValue(Box<dyn Any + Send + Sync>);

impl<T> Value for T
where
    T: fmt::Debug + Send + Sync,
{ }

// TODO: should `IntoValue` also be `Value`?
pub trait IntoValue {
    // like `Clone`, but "different"
    fn into_value(&self) -> OwnedValue;
}

impl<T> IntoValue for T
where
    T: ToOwned,
    <T as ToOwned>::Owned: Value + 'static,
{
    fn into_value(&self) -> OwnedValue {
        OwnedValue(Box::new(self.to_owned()))
    }
}

impl OwnedValue {
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.0.downcast_ref()
    }
}

impl fmt::Debug for OwnedValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let this = unsafe {
            // An impl of `Debug` for the boxed type is known to exist, because
            // the `OwnedValue` constructor requires a type implementing
            // `Debug`.
            ::std::mem::transmute::<&dyn Any, &dyn fmt::Debug>(self.0.as_ref())
        };
        fmt::Debug::fmt(this, f)
    }
}
