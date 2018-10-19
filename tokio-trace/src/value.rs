use std::{any::Any, borrow::Borrow, fmt};

pub trait Value: fmt::Debug + Send + Sync {}

pub struct OwnedValue {
    my_debug_impl: fn(&(), &mut fmt::Formatter) -> fmt::Result,
    any: Box<dyn Any + Send + Sync>,
}

impl<T> Value for T where T: fmt::Debug + Send + Sync {}

// TODO: should `IntoValue` also be `Value`?
pub trait IntoValue {
    // like `Clone`, but "different"
    fn into_value(&self) -> OwnedValue;
}

impl<T, V> IntoValue for T
where
    T: ToOwned<Owned = V>,
    V: Borrow<T> + Value + 'static,
{
    fn into_value(&self) -> OwnedValue {
        let owned = self.to_owned();
        // I'll just make the damn vtable *myself*!
        let my_debug_impl = |me: &(), f: &mut fmt::Formatter| unsafe {
            let me = &*(me as *const () as *const V);
            fmt::Debug::fmt(me, f)
        };
        let boxed = OwnedValue {
            my_debug_impl,
            any: Box::new(owned),
        };
        boxed
    }
}

impl OwnedValue {
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.any.downcast_ref()
    }
}

impl fmt::Debug for OwnedValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let me = unsafe { &*(self.any.as_ref() as *const _ as *const ()) };
        (self.my_debug_impl)(me, f)
    }
}
