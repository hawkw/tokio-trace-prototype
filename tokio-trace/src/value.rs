use std::{any::Any, borrow::Borrow, fmt};

pub trait Value: fmt::Debug + Send + Sync {}

pub struct OwnedValue {
    my_debug_impl: fn(&Any, &mut fmt::Formatter) -> fmt::Result,
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
        let any: Box<dyn Any + Send + Sync> = Box::new(self.to_owned());
        debug_assert!(any.as_ref().downcast_ref::<V>().is_some());
        // I'll just make the damn vtable *myself*!
        let my_debug_impl = |me: &Any, f: &mut fmt::Formatter| {
            let me = me.downcast_ref::<V>()
                .expect("type should downcast to its pre-erasure type");
            fmt::Debug::fmt(me, f)
        };
        OwnedValue {
            my_debug_impl,
            any,
        }
    }
}

impl OwnedValue {
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.any.downcast_ref()
    }
}

impl fmt::Debug for OwnedValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (self.my_debug_impl)(self.any.as_ref(), f)
    }
}
