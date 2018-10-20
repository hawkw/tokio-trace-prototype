//! Arbitrarily typed field values.
use std::{any::Any, borrow::Borrow, fmt};

/// A formattable field value of an erased type.
pub trait Value: fmt::Debug + Send + Sync {}

/// An owned value of an erased type.
///
/// Like `Any`, references to `OwnedValue` may attempt to downcast the value to
/// a concrete type. However, unlike `Any`, `OwnedValue`s are constructed from
/// types known to implement `fmt::Debug`. This means that arbitrary
/// `OwnedValue`s may be formatted using the erased type's `fmt::Debug`
/// implementation, _even when the erased type is no longer known_.
pub struct OwnedValue {
    my_debug_impl: fn(&Any, &mut fmt::Formatter) -> fmt::Result,
    any: Box<dyn Any + Send + Sync>,
}

impl<T> Value for T where T: fmt::Debug + Send + Sync {}

/// Trait representing a value which may be converted into an `OwnedValue`.
///
/// References to types implementing `IntoValue` may be formatted (as `Value`s),
/// _or_ may be converted into owned `OwnedValue`s. In addition to being owned,
/// instances of `OwnedValue` may also be downcast to their original erased type.
pub trait IntoValue: Value {
    // like `Clone`, but "different"
    fn into_value(&self) -> OwnedValue;
}

impl<T, V> IntoValue for T
where
    T: ToOwned<Owned = V> + Value,
    V: Borrow<T> + Value + 'static,
{
    fn into_value(&self) -> OwnedValue {
        let any: Box<dyn Any + Send + Sync> = Box::new(self.to_owned());
        debug_assert!(
            any.as_ref().downcast_ref::<V>().is_some(),
            "Box<Any> must be downcastable for OwnedValue to work",
        );
        // This closure "remembers" the original type `V` before it is erased, and
        // can thus format the `Any` by downcasting it back to `V` and calling
        // `V`'s debug impl.
        let my_debug_impl = |me: &Any, f: &mut fmt::Formatter| {
            me.downcast_ref::<V>()
                .expect("type should downcast to its pre-erasure type")
                .fmt(f)
        };
        OwnedValue { my_debug_impl, any }
    }
}

impl OwnedValue {
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.any.downcast_ref()
    }

    pub fn is<T: Any>(&self) -> bool {
        self.any.is::<T>()
    }
}

impl fmt::Debug for OwnedValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (self.my_debug_impl)(self.any.as_ref(), f)
    }
}
