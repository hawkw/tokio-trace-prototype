//! Arbitrarily-typed field values.
//!
//! Spans and events may be annotated with key-value data, referred to as known
//! as _fields_. These fields consist of a mapping from a `&'static str` to a
//! piece of data, known as a `Value`.
//!
//! # Value Type Erasure
//!
//! Rather than restricting `Value`s to a set of Rust primitives, `tokio-trace`
//! allows values to be of any type. This means that arbitrary user-defined
//! types may be attached to spans or events, provided they meet certain
//! requirements.
//!
//! Typically, we might accept arbitrarily-typed values by making the
//! `Subscriber` APIs that accept them generic. However, as the `Dispatch` type
//! holds a subscriber as a boxed trait object, the `Subscriber` trait must be
//! object-safe --- it cannot have trait methods that accept a generic
//! parameter. Thus, we represent `Value`s as trait objects, instead.
//!
//! # `Value`s and `Subscriber`s
//!
//! `Subscriber`s consume `Value`s as fields attached to `Event`s or `Span`s.
//! These cases are handled somewhat differently.
//!
//! When a field is attached to an `Event`, the `Subscriber::observe_event`
//! method is passed a slice --- called `field_values` --- of `&dyn Value`
//! references to the event's values. The ordering of `field_values` corresponds
//! to the `field_names` slice in the `Event`'s metadata, so for any given index
//! _i_, the field named `event.meta.field_names[i]` has the value at
//! `event.field_values[i]`. Since an `Event` represents a _moment_ in time, it
//! does not expect to outlive the scope that created it. Thus, the values
//! attached to an `Event` are _borrowed_ from the scope where the `Event`
//! originated.
//!
//! `Span`s, on the other hand, are somewhat more complex. A `Span` may outlive
//! scope in which it was created, and as `Span`s are not instantaneous, the
//! values of their fields may be discovered and added to the span _during_ the
//! `Span`'s execution. Thus, rather than receiving all the field values when
//! the span is initially created, subscribers are instead notified of each
//! field as it is added to the span, via the `Subscriber::add_value` method.
//! That method is called with the span's ID, the name of the field whose value
//! is being added, and the value to add.
//!
//! Since spans may have arbitrarily long lifetimes, passing the subscriber a
//! `&dyn Value` isn't sufficient. Instead, if a subscriber wishes to persist a
//! span value for the entire lifetime of the span, it needs the ability to
//! convert the value into a form in which it is owned by the _subscriber_,
//! rather than the scope in which it was added to the span. For this reason,
//! span values are passed as `&dyn IntoValue`. The `IntoValue` trait is an
//! extension of the `Value` trait that allows conversion into an `OwnedValue`,
//! a type which represents an owned value allocated on the heap. Since some
//! subscriber implementations may _not_ need to persist span field values
//! indefinitely, they are not heap-allocated by default, to avoid unnecessary
//! allocations, but the `IntoValue` trait presents `Subscriber`s with the
//! _option_ to box values should they need to do so.
//!
//! `OwnedValue`s also provide the capacity to optionally _downcast_ the erased
//! type to a concrete type, similarly to
use std::{any::{Any, TypeId}, borrow::Borrow, fmt};

/// A formattable field value of an erased type.
pub trait Value: fmt::Debug + Send + Sync {
    #[doc(hidden)]
    fn type_id(&self) -> TypeId
    where
        Self: 'static,
    {
        TypeId::of::<Self>()
    }
}

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

/// A `Value` which is formatted using `fmt::Display` rather than `fmt::Debug`.
pub struct DisplayValue<T: fmt::Display>(T);

impl<T> Value for T
where
    T: fmt::Debug + Send + Sync
{}

// Copied from `std::any::Any`.
impl Value + 'static {
    /// Returns true if the boxed type is the same as `T`
    #[inline]
    pub fn is<T: Value + 'static>(&self) -> bool {
        // Get TypeId of the type this function is instantiated with
        let t = TypeId::of::<T>();

        // Get TypeId of the type in the trait object
        let boxed = self.type_id();

        // Compare both TypeIds on equality
        t == boxed
    }

    /// Returns some reference to the boxed value if it is of type `T`, or
    /// `None` if it isn't.
    pub fn downcast_ref<T: Value + 'static>(&self) -> Option<&T> {
        if self.is::<T>() {
            unsafe {
                Some(&*(self as *const Value as *const T))
            }
        } else {
            None
        }
    }
}

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
        // This closure "remembers" the original type `V` before it is erased, and
        // can thus format the `Any` by downcasting it back to `V` and calling
        // `V`'s debug impl.
        let my_debug_impl = |me: &Any, f: &mut fmt::Formatter| {
            me.downcast_ref::<V>()
                .expect("type should downcast to its pre-erasure type")
                .fmt(f)
        };
        OwnedValue::new(self, my_debug_impl)
    }
}

// ===== impl OwnedValue =====

impl OwnedValue {
    fn new<T, V>(value: &T, my_debug_impl: fn(&Any, &mut fmt::Formatter) -> fmt::Result) -> Self
    where
        T: ToOwned<Owned = V> + Send + Sync,
        V: Borrow<T> + Send + Sync + 'static,
    {
        let any: Box<dyn Any + Send + Sync> = Box::new(value.to_owned());
        debug_assert!(
            any.as_ref().downcast_ref::<V>().is_some(),
            "Box<Any> must be downcastable for OwnedValue to work",
        );
        OwnedValue { my_debug_impl, any }
    }

    /// Attempts to downcast the `OwnedValue` to a given _concrete_ type.
    ///
    /// Returns a reference to a `T` if the boxed type is `T`, or `None` if it
    /// is not.
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.any.downcast_ref()
    }

    /// Returns `true` if the boxed type is the same as `T`.
    pub fn is<T: Any>(&self) -> bool {
        self.any.is::<T>()
    }
}

impl fmt::Debug for OwnedValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (self.my_debug_impl)(self.any.as_ref(), f)
    }
}

// ===== impl DisplayValue =====

/// Wraps a type implementing `fmt::Display` so that its `Display`
/// implementation will be used when formatting it as a `Value`.
///
/// # Examples
/// ```
/// # extern crate tokio_trace;
/// use tokio_trace::value;
/// # use std::fmt;
/// # fn main() {
///
/// #[derive(Clone, Debug)]
/// struct Foo;
///
/// impl fmt::Display for Foo {
///     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
///         f.pad("Hello, I'm Foo")
///     }
/// }
///
/// let foo = Foo;
/// assert_eq!("Foo".to_owned(), format!("{:?}", foo));
///
/// let foo = value::display(foo);
/// assert_eq!("Hello, I'm Foo".to_owned(), format!("{:?}", foo));
/// # }
/// ```
///
/// ```
/// # extern crate tokio_trace;
/// # use std::fmt;
/// # fn main() {
/// #
/// # #[derive(Clone, Debug)]
/// # struct Foo;
/// #
/// # impl fmt::Display for Foo {
/// #   fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
/// #       f.pad("Hello, I'm Foo")
/// #   }
/// # }
/// use tokio_trace::value::{self, IntoValue};
/// let foo = value::display(Foo);
///
/// let owned_value = foo.into_value();
/// assert_eq!("Hello, I'm Foo".to_owned(), format!("{:?}", owned_value));
///
/// assert!(owned_value.downcast_ref::<Foo>().is_some());
/// # }
/// ```
pub fn display<T>(t: T) -> DisplayValue<T>
where
    T: fmt::Display + Send + Sync,
{
    DisplayValue(t)
}

impl<T: fmt::Display> fmt::Debug for DisplayValue<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T, V> IntoValue for DisplayValue<T>
where
    T: ToOwned<Owned = V> + fmt::Display + Send + Sync,
    V: Borrow<T> + fmt::Display + Send + Sync + 'static,
{
    fn into_value(&self) -> OwnedValue {
        let my_debug_impl = |me: &Any, f: &mut fmt::Formatter| {
            let me = me.downcast_ref::<V>()
                .expect("type should downcast to its pre-erasure type");
            fmt::Display::fmt(me, f)
        };
        OwnedValue::new(&self.0, my_debug_impl)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Debug)]
    struct Foo {
        bar: &'static str,
    }

    impl fmt::Display for Foo {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "hello, I'm {}", self.bar)
        }
    }

    #[test]
    fn display_value_formats_with_display() {
        let foo = Foo { bar: "foo" };
        let display_foo = display(foo.clone());

        assert_eq!(format!("{:?}", foo), "Foo { bar: \"foo\" }".to_owned());
        assert_eq!(format!("{:?}", display_foo), format!("{}", foo));
    }

    #[test]
    fn display_value_is_into_value() {
        let foo = Foo { bar: "foo" };
        let display_foo = display(foo.clone());

        let owned_value: OwnedValue = display_foo.into_value();
        assert_eq!(format!("{:?}", owned_value), format!("{}", foo));
    }

    #[test]
    fn display_value_downcasts_to_original_type() {
        let foo = Foo { bar: "foo" };
        let display_foo = display(foo);

        let owned_value: OwnedValue = display_foo.into_value();
        assert!(owned_value.downcast_ref::<Foo>().is_some());
    }

    #[test]
    fn owned_value_downcasts_to_original_type() {
        let foo = Foo { bar: "foo" };

        let owned_value: OwnedValue = foo.into_value();
        assert!(owned_value.downcast_ref::<Foo>().is_some());
    }
}
