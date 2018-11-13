//! `Span` and `Event` key-value data.
//!
//! Spans and events may be annotated with key-value data, referred to as known
//! as _fields_. These fields consist of a mapping from a `&'static str` to a
//! piece of data, known as a `Value`.
//!
//! # Value Type Erasure
//!
//! Rather than restricting `Value`s to a set of Rust primitives, `tokio-trace`
//! allows values to be of any type. This means that arbitrary user-defined
//! types may be attached to spans or events, provided they implement the
//! `Value` trait or can be formatted with `fmt::Display` or `fmt::Debug`.
//!
//! Typically, we might accept arbitrarily-typed values by making the
//! `Subscriber` APIs that accept them generic. However, as the `Dispatch` type
//! holds a subscriber as a boxed trait object, the `Subscriber` trait must be
//! object-safe --- it cannot have trait methods that accept a generic
//! parameter. Thus, we erase the value's original type.
//!
//! An object-safe API, [`Record`](Record), is provided for consuming
//! `Value`s. A `Record` may implement a set of functions that record various
//! Rust primitive types, allowing user-defined behaviours to be implemented for
//! numbers, boolean values, strings, collections, and so on. `Value`s are
//! required to implement the `record` function, which is passed a mutable
//! reference to a `Record` trait object. The `Value` may choose which of the
//! `Record`'s `record` methods for various types it wishes to call, allowing
//! it to present typed data to the `Record`.
//!
//! # `Value`s and `Subscriber`s
//!
//! `Subscriber`s consume `Value`s as fields attached to `Event`s or `Span`s.
//! These cases are handled somewhat differently.
//!
//! When a field is attached to an `Event`, the `Subscriber::observe_event`
//! method is passed an `Event` struct which provides an iterator
//! (`Event::fields`) to iterate over the event's fields, providing references
//! to the values as `Value` trait objects.
//!
//! `Span`s, on the other hand, are somewhat more complex. As `Span`s are not
//! instantaneous, the values of their fields may be discovered and added to the
//! span _during_ the `Span`'s execution. Thus, rather than receiving all the
//! field values when the span is initially created, subscribers are instead
//! notified of each field as it is added to the span, via the
//! `Subscriber::add_value` method. That method is called with the span's ID,
//! the name of the field whose value is being added, and the value to add.
use ::{ span, Dispatch, Meta};
use std::fmt;

pub trait Value: ::sealed::Sealed {
    fn add_value(&self, span: &span::Id, key: &Key, subscriber: &Dispatch) -> Result<(), ::subscriber::AddValueError>;
}

/// An opaque key allowing _O_(1) access to a field in a `Span` or `Event`'s
/// key-value data.
///
/// As keys are defined by the _metadata_ of a span or event, rather than by an
/// individual instance of a span or event, a key may be used to access the same
/// field across all instances of a given span or event with the same metadata.
/// Thus, when a subscriber observes a new span or event, it need only access a
/// field by name _once_, and use the key for that name for all other accesses.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Key<'a> {
    i: usize,
    metadata: &'a Meta<'a>,
}

/// A `Value` which serializes as a string using `fmt::Display`.
#[derive(Clone)]
pub struct DisplayValue<T: fmt::Display>(T);

/// A `Value` which serializes as a string using `fmt::Debug`.
#[derive(Clone)]
pub struct DebugValue<T: fmt::Debug>(T);

// ===== impl Value =====

impl Value {
    /// Wraps a type implementing `fmt::Display` so that its `Display`
    /// implementation will be used when formatting it as a `Value`.
    ///
    /// # Examples
    /// ```
    /// # extern crate tokio_trace_core as tokio_trace;
    /// use tokio_trace::field::{self, Value, RecordResult};
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
    /// impl Value for Foo {
    ///     fn record(&self, recorder: &mut dyn field::Record) -> RecordResult {
    ///         recorder.record_str("foo")
    ///     }
    /// }
    ///
    /// let foo = Foo;
    /// assert_eq!("Foo".to_owned(), format!("{:?}", foo));
    ///
    /// let display_foo = Value::display(foo.clone());
    /// assert_eq!(
    ///     format!("{}", foo),
    ///     format!("{:?}", &display_foo),
    /// );
    /// # }
    /// ```
    pub fn display<'a, T>(t: T) -> DisplayValue<T>
    where
        T: fmt::Display,
    {
        DisplayValue(t)
    }

    /// Wraps a type implementing `fmt::Debug` as a `Value` that can be
    /// serialized using its `Debug` implementation.
    pub fn debug<T>(t: T) -> DebugValue<T>
    where
        T: fmt::Debug,
    {
        DebugValue(t)
    }
}

// ===== impl DisplayValue =====

impl<T: fmt::Display> ::sealed::Sealed for DisplayValue<T> {}

impl<T: fmt::Display> Value for DisplayValue<T> {
    fn add_value(&self, span: &span::Id, key: &Key, dispatch: &Dispatch) -> Result<(), ::subscriber::AddValueError> {
        dispatch.add_value_fmt(span, key, format_args!("{}", self.0))
    }
}


// ===== impl DebugValue =====

impl<T: fmt::Debug> ::sealed::Sealed for DebugValue<T> {}

impl<T: fmt::Debug> Value for DebugValue<T> {
    fn add_value(&self, span: &span::Id, key: &Key, dispatch: &Dispatch) -> Result<(), ::subscriber::AddValueError> {
        dispatch.add_value_fmt(span, key, format_args!("{:?}", self.0))
    }
}


// ===== impl Field =====

impl<'a> Key<'a> {
    pub(crate) fn new(i: usize, metadata: &'a Meta<'a>) -> Self {
        Self { i, metadata }
    }

    pub(crate) fn metadata(&self) -> &Meta<'a> {
        self.metadata
    }

    /// Return a `usize` representing the index into an array whose indices are
    /// ordered the same as the set of fields that generated this `key`.
    pub fn as_usize(&self) -> usize {
        self.i
    }

    /// Returns a string representing the name of the field, or `None` if the
    /// field does not exist.
    pub fn name(&self) -> Option<&'a str> {
        self.metadata.field_names.get(self.i).map(|&n| n)
    }

    /// If `self` indexes the given `metadata`, returns a new key into that
    /// metadata. Otherwise, returns `None`.
    ///
    /// This is essentially just a trick to tell the compiler that the lifetine
    /// parameters of two references to a metadata are equal if they are the
    /// same metadata (which can't be inferred when dealing with metadata with
    /// generic lifetimes).
    #[inline]
    pub fn with_metadata<'b>(&self, metadata: &'b Meta<'b>) -> Option<Key<'b>> {
        if self.metadata == metadata {
            Some(Key {
                i: self.i,
                metadata,
            })
        } else {
            None
        }
    }
}

impl<'a> fmt::Display for Key<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.pad(self.name().unwrap_or("???"))
    }
}

impl<'a> AsRef<str> for Key<'a> {
    fn as_ref(&self) -> &str {
        self.name().unwrap_or("???")
    }
}

impl ::sealed::Sealed for str {}

impl Value for str {
    fn add_value(&self, span: &span::Id, key: &Key, dispatch: &Dispatch) -> Result<(), ::subscriber::AddValueError> {
        dispatch.add_value_str(span, key, self)
    }
}

impl ::sealed::Sealed for bool {}

impl Value for bool {
    fn add_value(&self, span: &span::Id, key: &Key, dispatch: &Dispatch) -> Result<(), ::subscriber::AddValueError> {
        dispatch.add_value_bool(span, key, self)
    }
}

impl ::sealed::Sealed for i64 {}

impl Value for i64 {
    fn add_value(&self, span: &span::Id, key: &Key, dispatch: &Dispatch) -> Result<(), ::subscriber::AddValueError> {
        dispatch.add_value_i64(span, key, self)
    }
}

impl ::sealed::Sealed for u64 {}

impl Value for u64 {
    fn add_value(&self, span: &span::Id, key: &Key, dispatch: &Dispatch) -> Result<(), ::subscriber::AddValueError> {
        dispatch.add_value_u64(span, key, self)
    }
}
