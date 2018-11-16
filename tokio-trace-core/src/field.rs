//! `Span` and `Event` key-value data.
//!
//! Spans and events may be annotated with key-value data, referred to as known
//! as _fields_. These fields consist of a mapping
//!
//! # `Value`s and `Subscriber`s
//!
//! `Subscriber`s consume `Value`s as fields attached to `Event`s or `Span`s.
//! The set of field keys on a given `Span` or `Event` is defined on its
//! `Metadata`. Once the span or event has been created (i.e., the `new_id` or
//! `new_span` methods on the `Subscriber` have been called), field values may
//! be added by calls to the subscriber's `record_` methods.
//!
//! `tokio_trace` represents values as either one of a set of Rust primitives
//! (`i64`, `u64`, `bool`, and `&str`) or using a `fmt::Display` or `fmt::Debug`
//! implementation. The `record_` trait functions on the `Subscriber` trait allow
//! `Subscriber` implementations to provide type-specific behaviour for
//! consuming values of each type.
//!
//! The `Subscriber` trait provides default implementations of `record_u64`,
//! `record_i64`, `record_bool`, and `record_str` which call the `record_fmt`
//! function, so that only the `record_fmt` function must be implemented.
//! However, implementors of `Subscriber` that wish to consume these primitives
//! as their types may override the `record` methods for any types they care
//! about. For example, we might record integers by incrementing counters for
//! their field names, rather than printing them.
//
use std::fmt;
use {Id, Subscriber, Meta};

/// A field value of an erased type.
///
/// Implementors of `Value` may call the appropriate typed recording methods on
/// the `Subscriber` passed to `record` in order to indicate how their data
/// should be recorded.
pub trait Value: ::sealed::Sealed + Send {
    /// Records this value with the given `Subscriber`.
    fn record(
        &self,
        id: &Id,
        key: &Key,
        recorder: &dyn Subscriber,
    ) -> Result<(), ::subscriber::RecordError>;
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
    /// Wraps a type implementing `fmt::Display` as a `Value` that can be
    /// recorded using its `Display` implementation.
    pub fn display<'a, T>(t: T) -> DisplayValue<T>
    where
        T: fmt::Display,
    {
        DisplayValue(t)
    }

    /// Wraps a type implementing `fmt::Debug` as a `Value` that can be
    /// recorded using its `Debug` implementation.
    pub fn debug<T>(t: T) -> DebugValue<T>
    where
        T: fmt::Debug,
    {
        DebugValue(t)
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

// ===== impl DisplayValue =====

impl<T: fmt::Display> ::sealed::Sealed for DisplayValue<T> {}

impl<T> Value for DisplayValue<T>
where
    T: fmt::Display + Send,
{
    fn record(
        &self,
        id: &Id,
        key: &Key,
        recorder: &dyn Subscriber,
    ) -> Result<(), ::subscriber::RecordError> {
        recorder.record_fmt(id, key, format_args!("{}", self.0))
    }
}

// ===== impl Value =====

impl<T: fmt::Debug> ::sealed::Sealed for DebugValue<T> {}

impl<T: fmt::Debug> Value for DebugValue<T>
where
    T: fmt::Debug + Send,
{
    fn record(
        &self,
        id: &Id,
        key: &Key,
        recorder: &dyn Subscriber,
    ) -> Result<(), ::subscriber::RecordError> {
        recorder.record_fmt(id, key, format_args!("{:?}", self.0))
    }
}

impl<'a> ::sealed::Sealed for &'a str {}

impl<'a> Value for &'a str {
    fn record(
        &self,
        id: &Id,
        key: &Key,
        recorder: &dyn Subscriber,
    ) -> Result<(), ::subscriber::RecordError> {
        recorder.record_str(id, key, self)
    }
}

impl ::sealed::Sealed for bool {}

impl Value for bool {
    fn record(
        &self,
        id: &Id,
        key: &Key,
        recorder: &dyn Subscriber,
    ) -> Result<(), ::subscriber::RecordError> {
        recorder.record_bool(id, key, *self)
    }
}

impl ::sealed::Sealed for i64 {}

impl Value for i64 {
    fn record(
        &self,
        id: &Id,
        key: &Key,
        recorder: &dyn Subscriber,
    ) -> Result<(), ::subscriber::RecordError> {
        recorder.record_i64(id, key, *self)
    }
}

impl ::sealed::Sealed for u64 {}

impl Value for u64 {
    fn record(
        &self,
        id: &Id,
        key: &Key,
        recorder: &dyn Subscriber,
    ) -> Result<(), ::subscriber::RecordError> {
        recorder.record_u64(id, key, *self)
    }
}

impl<'a, V> ::sealed::Sealed for &'a V where V: Value + ::sealed::Sealed + Send + Sync {}

impl<'a, V> Value for &'a V
where
    V: Value + ::sealed::Sealed + Send + Sync,
{
    fn record(
        &self,
        id: &Id,
        key: &Key,
        recorder: &dyn Subscriber,
    ) -> Result<(), ::subscriber::RecordError> {
        (*self).record(id, key, recorder)
    }
}
