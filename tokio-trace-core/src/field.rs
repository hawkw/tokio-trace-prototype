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
//! An object-safe API, [`Recorder`](Recorder), is provided for consuming
//! `Value`s. A `Recorder` may implement a set of functions that record various
//! Rust primitive types, allowing user-defined behaviours to be implemented for
//! numbers, boolean values, strings, collections, and so on. `Value`s are
//! required to implement the `record` function, which is passed a mutable
//! reference to a `Recorder` trait object. The `Value` may choose which of the
//! `Recorder`'s `record` methods for various types it wishes to call, allowing
//! it to present typed data to the `Recorder`.
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
use super::Meta;
use std::{
    any::TypeId,
    cell::Cell,
    collections,
    error,
    fmt,
    hash::Hash,
    io,
};

pub type RecordResult = Result<(), Error>;

pub trait Recorder {
    // fn named<'b: 'a>(&'b mut self, name: &'b str) -> &'b dyn Recorder<'b>;
    fn record_uint(&mut self, value: u64) -> RecordResult {
        self.record_any(&value)
    }

    fn record_int(&mut self, value: i64) -> RecordResult {
        self.record_any(&value)
    }

    fn record_float(&mut self, value: f64) -> RecordResult {
        self.record_any(&value)
    }

    fn record_str(&mut self, value: &str) -> RecordResult {
        self.record_any(&value)
    }

    fn record_byte(&mut self, value: u8) -> RecordResult {
        self.record_any(&value)
    }

    fn record_bool(&mut self, value: bool) -> RecordResult {
        self.record_any(&value)
    }

    fn record_any(&mut self, value: &dyn Value) -> RecordResult {
        self.record_fmt(format_args!("{:?}", value))
    }

    fn record_kv(&mut self, k: &dyn Value, v: &dyn Value) -> RecordResult;
    fn record_fmt(&mut self, args: fmt::Arguments) -> RecordResult;

    fn open_map(&mut self) -> RecordResult;
    fn close_map(&mut self) -> RecordResult;

    fn open_list(&mut self) -> RecordResult;
    fn close_list(&mut self) -> RecordResult;

    fn open_struct(&mut self, name: &str) -> RecordResult;
    fn close_struct(&mut self) -> RecordResult;

    fn open_tuple(&mut self) -> RecordResult;
    fn close_tuple(&mut self) -> RecordResult;

    fn finish(self) -> RecordResult;
}

impl<'r> Recorder + 'r {
    pub fn record_map<'a, I, K, V>(&mut self, i: I) -> RecordResult
    where
        I: IntoIterator<Item = (&'a K, &'a V)>,
        K: Value + 'a,
        V: Value + 'a,
    {
        self.open_map()?;
        for (k, v) in i {
            self.record_kv(k, v)?;
        }
        self.close_map()
    }

    pub fn record_list<'a, I, V>(&mut self, i: I) -> RecordResult
    where
        I: IntoIterator<Item = &'a V>,
        V: Value + 'a,
    {
        self.open_list()?;
        for v in i {
            v.record(self)?;
        }
        self.close_list()
    }

    pub fn record_struct<'a, I, V>(&mut self, name: &str, i: I) -> RecordResult
    where
        I: IntoIterator<Item = (&'a str, &'a V)>,
        V: Value + 'a,
    {
        self.open_struct(name)?;
        for (name, v) in i {
            self.record_kv(&name, v)?;
        }
        self.close_struct()
    }

    pub fn record_tuple<'a, I, V>(&mut self, i: I) -> RecordResult
    where
        I: IntoIterator<Item = &'a V>,
        V: Value + 'a,
    {
        self.open_tuple()?;
        for v in i {
            v.record(self)?;
        }
        self.close_tuple()
    }

}

/// A formattable field value of an erased type.
pub trait Value: fmt::Debug + Send + Sync {
    /// Records the value with the given `Recorder`.
    fn record(&self, &mut dyn Recorder) -> RecordResult;

    #[doc(hidden)]
    fn type_id(&self) -> TypeId
    where
        Self: 'static,
    {
        TypeId::of::<Self>()
    }
}

#[derive(Debug)]
pub struct Error {
    // TODO: Should this carry a boxed error, or should it just discard the
    // cause and be opaque?
    inner: Box<dyn error::Error>,
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
    ///     fn record(&self, recorder: &mut dyn field::Recorder) -> RecordResult {
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
    ///
    /// ```
    /// # extern crate tokio_trace_core as tokio_trace;
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
    /// # impl Value for Foo {
    /// #   fn record(&self, recorder: &mut dyn tokio_trace::field::Recorder)
    /// #       -> tokio_trace::field::RecordResult
    /// #   {
    /// #       recorder.record_str("foo")
    /// #   }
    /// # }
    /// use tokio_trace::field::Value;
    /// let foo = Value::display(Foo);
    ///
    /// assert_eq!("Hello, I'm Foo".to_owned(), format!("{:?}", foo));
    ///
    /// assert!(Value::downcast_ref::<Foo>(&foo).is_some());
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

// Copied from `std::any::Any`.
impl Value + 'static {
    /// Returns true if the boxed type is the same as `T`
    #[inline]
    pub fn is<T: Value + 'static>(&self) -> bool
    where
        Self: 'static,
    {
        // Get TypeId of the type this function is instantiated with
        let t = TypeId::of::<T>();

        // Get TypeId of the type in the trait object
        let boxed = self.type_id();

        // Compare both TypeIds on equality
        t == boxed
    }

    /// Returns some reference to the boxed value if it is of type `T`, or
    /// `None` if it isn't.
    pub fn downcast_ref<T: Value + 'static>(&self) -> Option<&T>
    where
        Self: 'static,
    {
        if self.is::<T>() {
            unsafe { Some(&*(self as *const Value as *const T)) }
        } else {
            None
        }
    }
}

// ===== impl DisplayValue =====

impl<T: fmt::Display + Send + Sync> Value for DisplayValue<T> {
    fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
        recorder.record_fmt(format_args!("{}", self.0))
    }

    #[doc(hidden)]
    fn type_id(&self) -> TypeId
    where
        Self: 'static,
    {
        TypeId::of::<T>()
    }
}

impl<T: fmt::Display> fmt::Debug for DisplayValue<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// ===== impl DebugValue =====

impl<T: fmt::Debug + Send + Sync> Value for DebugValue<T> {
    fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
        recorder.record_fmt(format_args!("{:?}", self.0))
    }

    #[doc(hidden)]
    fn type_id(&self) -> TypeId
    where
        Self: 'static,
    {
        TypeId::of::<T>()
    }
}

impl<T: fmt::Debug> fmt::Debug for DebugValue<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
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

macro_rules! impl_values {
    ( $( $record:ident( $( $whatever:tt)+ ) ),+ ) => {
        $(
            impl_value!{ $record( $( $whatever )+ ) }
        )+
    }
}
macro_rules! impl_value {
    ( $record:ident( $( $value_ty:ty ),+ ) ) => {
        $(
            impl Value for $value_ty {
                fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
                    recorder.$record(*self)
                }
            }
        )+
    };
    ( $record:ident( $( $value_ty:ty ),+ as $as_ty:ty) ) => {
        $(
            impl Value for $value_ty {
                fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
                    recorder.$record(*self as $as_ty)
                }
            }
        )+
    };
}

impl_values! {
    record_byte(u8),
    record_uint(u64),
    record_uint(usize, u32, u16 as u64),
    record_int(i64),
    record_int(isize, i32, i16, i8 as i64),
    record_float(f64, f32 as f64),
    record_bool(bool)
}

impl<'a> Value for &'a str {
    fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
        recorder.record_str(self)
    }
}

impl<'a> Value for Key<'a> {
    fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
        recorder.record_str(self.name().unwrap_or("???"))
    }
}

impl<T> Value for [T]
where
    T: Value,
{
    fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
        recorder.record_list(self.iter())
    }
}

impl<T> Value for Vec<T>
where
    T: Value,
{
    fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
        self.as_slice().record(recorder)
    }
}

impl<K, V> Value for collections::HashMap<K, V>
where
    K: Value + Hash + Eq,
    V: Value,
{
    fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
        recorder.record_map(self.iter())
    }
}

impl<T> Value for collections::HashSet<T>
where
    T: Value + Hash + Eq,
{
    fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
        recorder.record_tuple(self.iter())
    }
}

impl<K, V> Value for collections::BTreeMap<K, V>
where
    K: Value + Eq,
    V: Value,
{
    fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
        recorder.record_map(self.iter())
    }
}

impl<T> Value for collections::BTreeSet<T>
where
    T: Value + Eq,
{
    fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
        recorder.record_tuple(self.iter())
    }
}

impl<T> Value for collections::LinkedList<T>
where
    T: Value,
{
    fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
        recorder.record_list(self.iter())
    }
}

impl<T> Value for collections::VecDeque<T>
where
    T: Value,
{
    fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
        recorder.record_list(self.iter())
    }
}

impl<T> Value for collections::BinaryHeap<T>
where
    T: Value + Ord,
{
    fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
        // NOTE: the values will *not* be visited in order --- is that something
        // we want to guarantee?
        recorder.record_list(self.iter())
    }
}

impl<'a, T> Value for &'a T
where
    T: Value + 'a,
{
    fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
        self.record(recorder)
    }
}

// ===== impl Error =====

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&dyn error::Error> {
        self.inner.cause()
    }
}

impl From<fmt::Error> for Error {
    fn from(inner: fmt::Error) -> Self {
        // TODO: since `fmt::Error` is empty, it would be nice if we didn't
        // have to box this.
        Self::new(inner)
    }
}

impl From<io::Error> for Error {
    fn from(inner: io::Error) -> Self {
        Self::new(inner)
    }
}

impl Error {
    pub fn new<E>(inner: E) -> Self
    where
        E: error::Error + 'static,
    {
        Self {
            inner: Box::new(inner)
        }
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

    impl Value for Foo {
        fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
            let mut fields = ::std::iter::once::<(&str, &dyn Value)>(("bar", &self.bar));
            recorder.record_struct(&"Foo", &mut fields)
        }
    }

    #[test]
    fn display_value_formats_with_display() {
        let foo = Foo { bar: "foo" };
        let display_foo = Value::display(foo.clone());

        assert_eq!(format!("{:?}", foo), "Foo { bar: \"foo\" }".to_owned());
        assert_eq!(format!("{:?}", display_foo), format!("{}", foo));
    }

    #[test]
    fn display_value_is_value() {
        let foo = Foo { bar: "foo" };
        let display_foo = Value::display(foo.clone());

        let value: &dyn Value = &display_foo;
        assert_eq!(format!("{:?}", value), format!("{}", foo));
    }

    #[test]
    fn display_value_downcasts_to_original_type() {
        let foo = Foo { bar: "foo" };
        let display_foo = Value::display(foo);
        let value: &dyn Value = &display_foo;

        assert!(value.downcast_ref::<Foo>().is_some());
    }
}
