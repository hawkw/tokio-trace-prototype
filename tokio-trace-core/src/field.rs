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
//! types may be attached to spans or events, provided they meet certain
//! requirements.
//!
//! Typically, we might accept arbitrarily-typed values by making the
//! `Subscriber` APIs that accept them generic. However, as the `Dispatch` type
//! holds a subscriber as a boxed trait object, the `Subscriber` trait must be
//! object-safe --- it cannot have trait methods that accept a generic
//! parameter. Thus, we erase the value's original type.
//!
//! However, a `Value` which is valid for the `'static` lifetime may also be
//! _downcast_ back to a concrete type, similarly to `std::error::Error` and
//! `std::any::Any`. If the erased type of the value is known, downcasting
//! allows it to be used as an instance of that type.
//!
//! # `Value`s and `Subscriber`s
//!
//! `Subscriber`s consume `Value`s as fields attached to `Event`s or `Span`s.
//! These cases are handled somewhat differently.
//!
//! When a field is attached to an `Event`, the `Subscriber::observe_event`
//! method is passed an `Event` struct which provides an iterator
//! (`Event::fields`) to iterate over the event's fields, providing references
//! to the values as `BorrowedValue`s. Since an `Event` represents a _moment_ in
//! time, it   does not expect to outlive the scope that created it. Thus, the
//! values attached to an `Event` are _borrowed_ from the scope where the
//! `Event` originated.
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
//! `&dyn AsValue` isn't sufficient. Instead, if a subscriber wishes to persist a
//! span value for the entire lifetime of the span, it needs the ability to
//! convert the value into a form in which it is owned by the _subscriber_,
//! rather than the scope in which it was added to the span. For this reason,
//! span values are passed as `&dyn field::Value`. The `IntoValue` trait is an
//! extension of the `Value` trait that allows conversion into an `OwnedValue`,
//! a type which represents an owned value allocated on the heap. Since some
//! subscriber implementations may _not_ need to persist span field values
//! indefinitely, they are not heap-allocated by default, to avoid unnecessary
//! allocations, but the `IntoValue` trait presents `Subscriber`s with the
//! _option_ to box values should they need to do so.
use super::Meta;
use std::{
    any::TypeId,
    cell::Cell,
    error::Error,
    fmt,
    io::{self, Write},
};

pub type RecordResult = Result<(), Box<dyn Error>>;

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

impl Recorder {
    pub fn record_map<'a, I>(&mut self, i: I) -> RecordResult
    where I: IntoIterator<Item = (&'a dyn Value, &'a dyn Value)> {
        self.open_map()?;
        for (k, v) in i {
            self.record_kv(k, v)?;
        }
        self.close_map()
    }

    pub fn record_list<'a, I>(&mut self, i: I) -> RecordResult
    where I: IntoIterator<Item = &'a dyn Value> {
        self.open_map()?;
        for v in i {
            v.record(self)?;
        }
        self.close_map()
    }

    pub fn record_struct<'a, I>(&mut self, name: &str, i: I) -> RecordResult
    where I: IntoIterator<Item = (&'a str, &'a dyn Value)>  {
        self.open_struct(name)?;
        for (name, v) in i {
            self.record_kv(&name, v)?;
        }
        self.close_struct()
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

pub struct DebugRecorder<W> {
    write: W,
    comma_delimited: usize,
    add_comma: bool,
}

impl<W> DebugRecorder<W>
where
    W: Write,
{
    pub fn new(write: W) -> Self {
        Self {
            write,
            comma_delimited: 0,
            add_comma: false,
        }
    }

    fn maybe_comma(&mut self) -> io::Result<()> {
        if self.comma_delimited > 0 {
            self.write.write_all(b", ")?;
        } else if self.add_comma {
            self.comma_delimited += 1;
            self.add_comma = false;
        }
        Ok(())
    }

    fn open(&mut self, args: fmt::Arguments) -> RecordResult {
        self.maybe_comma()?;
        self.write.write_fmt(args)?;
        self.add_comma = true;
        Ok(())
    }

    fn close(&mut self, args: fmt::Arguments) -> RecordResult {
        self.write.write_fmt(args)?;
        self.comma_delimited.saturating_sub(1);
        Ok(())
    }

}

impl<W> Recorder for DebugRecorder<W>
where
    W: Write,
{
    fn record_fmt(&mut self, args: fmt::Arguments) -> RecordResult {
        self.maybe_comma()?;
        self.write.write_fmt(args)?;
        Ok(())
    }

    fn open_map(&mut self) -> RecordResult {
        self.open(format_args!("{{"))
    }

    fn close_map(&mut self) -> RecordResult {
        self.close(format_args!("}}"))
    }

    fn open_list(&mut self) -> RecordResult {
        self.open(format_args!("["))
    }

    fn close_list(&mut self) -> RecordResult {
        self.open(format_args!("]"))
    }

    fn open_struct(&mut self, name: &str) -> RecordResult {
        self.open(format_args!("{} {{", name))
    }

    fn close_struct(&mut self) -> RecordResult {
        self.close(format_args!("}}"))
    }

    fn open_tuple(&mut self) -> RecordResult {
        self.open(format_args!("["))
    }

    fn close_tuple(&mut self) -> RecordResult {
        self.open(format_args!("]"))
    }

    fn record_kv(&mut self, k: &dyn Value, v: &dyn Value) -> RecordResult {
        self.maybe_comma()?;
        k.record(self)?;
        self.write.write_all(b": ")?;
        v.record(self)?;
        Ok(())
    }

    fn finish(mut self) -> RecordResult {
        self.write.flush()?;
        Ok(())
    }
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
    /// use tokio_trace::field;
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
    /// let display_foo = field::display(foo.clone());
    /// assert_eq!(
    ///     format!("{}", foo),
    ///     format!("{:?}", field::borrowed(&display_foo)),
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
    /// use tokio_trace::field::{self, Value, IntoValue};
    /// let foo = field::display(Foo);
    ///
    /// let owned_value = foo.into_value();
    /// assert_eq!("Hello, I'm Foo".to_owned(), format!("{:?}", owned_value));
    ///
    /// assert!(owned_value.downcast_ref::<Foo>().is_some());
    /// # }
    /// ```
    pub fn display<T>(t: T) -> DisplayValue<T>
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
    fn is<T: Value + 'static>(&self) -> bool
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
    fn downcast_ref<T: Value + 'static>(&self) -> Option<&T>
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
        let display_foo = display(foo.clone());

        assert_eq!(
            format!("{:?}", foo),
            "Foo { bar: \"foo\" }".to_owned()
        );
        assert_eq!(
            format!("{:?}", display_foo),
            format!("{}", foo)
        );
    }

    #[test]
    fn display_value_is_value() {
        let foo = Foo { bar: "foo" };
        let display_foo = display(foo.clone());

        let value: &dyn Value = &display_foo;
        assert_eq!(format!("{:?}", value), format!("{}", foo));
    }

    #[test]
    fn display_value_downcasts_to_original_type() {
        let foo = Foo { bar: "foo" };
        let display_foo = display(foo);
        let value: &dyn Value = &display_foo;

        assert!(value.downcast_ref::<Foo>().is_some());
    }
}
