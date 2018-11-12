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
//! span values are passed as `&dyn IntoValue`. The `IntoValue` trait is an
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

    fn record_any(&mut self, value: &dyn Value) -> RecordResult;

    fn record_tuple(&mut self, tuple: (&dyn Value, &dyn Value)) -> RecordResult;

    fn record_map(
        &mut self,
        map: &mut dyn Iterator<Item = (&dyn Value, &dyn Value)>,
    ) -> RecordResult;

    fn record_list(&mut self, list: &mut dyn Iterator<Item = &dyn Value>) -> RecordResult;

    fn record_struct(
        &mut self,
        name: &str,
        strct: &mut dyn Iterator<Item = (&str, &dyn Value)>,
    ) -> RecordResult;
    fn finish(self) -> RecordResult;
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

pub struct DebugWriter<W>(W);

impl<W> Recorder for DebugWriter<W>
where
    W: Write,
{
    fn record_tuple(&mut self, tuple: (&dyn Value, &dyn Value)) -> RecordResult {
        // self.record_any(&tuple)
        unimplemented!()
    }

    fn record_any(&mut self, value: &dyn Value) -> RecordResult {
        write!(&mut self.0, "{:?}", value).map_err(Into::into)
    }

    fn record_map(
        &mut self,
        map: &mut dyn Iterator<Item = (&dyn Value, &dyn Value)>,
    ) -> RecordResult {
        // TODO: this is...not great.
        struct Debug<'a, A: 'a, B: ?Sized + 'a>(Cell<Option<&'a mut dyn Iterator<Item = (A, B)>>>);
        impl<'a, A: fmt::Debug + 'a, B: fmt::Debug + 'a> fmt::Debug for Debug<'a, A, B> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_map().entries(self.0.replace(None).expect("should not be formatted twice")).finish()
            }
        }
        write!(self.0, "{:?}", Debug(Cell::new(Some(map)))).map_err(Into::into)
    }

    fn record_list(&mut self, list: &mut dyn Iterator<Item = &dyn Value>) -> RecordResult {
        struct Debug<'a, T: ?Sized + 'a>(Cell<Option<&'a mut dyn Iterator<Item = T>>>);
        impl<'a, T: fmt::Debug + 'a> fmt::Debug for Debug<'a, T> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_list().entries(self.0.replace(None).expect("should not be formatted twice")).finish()
            }
        }
        write!(self.0, "{:?}", Debug(Cell::new(Some(list)))).map_err(Into::into)
    }

    fn record_struct(
        &mut self,
        name: &str,
        fields: &mut dyn Iterator<Item = (&str, &(dyn Value))>,
    ) -> RecordResult {
        struct Debug<'a, 'b, 'c: 'a, T: ?Sized + 'a> {
            name: &'b str,
            fields: Cell<Option<&'a mut dyn Iterator<Item = (&'c str, T)>>>,
        }
        impl<'a, 'b, 'c: 'a, T: fmt::Debug + 'a> fmt::Debug for Debug<'a, 'b, 'c, T> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let mut debug = f.debug_struct(self.name.as_ref());
                self.fields.replace(None).expect("should not be formatted twice")
                    .fold(
                        &mut debug,
                        |debug, (name, value)| debug.field(name.as_ref(), &value),
                    );
                debug.finish()
            }
        }
        let fields = Cell::new(Some(fields));
        write!(self.0, "{:?}", Debug { name, fields
        }).map_err(From::from)
    }

    fn finish(mut self) -> RecordResult {
        self.0.flush().map_err(From::from)
    }
}

impl<W: Write> DebugWriter<W> {
    pub fn new_named(mut writer: W, field: &Key) -> Result<Self, io::Error> {
        write!(&mut writer, "{}: ", field.name().unwrap_or("???"))?;
        Ok(DebugWriter(writer))
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
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Key<'a> {
    i: usize,
    metadata: &'a Meta<'a>,
}

/// A `Value` which is formatted using `fmt::Display` rather than `fmt::Debug`.
#[derive(Clone)]
pub struct DisplayValue<T: fmt::Display>(T);

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
    T: Value + fmt::Display,
{
    DisplayValue(t)
}

// ===== impl AsValue =====

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

impl<T: Value + fmt::Display> Value for DisplayValue<T> {
    fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
        self.0.record(recorder)
    }

    #[doc(hidden)]
    fn type_id(&self) -> TypeId
    where
        Self: 'static,
    {
        self.0.type_id()
    }
}

impl<T: Value + fmt::Display> fmt::Debug for DisplayValue<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
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
    record_float(f64, f32 as f64)
}

impl<'a> Value for &'a str {
    fn record(&self, recorder: &mut dyn Recorder) -> RecordResult {
        recorder.record_str(self)
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
