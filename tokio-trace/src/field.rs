use std::{fmt, io};
pub use tokio_trace_core::field::*;
use Meta;

/// Trait implemented to allow a type to be used as a field key.
///
/// **Note**: Although this is implemented for both the [`Key`] type *and* any
/// type that can be borrowed as an `&str`, only `Key` allows _O_(1) access.
/// Indexing a field with a string results in an iterative search that performs
/// string comparisons. Thus, if possible, once the key for a field is known, it
/// should be used whenever possible.
pub trait AsKey {
    /// Attempts to convert `&self` into a `Key` with the specified `metadata`.
    ///
    /// If `metadata` defines a key corresponding to this field, then the key is
    /// returned. Otherwise, this function returns `None`.
    fn as_key<'a>(&self, metadata: &'a Meta<'a>) -> Option<Key<'a>>;
}

pub struct DebugRecorder<W> {
    write: W,
}


// ===== impl DebugRecorder =====

impl<W> DebugRecorder<W> {
    pub fn into_inner(self) -> W {
        self.write
    }
}

impl<'a> DebugRecorder<&'a mut fmt::Write> {
    pub fn new_fmt<W: fmt::Write>(write: &'a mut W) -> Self {
        Self {
            write,
        }
    }
}

impl<'a> DebugRecorder<&'a mut io::Write> {
    pub fn new_io<W: io::Write>(write: &'a mut W) -> Self {
        Self {
            write,
        }
    }
}

impl<'a> Record for DebugRecorder<&'a mut dyn fmt::Write> {
    fn record_fmt(&mut self, key: &Key, args: fmt::Arguments) -> Result<(), ::subscriber::RecordError> {
        self.write.write_fmt(format_args!("{}=", key.name().unwrap_or("???")))?;
        self.write.write_fmt(args)?;
        Ok(())
    }
}

// ===== impl AsKey =====

impl<'f> AsKey for Key<'f> {
    #[inline]
    fn as_key<'a>(&self, metadata: &'a Meta<'a>) -> Option<Key<'a>> {
        self.with_metadata(metadata)
    }
}

impl<'f> AsKey for &'f Key<'f> {
    #[inline]
    fn as_key<'a>(&self, metadata: &'a Meta<'a>) -> Option<Key<'a>> {
        self.with_metadata(metadata)
    }
}

impl AsKey for str {
    #[inline]
    fn as_key<'a>(&self, metadata: &'a Meta<'a>) -> Option<Key<'a>> {
        metadata.key_for(&self)
    }
}
