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

pub struct DebugWriter<W> {
    write: W,
    comma_delimited: usize,
    add_comma: bool,
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

// ===== impl DebugWriter =====

impl<W> DebugWriter<W> {
    pub fn into_inner(self) -> W {
        self.write
    }
}

impl<'a> DebugWriter<&'a mut fmt::Write> {
    pub fn new_fmt<W: fmt::Write>(write: &'a mut W) -> Self {
        Self {
            write,
            comma_delimited: 0,
            add_comma: false,
        }
    }

    fn fmt_maybe_comma(&mut self) -> fmt::Result {
        if self.comma_delimited > 0 {
            self.write.write_str(", ")?;
        } else if self.add_comma {
            self.comma_delimited += 1;
            self.add_comma = false;
        }
        Ok(())
    }

    fn fmt_open(&mut self, args: fmt::Arguments) -> RecordResult {
        self.fmt_maybe_comma()?;
        self.write.write_fmt(args)?;
        self.add_comma = true;
        Ok(())
    }

    fn fmt_close(&mut self, args: fmt::Arguments) -> RecordResult {
        self.write.write_fmt(args)?;
        self.comma_delimited.saturating_sub(1);
        Ok(())
    }
}

impl<'a> DebugWriter<&'a mut io::Write> {
    pub fn new_io<W: io::Write>(write: &'a mut W) -> Self {
        Self {
            write,
            comma_delimited: 0,
            add_comma: false,
        }
    }

    fn io_maybe_comma(&mut self) -> io::Result<()> {
        if self.comma_delimited > 0 {
            self.write.write_all(b", ")?;
        } else if self.add_comma {
            self.comma_delimited += 1;
            self.add_comma = false;
        }
        Ok(())
    }

    fn io_open(&mut self, args: fmt::Arguments) -> RecordResult {
        self.io_maybe_comma()?;
        self.write.write_fmt(args)?;
        self.add_comma = true;
        Ok(())
    }

    fn io_close(&mut self, args: fmt::Arguments) -> RecordResult {
        self.write.write_fmt(args)?;
        self.comma_delimited.saturating_sub(1);
        Ok(())
    }
}

impl<'a> Record for DebugWriter<&'a mut dyn io::Write> {
    fn record_fmt(&mut self, args: fmt::Arguments) -> RecordResult {
        self.io_maybe_comma()?;
        self.write.write_fmt(args)?;
        Ok(())
    }

    fn open_map(&mut self) -> RecordResult {
        self.io_open(format_args!("{{"))
    }

    fn close_map(&mut self) -> RecordResult {
        self.io_close(format_args!("}}"))
    }

    fn open_list(&mut self) -> RecordResult {
        self.io_open(format_args!("["))
    }

    fn close_list(&mut self) -> RecordResult {
        self.io_open(format_args!("]"))
    }

    fn open_struct(&mut self, name: &str) -> RecordResult {
        self.io_open(format_args!("{} {{", name))
    }

    fn close_struct(&mut self) -> RecordResult {
        self.io_close(format_args!("}}"))
    }

    fn open_tuple(&mut self) -> RecordResult {
        self.io_open(format_args!("["))
    }

    fn close_tuple(&mut self) -> RecordResult {
        self.io_open(format_args!("]"))
    }

    fn record_kv(&mut self, k: &dyn Value, v: &dyn Value) -> RecordResult {
        self.io_maybe_comma()?;
        k.record(self)?;
        self.write.write_all(b": ")?;
        v.record(self)?;
        Ok(())
    }

    fn finish(self) -> RecordResult {
        self.write.flush()?;
        Ok(())
    }
}

impl<'a> Record for DebugWriter<&'a mut dyn fmt::Write> {
    fn record_fmt(&mut self, args: fmt::Arguments) -> RecordResult {
        self.fmt_maybe_comma()?;
        self.write.write_fmt(args)?;
        Ok(())
    }

    fn open_map(&mut self) -> RecordResult {
        self.fmt_open(format_args!("{{"))
    }

    fn close_map(&mut self) -> RecordResult {
        self.fmt_close(format_args!("}}"))
    }

    fn open_list(&mut self) -> RecordResult {
        self.fmt_open(format_args!("["))
    }

    fn close_list(&mut self) -> RecordResult {
        self.fmt_open(format_args!("]"))
    }

    fn open_struct(&mut self, name: &str) -> RecordResult {
        self.fmt_open(format_args!("{} {{", name))
    }

    fn close_struct(&mut self) -> RecordResult {
        self.fmt_close(format_args!("}}"))
    }

    fn open_tuple(&mut self) -> RecordResult {
        self.fmt_open(format_args!("["))
    }

    fn close_tuple(&mut self) -> RecordResult {
        self.fmt_open(format_args!("]"))
    }

    fn record_kv(&mut self, k: &dyn Value, v: &dyn Value) -> RecordResult {
        self.fmt_maybe_comma()?;
        k.record(self)?;
        self.write.write_str(": ")?;
        v.record(self)?;
        Ok(())
    }

    fn finish(self) -> RecordResult {
        Ok(())
    }
}
