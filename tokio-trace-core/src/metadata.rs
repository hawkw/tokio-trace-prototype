use super::{callsite, field, Level};
use std::{
    borrow::Borrow,
};

/// Metadata describing a [`Span`] or [`Event`].
///
/// This includes the source code location where the span or event occurred, the
/// names of its fields, et cetera.
///
/// Metadata is used by [`Subscriber`]s when filtering spans and events, and it
/// may also be used as part of their data payload.
///
/// When created by the `event!` or `span!` macro, the metadata describing a
/// particular event or span is constructed statically and exists as a single
/// static instance. Thus, the overhead of  creating the metadata is
/// _significantly_ lower than that of creating the actual span or event.
/// Therefore, filtering is based on metadata, rather than  on the constructed
/// span or event.
///
/// **Note**: The implementation of `PartialEq` for `Meta` is address-based,
/// *not* structural equality. This is intended as a performance optimization,
/// as metadata are intended to be created statically once per callsite.
/// [`Span`]: ::span::Span
/// [`Event`]: ::Event
/// [`Subscriber`]: ::Subscriber
#[derive(Clone)]
pub struct Meta<'a> {
    /// If this metadata describes a span, the name of the span.
    ///
    /// **Warning**: The fields on this type are currently `pub` because it must be able
    /// to be constructed statically by macros. However, when `const fn`s are
    /// available on stable Rust, this will no longer be necessary. Thus, these
    /// fields are *not* considered stable public API, and they may change
    /// warning. Do not rely on any fields on `Meta`. When constructing new
    /// `Meta`s, use the `metadata!` macro or the `Meta::new_span` and
    /// `Meta::new_event` constructors instead!
    #[doc(hidden)]
    pub name: Option<&'a str>,

    /// The part of the system that the span or event that this metadata
    /// describes occurred in.
    ///
    /// Typically, this is the module path, but alternate targets may be set
    /// when spans or events are constructed.
    ///
    /// **Warning**: The fields on this type are currently `pub` because it must be able
    /// to be constructed statically by macros. However, when `const fn`s are
    /// available on stable Rust, this will no longer be necessary. Thus, these
    /// fields are *not* considered stable public API, and they may change
    /// warning. Do not rely on any fields on `Meta`. When constructing new
    /// `Meta`s, use the `metadata!` macro or the `Meta::new_span` and
    /// `Meta::new_event` constructors instead!
    #[doc(hidden)]
    pub target: &'a str,

    /// The level of verbosity of the described span or event.
    ///
    /// **Warning**: The fields on this type are currently `pub` because it must be able
    /// to be constructed statically by macros. However, when `const fn`s are
    /// available on stable Rust, this will no longer be necessary. Thus, these
    /// fields are *not* considered stable public API, and they may change
    /// warning. Do not rely on any fields on `Meta`. When constructing new
    /// `Meta`s, use the `metadata!` macro or the `Meta::new_span` and
    /// `Meta::new_event` constructors instead!
    #[doc(hidden)]
    pub level: Level,

    /// The name of the Rust module where the span or event occurred, or `None`
    /// if this could not be determined.
    ///
    /// **Warning**: The fields on this type are currently `pub` because it must be able
    /// to be constructed statically by macros. However, when `const fn`s are
    /// available on stable Rust, this will no longer be necessary. Thus, these
    /// fields are *not* considered stable public API, and they may change
    /// warning. Do not rely on any fields on `Meta`. When constructing new
    /// `Meta`s, use the `metadata!` macro or the `Meta::new_span` and
    /// `Meta::new_event` constructors instead!
    #[doc(hidden)]
    pub module_path: Option<&'a str>,

    /// The name of the source code file where the span or event occurred, or
    /// `None` if this could not be determined.
    ///
    /// **Warning**: The fields on this type are currently `pub` because it must be able
    /// to be constructed statically by macros. However, when `const fn`s are
    /// available on stable Rust, this will no longer be necessary. Thus, these
    /// fields are *not* considered stable public API, and they may change
    /// warning. Do not rely on any fields on `Meta`. When constructing new
    /// `Meta`s, use the `metadata!` macro or the `Meta::new_span` and
    /// `Meta::new_event` constructors instead!
    #[doc(hidden)]
    pub file: Option<&'a str>,

    /// The line number in the source code file where the span or event
    /// occurred, or `None` if this could not be determined.
    ///
    /// **Warning**: The fields on this type are currently `pub` because it must be able
    /// to be constructed statically by macros. However, when `const fn`s are
    /// available on stable Rust, this will no longer be necessary. Thus, these
    /// fields are *not* considered stable public API, and they may change
    /// warning. Do not rely on any fields on `Meta`. When constructing new
    /// `Meta`s, use the `metadata!` macro or the `Meta::new_span` and
    /// `Meta::new_event` constructors instead!
    #[doc(hidden)]
    pub line: Option<u32>,

    /// The names of the key-value fields attached to the described span or
    /// event.
    ///
    /// **Warning**: The fields on this type are currently `pub` because it must be able
    /// to be constructed statically by macros. However, when `const fn`s are
    /// available on stable Rust, this will no longer be necessary. Thus, these
    /// fields are *not* considered stable public API, and they may change
    /// warning. Do not rely on any fields on `Meta`. When constructing new
    ///
    /// `Meta`s, use the `metadata!` macro or the `Meta::new_span` and
    /// `Meta::new_event` constructors instead!
    #[doc(hidden)]
    pub field_names: &'static [&'static str],

    /// A reference to the callsite that produced this metadata.
    ///
    /// This is used for implementing meta equality.
    ///
    /// **Warning**: The fields on this type are currently `pub` because it must be able
    /// to be constructed statically by macros. However, when `const fn`s are
    /// available on stable Rust, this will no longer be necessary. Thus, these
    /// fields are *not* considered stable public API, and they may change
    /// warning. Do not rely on any fields on `Meta`. When constructing new
    /// `Meta`s, use the `metadata!` macro or the `Meta::new_span` and
    /// `Meta::new_event` constructors instead!
    #[doc(hidden)]
    pub callsite: &'static callsite::Callsite,

    /// Whether this metadata describes a span or event.
    ///
    /// **Warning**: The fields on this type are currently `pub` because it must be able
    /// to be constructed statically by macros. However, when `const fn`s are
    /// available on stable Rust, this will no longer be necessary. Thus, these
    /// fields are *not* considered stable public API, and they may change
    /// warning. Do not rely on any fields on `Meta`. When constructing new
    /// `Meta`s, use the `metadata!` macro or the `Meta::new_span` and
    /// `Meta::new_event` constructors instead!
    #[doc(hidden)]
    pub kind: Kind,
}

/// Indicates whether a set of [metadata] describes a [`Span`] or an [`Event`].
///
/// [metadata]: ::Meta
/// [`Span`]: ::span::Span
/// [`Event`]: ::Event
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Kind(KindInner);

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
enum KindInner {
    Span,
    Event,
}

// ===== impl Meta =====

impl<'a> Meta<'a> {
    /// Construct new metadata for a span, with a name, target, level, field
    /// names, and optional source code location.
    pub fn new_span(
        name: Option<&'a str>,
        target: &'a str,
        level: Level,
        module_path: Option<&'a str>,
        file: Option<&'a str>,
        line: Option<u32>,
        field_names: &'static [&'static str],
        callsite: &'static callsite::Callsite,
    ) -> Self {
        Self {
            name,
            target,
            level,
            module_path,
            file,
            line,
            field_names,
            callsite,
            kind: Kind::SPAN,
        }
    }

    /// Construct new metadata for an event, with a target, level, field names,
    /// and optional source code location.
    pub fn new_id(
        target: &'a str,
        level: Level,
        module_path: Option<&'a str>,
        file: Option<&'a str>,
        line: Option<u32>,
        field_names: &'static [&'static str],
        callsite: &'static callsite::Callsite,
    ) -> Self {
        Self {
            name: None,
            target,
            level,
            module_path,
            file,
            line,
            field_names,
            callsite,
            kind: Kind::EVENT,
        }
    }

    /// Returns true if this metadata corresponds to an event.
    pub fn is_event(&self) -> bool {
        self.kind.is_event()
    }

    /// Returns true if this metadata corresponds to a span.
    pub fn is_span(&self) -> bool {
        self.kind.is_span()
    }

    /// Returns an iterator over the fields defined by this set of metadata.
    pub fn fields(&'a self) -> impl Iterator<Item = field::Key<'a>> {
        (0..self.field_names.len()).map(move |i| field::Key::new(i, self))
    }

    /// Returns a [`Key`](::field::Key) to the field corresponding to `name`, if
    /// one exists, or `None` if no such field exists.
    pub fn key_for<Q>(&'a self, name: &Q) -> Option<field::Key<'a>>
    where
        Q: Borrow<str>,
    {
        let name = &name.borrow();
        self.field_names
            .iter()
            .position(|f| f == name)
            .map(|i| field::Key::new(i, self))
    }

    /// Returns `true` if `self` contains a field for the given `key`.
    pub fn contains_key(&self, key: &field::Key<'a>) -> bool {
        key.metadata() == self && key.as_usize() <= self.field_names.len()
    }

    /// Returns the level of verbosity of the described span or event.
    pub fn level(&self) -> Level {
        self.level
    }

    /// Returns a string describing the part of the system where the
    /// span or event that this metadata describes occurred.
    ///
    /// Typically, this is the module path, but alternate targets may be
    /// set when spans or events are constructed.
    pub fn target(&self) -> &'a str {
        self.target
    }

    /// Returns the path to the Rust module where the span or event occurred, or `None`
    /// if the module path is unknown.
    pub fn module_path(&self) -> Option<&'a str> {
        self.module_path
    }

    /// Returns the name of the source code file where the span or event occurred, or
    /// `None` if the file is unknown
    pub fn file(&self) -> Option<&'a str> {
        self.file
    }

    /// Returns the line number in the source code file where the span or event
    /// occurred, or `None` if the line number is unknown.
    pub fn line(&self) -> Option<u32> {
        self.line
    }
}

impl<'a> PartialEq for Meta<'a> {
    #[inline]
    fn eq(&self, other: &Meta<'a>) -> bool {
        ::std::ptr::eq(self.callsite, other.callsite)
    }
}

// ===== impl Kind =====

impl Kind {
    /// Returns `true` if this metadata corresponds to a `Span`.
    pub fn is_span(&self) -> bool {
        match self {
            Kind(KindInner::Span) => true,
            _ => false,
        }
    }

    /// Returns `true` if this metadata corresponds to an `Event`.
    pub fn is_event(&self) -> bool {
        match self {
            Kind(KindInner::Event) => true,
            _ => false,
        }
    }

    /// The `Kind` for `Span` metadata.
    pub const SPAN: Self = Kind(KindInner::Span);

    /// The `Kind` for `Event` metadata.
    pub const EVENT: Self = Kind(KindInner::Event);
}
