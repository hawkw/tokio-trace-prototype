//! Metadata describing trace data.
use super::{callsite::Callsite, field};
use std::{
    fmt,
    hash::{Hash, Hasher},
    ptr,
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
    pub fields: field::Fields,

    /// The callsite from which this metadata originates.
    ///
    /// **Warning**: The fields on this type are currently `pub` because it must be able
    /// to be constructed statically by macros. However, when `const fn`s are
    /// available on stable Rust, this will no longer be necessary. Thus, these
    /// fields are *not* considered stable public API, and they may change
    /// warning. Do not rely on any fields on `Meta`. When constructing new
    /// `Meta`s, use the `metadata!` macro or the `Meta::new_span` and
    /// `Meta::new_event` constructors instead!
    #[doc(hidden)]
    pub callsite: &'static Callsite,

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

/// Describes the level of verbosity of a `Span` or `Event`.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Level(LevelInner);

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

/// Uniquely identifies a set of [metadata].
///
/// Two `Identifier`s are equal if they both refer to the same metadata.
///
/// [metadata]: ::Meta
#[derive(Clone)]
pub struct Identifier(&'static Callsite);

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
        callsite: &'static Callsite,
    ) -> Self {
        Self {
            name,
            target,
            level,
            module_path,
            file,
            line,
            fields: field::Fields {
                names: field_names,
                callsite,
            },
            callsite,
            kind: Kind::SPAN,
        }
    }

    /// Construct new metadata for an event, with a target, level, field names,
    /// and optional source code location.
    pub fn new_event(
        target: &'a str,
        level: Level,
        module_path: Option<&'a str>,
        file: Option<&'a str>,
        line: Option<u32>,
        field_names: &'static [&'static str],
        callsite: &'static Callsite,
    ) -> Self {
        Self {
            name: None,
            target,
            level,
            module_path,
            file,
            line,
            fields: field::Fields {
                names: field_names,
                callsite,
            },
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

    /// Returns the set of fields on the described span or event.
    pub fn fields(&self) -> &field::Fields {
        &self.fields
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

    /// Returns an opaque `Identifier` that uniquely identifies this `Metadata`.
    pub fn id(&self) -> Identifier {
        Identifier::from_callsite(self.callsite)
    }
}

impl<'a> fmt::Debug for Meta<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Meta")
            .field("name", &self.name)
            .field("target", &self.target)
            .field("level", &self.level)
            .field("module_path", &self.module_path)
            .field("file", &self.file)
            .field("line", &self.line)
            .field("field_names", &self.fields)
            .finish()
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

// ===== impl Level =====

impl Level {
    /// The "error" level.
    ///
    /// Designates very serious errors.
    pub const ERROR: Level = Level(LevelInner::Error);
    /// The "warn" level.
    ///
    /// Designates hazardous situations.
    pub const WARN: Level = Level(LevelInner::Warn);
    /// The "info" level.
    ///
    /// Designates useful information.
    pub const INFO: Level = Level(LevelInner::Info);
    /// The "debug" level.
    ///
    /// Designates lower priority information.
    pub const DEBUG: Level = Level(LevelInner::Debug);
    /// The "trace" level.
    ///
    /// Designates very low priority, often extremely verbose, information.
    pub const TRACE: Level = Level(LevelInner::Trace);
}

#[repr(usize)]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
enum LevelInner {
    /// The "error" level.
    ///
    /// Designates very serious errors.
    Error = 1, // This way these line up with the discriminants for LevelFilter below
    /// The "warn" level.
    ///
    /// Designates hazardous situations.
    Warn,
    /// The "info" level.
    ///
    /// Designates useful information.
    Info,
    /// The "debug" level.
    ///
    /// Designates lower priority information.
    Debug,
    /// The "trace" level.
    ///
    /// Designates very low priority, often extremely verbose, information.
    Trace,
}

// ===== impl Identifier =====

impl Identifier {
    /// Returns an `Identifier` unique to the provided `Callsite`.
    // TODO: can this just be public API?
    pub(crate) fn from_callsite(callsite: &'static Callsite) -> Self {
        Identifier(callsite)
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Identifier) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl Eq for Identifier {}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.pad("Identifier(...)")
    }
}

impl Hash for Identifier {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        (self.0 as *const Callsite).hash(state)
    }
}
