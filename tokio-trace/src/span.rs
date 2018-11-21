//! Spans represent periods of time in the execution of a program.
//!
//! # Entering a Span
//!
//! A thread of execution is said to _enter_ a span when it begins executing,
//! and _exit_ the span when it switches to another context. Spans may be
//! entered through the [`enter`](`Span::enter`) method, which enters the target span,
//! performs a given function (either a closure or a function pointer), exits
//! the span, and then returns the result.
//!
//! Calling `enter` on a span handle enters the span that handle corresponds to,
//! if the span exists:
//! ```
//! # #[macro_use] extern crate tokio_trace;
//! # fn main() {
//! let my_var: u64 = 5;
//! let mut my_span = span!("my_span", my_var = &my_var);
//!
//! my_span.enter(|| {
//!     // perform some work in the context of `my_span`...
//! });
//!
//! // Perform some work outside of the context of `my_span`...
//!
//! my_span.enter(|| {
//!     // Perform some more work in the context of `my_span`.
//! });
//! # }
//! ```
//!
//! # The Span Lifecycle
//!
//! Execution may enter and exit a span multiple times before that
//! span is _closed_. Consider, for example, a future which has an associated
//! span and enters that span every time it is polled:
//! ```rust
//! # extern crate tokio_trace;
//! # extern crate futures;
//! # use futures::{Future, Poll, Async};
//! struct MyFuture {
//!    // data
//!    span: tokio_trace::Span,
//! }
//!
//! impl Future for MyFuture {
//!     type Item = ();
//!     type Error = ();
//!
//!     fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
//!         self.span.enter(|| {
//!             // Do actual future work
//! # Ok(Async::Ready(()))
//!         })
//!     }
//! }
//! ```
//!
//! If this future was spawned on an executor, it might yield one or more times
//! before `poll` returns `Ok(Async::Ready)`. If the future were to yield, then
//! the executor would move on to poll the next future, which may _also_ enter
//! an associated span or series of spans. Therefore, it is valid for a span to
//! be entered repeatedly before it completes. Only the time when that span or
//! one of its children was the current span is considered to be time spent in
//! that span. A span which is not executing and has not yet been closed is said
//! to be _idle_.
//!
//! Because spans may be entered and exited multiple times before they close,
//! [`Subscriber`]s have separate trait methods which are called to notify them
//! of span exits and span closures. When execution exits a span,
//! [`exit`](::Subscriber::exit) will always be called with that span's ID to
//! notify the subscriber that the span has been exited. If the span has been
//! exited for the final time, the `exit` will be followed by a call to
//! [`close`](::Subscriber::close), signalling that the span has been closed.
//! Subscribers may expect that a span which has closed will not be entered
//! again.
//!
//! If there is only a single handle with the capacity to exit a span, dropping
//! that handle will automatically close the span, since the capacity to enter
//! it no longer exists. For example:
//! ```
//! # #[macro_use] extern crate tokio_trace;
//! # fn main() {
//! {
//!     span!("my_span").enter(|| {
//!         // perform some work in the context of `my_span`...
//!     }); // --> Subscriber::exit(my_span)
//!
//!     // The handle to `my_span` only lives inside of this block; when it is
//!     // dropped, the subscriber will be informed that `my_span` has closed.
//!
//! } // --> Subscriber::close(my_span)
//! # }
//! ```
//!
//! If one or more handles to a span exist, the span will be kept open until
//! that handle drops. However, a span may be explicitly asked to close by
//! calling the [`Span::close`] method. For example:
//! ```
//! # #[macro_use] extern crate tokio_trace;
//! # fn main() {
//! use tokio_trace::Span;
//!
//! let mut my_span = span!("my_span");
//! my_span.enter(|| {
//!     // Signal to my_span that it should close when it exits
//!     Span::current().close();
//! }); // --> Subscriber::exit(my_span); Subscriber::close(my_span)
//!
//! // The handle to `my_span` still exists, but it now knows that the span was
//! // closed while it was executing.
//! my_span.is_closed(); // ==> true
//!
//! // Attempting to enter the span using the handle again will do nothing.
//! my_span.enter(|| {
//!     // no-op
//! });
//! # }
//! ```
//!
//! When a span is asked to close by explicitly calling `Span::close`, if it is
//! executing, it will wait until it exits to signal that it has been closed. If
//! it is not currently executing, it will signal closure immediately.
//!
//! Calls to `Span::close()` are *not* guaranteed to close the span immediately.
//! If multiple handles to the span exist, the span will not be closed until all
//! but the one which opened the span have been dropped. This is to ensure that
//! a subscriber never observes an inconsistant state; namely, a span being
//! entered after it has closed.
//!
//! ## Shared spans
//!
//! In general, it is rarely necessary to create multiple handles to a span that
//! persist outside of the period during which that span is executing. Users who
//! wish to have multiple handles capable of entering a span, or enter the same
//! span from multiple threads, may wish to use the [shared span] type instead.
//!
//! A `Shared` handle behaves similarly to a `Span` handle, but may be cloned
//! freely. Any `Shared` handle may be used to enter the span it corresponds to,
//! and (since `Shared` handles are `Send + Sync`) they may be used to allow
//! multiple threads to enter the same span.
//!
//! Shared handles may be created from a span using the [`IntoShared`] trait:
//! ```
//! # #[macro_use] extern crate tokio_trace;
//! # fn main() {
//! use tokio_trace::span::IntoShared;
//! // Convert a regular `Span` handle into a cloneable `Shared` handle.
//! let span = span!("foo").into_shared();
//!
//! // Entering a `Shared` span handle *consumes* the handle:
//! span.clone().enter(|| {
//!     // ...
//! });
//!
//! // When all `Shared` handles to the span have been consumed, it will close.
//! span.enter(|| {
//!     // ...
//! }) // --> Subscriber::close(span)
//! # }
//! ```
//!
//! Unlike `Span` handles, `Shared` spans are represented by `Arc` pointers, so
//! constructing them will allocate memory, if the span is enabled.
//!
//! **Note**: When _not_ using shared spans, it is possible to cause a span to
//! _never_ be dropped, by entering it, creating a second handle using
//! `Span::current`, and returning that handle from the closure which executes
//! inside the span, and then dropping that handle while the span is not
//! currently executing. Since the span is not currently executing, it has no
//! way to observe the second handle being dropped, and it cannot determine if
//! it is safe to close.  However, if all such handles are dropped while the
//! span is executing, the span will still be able to close normally. Spans will
//! only  fail to close in situations where the second handle is dropped while
//! the span is _not_ executing.
//!
//! This is possible because the logic for determining if a span can close is
//! intentionally cautious. Spans will only close if they _know_ they cannot be
//! entered; although a span failing to close does result in potentially
//! incorrect trace data, this is less serious of a problem than the
//! inconsistant state that arises when a span is closed and then re-entered.
//! The latter violates the assumptions that a reasonable subscriber
//! implementation could be expected to make.
//!
//!
//! # Accessing a Span's Attributes
//!
//! The [`Attributes`] type represents a *non-entering* reference to a `Span`'s data
//! --- a set of key-value pairs (known as _fields_), a creation timestamp,
//! a reference to the span's parent in the trace tree, and metadata describing
//! the source code location where the span was created. This data is provided
//! to the [`Subscriber`] when the span is created; it may then choose to cache
//! the data for future use, record it in some manner, or discard it completely.
//!
//! [`Subscriber`]: ::Subscriber
//! [`State`]: ::span::State
//! [`Attributes`]: ::span::Attributes
//! [shared span]: ::span::Shared
//! [`IntoShared`]: ::span::IntoShared
pub use tokio_trace_core::span::{Attributes, Id, SpanAttributes};

#[cfg(any(test, feature = "test-support"))]
pub use tokio_trace_core::span::{mock, MockSpan};

use std::{
    borrow::Borrow,
    cmp, fmt,
    hash::{Hash, Hasher},
    sync::atomic::{AtomicBool, AtomicUsize, Ordering},
};
use {
    callsite::Callsite,
    field,
    subscriber::{Interest, Subscriber},
    Dispatch, Meta,
};

/// A handle representing a span, with the capability to enter the span if it
/// exists.
///
/// If the span was rejected by the current `Subscriber`'s filter, entering the
/// span will silently do nothing. Thus, the handle can be used in the same
/// manner regardless of whether or not the trace is currently being collected.
#[derive(PartialEq, Hash)]
pub struct Span {
    /// A handle used to enter the span when it is not executing.
    ///
    /// If this is `None`, then the span has either closed or was never enabled.
    inner: Option<Enter>,

    /// Set to `true` when the span closes.
    ///
    /// This allows us to distinguish if `inner` is `None` because the span was
    /// never enabled (and thus the inner state was never created), or if the
    /// previously entered, but it is now closed.
    is_closed: bool,
}

/// `Event`s represent single points in time where something occurred during the
/// execution of a program.
///
/// An event can be compared to a log record in unstructured logging, but with
/// two key differences:
/// - Events exist _within the context of a [`Span`]_. Unlike log lines, they may
///   be located within the trace tree, allowing visibility into the context in
///   which the event occurred.
/// - Events have structured key-value data known as _fields_, as well as a
///   textual message. In general, a majority of the data associated with an
///   event should be in the event's fields rather than in the textual message,
///   as the fields are more structed.
///
/// [`Span`]: ::span::Span
#[derive(PartialEq, Hash)]
pub struct Event<'a> {
    /// A handle used to enter the span when it is not executing.
    ///
    /// If this is `None`, then the span has either closed or was never enabled.
    inner: Option<Inner<'a>>,
}


/// A handle representing the capacity to enter a span which is known to exist.
///
/// Unlike `Span`, this type is only constructed for spans which _have_ been
/// enabled by the current filter. This type is primarily used for implementing
/// span handles; users should typically not need to interact with it directly.
#[derive(Debug)]
pub struct Inner<'a> {
    /// The span's ID, as provided by `subscriber`.
    id: Id,

    /// The subscriber that will receive events relating to this span.
    ///
    /// This should be the same subscriber that provided this span with its
    /// `id`.
    subscriber: Dispatch,

    /// The span ID of this span's parent, if there is one.
    parent: Option<Id>,

    /// A flag indicating that the span has been instructed to close when
    /// possible.
    ///
    /// This does _not_ indicate that it is _currently_ okay to close the span,
    /// only that it _should_ close when it is safe to do so.
    wants_close: AtomicBool,

    /// A flag that is set when the span is entered for the first time.
    ///
    /// If this is false, closing the span should do nothing.
    has_entered: AtomicBool,

    /// Incremented when new handles are created, and decremented when they are
    /// dropped if this is the current span.
    handles: AtomicUsize,

    meta: &'a Meta<'a>,
}

/// When an `Inner` corresponds to a `Span` rather than an `Event`, it can be
/// used to enter that span.
type Enter = Inner<'static>;

/// A guard representing a span which has been entered and is currently
/// executing.
///
/// This guard may be used to exit the span, returning an `Enter` to
/// re-enter it.
///
/// This type is primarily used for implementing span handles; users should
/// typically not need to interact with it directly.
#[derive(Debug)]
#[must_use = "once a span has been entered, it should be exited"]
struct Entered {
    id: Id,
    dispatch: Dispatch,
    prior: Option<Enter>,
}

// ===== impl Span =====

impl Span {
    pub(crate) const NONE: Span = Span {
        inner: None,
        is_closed: false,
    };

    /// Constructs a new `Span` originating from the given [`Callsite`].
    ///
    /// The new span will be constructed by the currently-active [`Subscriber`],
    /// with the [current span] as its parent (if one exists).
    ///
    /// If the new span is enabled, then the provided function `if_enabled` is
    /// envoked on it before it is returned. This allows [field values] and/or
    /// [`follows_from` annotations] to be added to the span, but skips this
    /// work for spans which are disabled.
    ///
    /// [`Callsite`]: ::callsite::Callsite
    /// [`Subscriber`]: ::subscriber::Subscriber
    /// [current span]: ::span::Span::current
    /// [field values]: ::span::Span::record
    /// [`follows_from` annotations]: ::span::Span::follows_from
    #[inline]
    pub fn new<F>(callsite: &'static dyn Callsite, if_enabled: F) -> Span
    where
        F: FnOnce(&mut Span),
    {
        let interest = callsite.interest();
        if interest == Interest::NEVER {
            return Span::new_disabled();
        }
        Dispatch::with_current(|dispatch| {
            let meta = callsite.metadata();
            if interest == Interest::SOMETIMES && !dispatch.enabled(meta) {
                return Span {
                    inner: None,
                    is_closed: false,
                };
            }
            let parent = Id::current();
            let attrs = Attributes::new(parent.clone(), meta);
            let id = dispatch.new_span(attrs);
            let inner = Some(Enter::new(id, parent, dispatch, meta));
            let mut span = Self {
                inner,
                is_closed: false,
            };
            if_enabled(&mut span);
            span
        })
    }

    /// Constructs a new disabled span.
    pub fn new_disabled() -> Span {
        Span {
            inner: None,
            is_closed: false,
        }
    }

    /// Returns a reference to the span that this thread is currently
    /// executing.
    pub fn current() -> Self {
        Dispatch::with_current(|dispatch| dispatch.current_span().clone())
    }

    /// Executes the given function in the context of this span.
    ///
    /// If this span is enabled, then this function enters the span, invokes
    /// and then exits the span. If the span is disabled, `f` will still be
    /// invoked, but in the context of the currently-executing span (if there is
    /// one).
    ///
    /// Returns the result of evaluating `f`.
    pub fn enter<F: FnOnce() -> T, T>(&mut self, f: F) -> T {
        match self.inner.take() {
            Some(inner) => inner.subscriber.as_default(|| {
                if !self.is_closed() {
                    inner.take_close();
                }
                let guard = inner.enter();
                let result = f();
                self.inner = guard.exit();
                result
            }),
            None => f(),
        }
    }

    /// Returns the `Id` of the parent of this span, if one exists.
    pub fn parent(&self) -> Option<Id> {
        self.inner.as_ref().and_then(Enter::parent)
    }

    /// Returns a [`Key`](::field::Key) for the field with the given `name`, if
    /// one exists,
    pub fn key_for<Q>(&self, name: &Q) -> Option<field::Key<'static>>
    where
        Q: Borrow<str>,
    {
        self.inner
            .as_ref()
            .and_then(|inner| inner.meta.key_for(name))
    }
    /// Record a signed 64-bit integer value.
    pub fn record_value_i64(&mut self, field: &field::Key, value: i64) -> &Self {
        if let Some(ref inner) = self.inner {
            inner.record_value_i64(field, value);
        }
        self
    }

    /// Record an umsigned 64-bit integer value.
    pub fn record_value_u64(&self, field: &field::Key, value: u64) -> &Self {
        if let Some(ref inner) = self.inner {
            inner.record_value_u64(field, value);
        }
        self
    }

    /// Record a boolean value.
    pub fn record_value_bool(&self, field: &field::Key, value: bool) -> &Self {
        if let Some(ref inner) = self.inner {
            inner.record_value_bool(field, value);
        }
        self
    }

    /// Record a string value.
    pub fn record_value_str(&self, field: &field::Key, value: &str) -> &Self {
        if let Some(ref inner) = self.inner {
            inner.record_value_str(field, value);
        }
        self
    }

    /// Record a precompiled set of format arguments.
    pub fn record_value_fmt(&self, field: &field::Key, value: fmt::Arguments) -> &Self {
        if let Some(ref inner) = self.inner {
            inner.record_value_fmt(field, value);
        }
        self
    }

    /// Signals that this span should close the next time it is exited, or when
    /// it is dropped.
    pub fn close(&mut self) {
        self.is_closed = true;
        // Take the span's `Enter` handle and try to close it. If the span is
        // idle, then the `Enter` will close it immediately when it is dropped.
        // Otherwise, if corresponds to the same span as the currently-executing
        // one, it will tell the current span to try to close when it exits.
        // Taking the `Enter` will prevent this handle from re-entering the span.
        self.inner.take().as_ref().map(Enter::close);
    }

    /// Returns `true` if this span is closed.
    pub fn is_closed(&self) -> bool {
        self.inner.is_none() && self.is_closed
    }

    /// Returns `true` if this span was disabled by the subscriber and does not
    /// exist.
    pub fn is_disabled(&self) -> bool {
        self.inner.is_none() && !self.is_closed
    }

    /// Indicates that the span with the given ID has an indirect causal
    /// relationship with this span.
    ///
    /// This relationship differs somewhat from the parent-child relationship: a
    /// span may have any number of prior spans, rather than a single one; and
    /// spans are not considered to be executing _inside_ of the spans they
    /// follow from. This means that a span may close even if subsequent spans
    /// that follow from it are still open, and time spent inside of a
    /// subsequent span should not be included in the time its precedents were
    /// executing. This is used to model causal relationships such as when a
    /// single future spawns several related background tasks, et cetera.
    ///
    /// If this span is disabled, or the resulting follows-from relationship
    /// would be invalid, this function will do nothing.
    pub fn follows_from(&self, from: Id) -> &Self {
        if let Some(ref inner) = self.inner {
            inner.follows_from(from);
        }
        self
    }

    /// Returns this span's `Id`, if it is enabled.
    pub fn id(&self) -> Option<Id> {
        self.inner.as_ref().map(Enter::id)
    }

    /// Returns this span's `Meta`, if it is enabled.
    pub fn metadata(&self) -> Option<&'static Meta<'static>> {
        self.inner.as_ref().map(|inner| inner.metadata())
    }

    fn clone(&self) -> Self {
        let inner = self.inner.as_ref().map(Enter::duplicate_handle);
        Self {
            inner,
            is_closed: self.is_closed,
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut span = f.debug_struct("Span");
        if let Some(ref inner) = self.inner {
            span.field("id", &inner.id())
                .field("parent", &inner.parent())
        } else {
            span.field("disabled", &true)
        }.finish()
    }
}

// ===== impl Event =====
impl<'a> Event<'a> {
    /// Constructs a new `Span` originating from the given [`Callsite`].
    ///
    /// The new span will be constructed by the currently-active [`Subscriber`],
    /// with the [current span] as its parent (if one exists).
    ///
    /// If the new span is enabled, then the provided function `if_enabled` is
    /// envoked on it before it is returned. This allows [field values] and/or
    /// [`follows_from` annotations] to be added to the span, but skips this
    /// work for spans which are disabled.
    ///
    /// [`Callsite`]: ::callsite::Callsite
    /// [`Subscriber`]: ::subscriber::Subscriber
    /// [current span]: ::span::Span::current
    /// [field values]: ::span::Span::record
    /// [`follows_from` annotations]: ::span::Span::follows_from
    #[inline]
    pub fn new<F>(callsite: &'a dyn Callsite, if_enabled: F) -> Self
    where
        F: FnOnce(&mut Self),
    {
        let interest = callsite.interest();
        if interest == Interest::NEVER {
            return Self { inner: None };
        }
        Dispatch::with_current(|dispatch| {
            let meta = callsite.metadata();
            if interest == Interest::SOMETIMES && !dispatch.enabled(meta) {
                return Self { inner: None };
            }
            let parent = Id::current();
            let attrs = Attributes::new(parent.clone(), meta);
            let id = dispatch.new_id(attrs);
            let inner = Enter::new(id, parent, dispatch, meta);
            inner.has_entered.store(true, Ordering::Relaxed);
            inner.close();
            let mut event = Self { inner: Some(inner) };
            if_enabled(&mut event);
            event
        })
    }

    /// Adds a formattable message describing the event that occurred.
    pub fn message(&mut self, key: &field::Key, message: fmt::Arguments) -> &mut Self {
        if let Some(ref mut inner) = self.inner {
            inner.subscriber.record_fmt(&inner.id, key, message);
        }
        self
    }

    /// Record a signed 64-bit integer value.
    pub fn record_value_i64(&mut self, field: &field::Key, value: i64) -> &Self {
        if let Some(ref inner) = self.inner {
            inner.record_value_i64(field, value);
        }
        self
    }

    /// Record an umsigned 64-bit integer value.
    pub fn record_value_u64(&self, field: &field::Key, value: u64) -> &Self {
        if let Some(ref inner) = self.inner {
            inner.record_value_u64(field, value);
        }
        self
    }

    /// Record a boolean value.
    pub fn record_value_bool(&self, field: &field::Key, value: bool) -> &Self {
        if let Some(ref inner) = self.inner {
            inner.record_value_bool(field, value);
        }
        self
    }

    /// Record a string value.
    pub fn record_value_str(&self, field: &field::Key, value: &str) -> &Self {
        if let Some(ref inner) = self.inner {
            inner.record_value_str(field, value);
        }
        self
    }

    /// Record a precompiled set of format arguments.
    pub fn record_value_fmt(&self, field: &field::Key, value: fmt::Arguments) -> &Self {
        if let Some(ref inner) = self.inner {
            inner.record_value_fmt(field, value);
        }
        self
    }

    /// Returns the `Id` of the parent of this span, if one exists.
    pub fn parent(&self) -> Option<Id> {
        self.inner.as_ref().and_then(Enter::parent)
    }

    /// Returns a [`Key`](::field::Key) for the field with the given `name`, if
    /// one exists,
    pub fn key_for<Q>(&self, name: &Q) -> Option<field::Key<'a>>
    where
        Q: Borrow<str>,
    {
        self.inner
            .as_ref()
            .and_then(|inner| inner.meta.key_for(name))
    }

    /// Returns `true` if this span was disabled by the subscriber and does not
    /// exist.
    pub fn is_disabled(&self) -> bool {
        self.inner.is_none()
    }

    /// Indicates that the span with the given ID has an indirect causal
    /// relationship with this event.
    ///
    /// This relationship differs somewhat from the parent-child relationship: a
    /// span may have any number of prior spans, rather than a single one; and
    /// spans are not considered to be executing _inside_ of the spans they
    /// follow from. This means that a span may close even if subsequent spans
    /// that follow from it are still open, and time spent inside of a
    /// subsequent span should not be included in the time its precedents were
    /// executing. This is used to model causal relationships such as when a
    /// single future spawns several related background tasks, et cetera.
    ///
    /// If this event is disabled, or the resulting follows-from relationship
    /// would be invalid, this function will do nothing.
    pub fn follows_from(&self, from: Id) -> &Self {
        if let Some(ref inner) = self.inner {
            inner.follows_from(from);
        }
        self
    }

    /// Returns this span's `Id`, if it is enabled.
    pub fn id(&self) -> Option<Id> {
        self.inner.as_ref().map(Enter::id)
    }

    /// Returns this span's `Meta`, if it is enabled.
    pub fn metadata(&self) -> Option<&'a Meta<'a>> {
        self.inner.as_ref().map(|inner| inner.metadata())
    }
}

/// Trait for converting a `Span` into a cloneable `Shared` span.
pub trait IntoShared {
    /// Returns a `Shared` span handle that can be cloned.
    ///
    /// This will allocate memory to store the span if the span is enabled.
    fn into_shared(self) -> Shared;
}

pub trait SpanExt: field::Record + ::sealed::Sealed {
    fn record<Q: ?Sized, V: ?Sized>(&mut self, field: &Q, value: &V) -> &mut Self
    where
        Q: field::AsKey,
        V: field::Value;

    fn has_field_for<Q: ?Sized>(&self, field: &Q) -> bool
    where
        Q: field::AsKey;
}

impl IntoShared for Span {
    fn into_shared(self) -> Shared {
        Shared::from_span(self)
    }
}

/// A shared, heap-allocated span handle, which may be cloned.
///
/// Unlike a `Span` handle, entering a `Shared` span handle consumes the handle.
/// This is used to determine when the span may be closed. Thus, each `Shared`
/// handle may be thought of as representing a *single* attempt to enter the
/// span.
///
/// However, they may be cloned inexpensively (as they are internally
/// reference-counted).
// NOTE: It may also be worthwhile to provide an `UnsyncShared` type, that uses
// `Rc` rather than `Arc` for performance reasons? I'm not sure what the
// use-case for that would be, though.
#[derive(Clone, Debug)]
pub struct Shared {
    inner: Option<Arc<Enter>>,
}

impl Shared {
    /// Returns a `Shared` span handle that can be cloned.
    ///
    /// This function allocates memory to store the shared span, if the span is
    /// enabled.
    pub fn from_span(span: Span) -> Self {
        Self {
            inner: Enter::from_span(span).map(Arc::new),
        }
    }

    /// Executes the given function in the context of this span.
    ///
    /// Unlike `Span::enter`, this *consumes* the shared span handle.
    ///
    /// If this span is enabled, then this function enters the span, invokes
    /// and then exits the span. If the span is disabled, `f` will still be
    /// invoked, but in the context of the currently-executing span (if there is
    /// one).
    ///
    /// Returns the result of evaluating `f`.
    pub fn enter<F: FnOnce() -> T, T>(self, f: F) -> T {
        if let Some(inner) = self.inner {
            let guard = inner.enter();
            let result = f();
            inner.exit_and_join(guard);
            if Arc::strong_count(&inner) == 1 {
                inner.close();
            }
            result
        } else {
            f()
        }
    }

    /// Returns the `Id` of the parent of this span, if one exists.
    pub fn parent(&self) -> Option<Id> {
        self.inner.as_ref().and_then(|inner| inner.parent())
    }

    /// Returns the `Id` of the span, or `None` if it is disabled.
    pub fn id(&self) -> Option<Id> {
        self.inner.as_ref().map(|inner| inner.id())
    }

    /// Returns `true` if this span is enabled.
    pub fn is_enabled(&self) -> bool {
        self.inner.is_some()
    }

    /// Indicates that the span with the given ID has an indirect causal
    /// relationship with this span.
    ///
    /// This relationship differs somewhat from the parent-child relationship: a
    /// span may have any number of prior spans, rather than a single one; and
    /// spans are not considered to be executing _inside_ of the spans they
    /// follow from. This means that a span may close even if subsequent spans
    /// that follow from it are still open, and time spent inside of a
    /// subsequent span should not be included in the time its precedents were
    /// executing. This is used to model causal relationships such as when a
    /// single future spawns several related background tasks, et cetera.
    ///
    /// If this span is disabled, this function will do nothing. Otherwise, it
    /// returns `Ok(())` if the other span was added as a precedent of this
    /// span, or an error if this was not possible.
    pub fn follows_from(&self, from: Id) -> &Self {
        if let Some(ref inner) = self.inner {
            inner.follows_from(from);
        }
        self
    }
}


// ===== impl Enter =====

impl<'a> Inner<'a> {
    /// Indicates the span _should_ be closed the next time it exits or this
    /// handle is dropped.
    pub fn close(&self) {
        self.wants_close.store(true, Ordering::Release);
    }

    /// Indicates that the span with the given ID has an indirect causal
    /// relationship with this span.
    ///
    /// This relationship differs somewhat from the parent-child relationship: a
    /// span may have any number of prior spans, rather than a single one; and
    /// spans are not considered to be executing _inside_ of the spans they
    /// follow from. This means that a span may close even if subsequent spans
    /// that follow from it are still open, and time spent inside of a
    /// subsequent span should not be included in the time its precedents were
    /// executing. This is used to model causal relationships such as when a
    /// single future spawns several related background tasks, et cetera.
    ///
    /// If this span is disabled, this function will do nothing. Otherwise, it
    /// returns `Ok(())` if the other span was added as a precedent of this
    /// span, or an error if this was not possible.
    pub fn follows_from(&self, from: Id) {
        self.subscriber.add_follows_from(&self.id, from)
    }

    /// Returns the span's ID.
    pub fn id(&self) -> Id {
        self.id.clone()
    }

    /// Returns the ID of the span's parent, if it has one.
    pub fn parent(&self) -> Option<Id> {
        self.parent.clone()
    }

    /// Returns the span's metadata.
    pub fn metadata(&self) -> &'a Meta<'a> {
        self.meta
    }

    /// Record a signed 64-bit integer value.
    pub(crate) fn record_value_i64(&self, field: &field::Key, value: i64) {
        if self.meta.contains_key(field) {
            self.subscriber.record_i64(&self.id, field, value)
        }
    }

    /// Record an umsigned 64-bit integer value.
    pub(crate) fn record_value_u64(&self, field: &field::Key, value: u64) {
        if self.meta.contains_key(field) {
            self.subscriber.record_u64(&self.id, field, value)
        }
    }

    /// Record a boolean value.
    pub(crate) fn record_value_bool(&self, field: &field::Key, value: bool) {
        if self.meta.contains_key(field) {
            self.subscriber.record_bool(&self.id, field, value)
        }
    }

    /// Record a string value.
    pub(crate) fn record_value_str(&self, field: &field::Key, value: &str) {
        if self.meta.contains_key(field) {
            self.subscriber.record_str(&self.id, field, value)
        }
    }

    /// Record a precompiled set of format arguments value.
    pub(crate) fn record_value_fmt(&self, field: &field::Key, value: fmt::Arguments) {
        if self.meta.contains_key(field) {
            self.subscriber.record_fmt(&self.id, field, value)
        }
    }

    fn new(id: Id, parent: Option<Id>, subscriber: &Dispatch, meta: &'a Meta<'a>) -> Self {
        Self {
            id,
            subscriber: subscriber.clone(),
            parent,
            wants_close: AtomicBool::from(false),
            has_entered: AtomicBool::from(false),
            handles: AtomicUsize::from(1),
            meta,
        }
    }

    fn duplicate(&self) -> Self {
        let id = self.subscriber.clone_span(self.id.clone());
        Self {
            id,
            subscriber: self.subscriber.clone(),
            parent: self.parent.clone(),
            wants_close: AtomicBool::from(self.wants_close()),
            has_entered: AtomicBool::from(self.has_entered()),
            handles: AtomicUsize::from(self.handle_count()),
            meta: self.meta,
        }
    }

    /// Returns `true` if this span _should_ close itself.
    ///
    /// This is true IFF:
    /// - the span has been told to close,
    /// - the span has been entered already (we don't want to close spans that
    ///   have never been entered),
    /// - there aren't multiple handles capable of entering the span.
    fn should_close(&self) -> bool {
        self.wants_close() && self.has_entered() && self.handle_count() == 1
    }

    fn has_entered(&self) -> bool {
        self.has_entered.load(Ordering::Acquire)
    }

    fn wants_close(&self) -> bool {
        self.wants_close.load(Ordering::Acquire)
    }

    fn take_close(&self) -> bool {
        self.wants_close.swap(false, Ordering::Release)
    }

    fn handle_count(&self) -> usize {
        self.handles.load(Ordering::Acquire)
    }
}

impl Enter {
    /// Consumes `span` and returns the inner entering handle, if the span is
    /// enabled.
    ///
    /// The returned `Enter` has approximately the same behaviour to a `Span`.
    /// However, it is primarily intended for use in libraries custom span
    /// types; the `Span` handle will typically represent a more ergonomic API
    /// for actually _using_ spans.
    pub fn from_span(span: Span) -> Option<Self> {
        span.inner
    }

    /// Return a new `Span` handle to the span represented by this `Enter`.
    pub fn into_span(self) -> Span {
        Span {
            inner: Some(self),
            is_closed: false,
        }
    }

    /// Enters the span, returning a guard that may be used to exit the span and
    /// re-enter the prior span.
    ///
    /// This is used internally to implement `Span::enter`. It may be used for
    /// writing custom span handles, but should generally not be called directly
    /// when entering a span.
    pub fn enter(&self) -> Entered {
        // The current handle will no longer enter the span, since it has just
        // been used to enter. Therefore, it will be safe to close the span if
        // no additional handles exist when the span is exited.
        self.handles.fetch_sub(1, Ordering::Release);
        // The span has now been entered, so it's okay to close it.
        self.has_entered.store(true, Ordering::Release);
        let Span { inner: prior, .. } = self.subscriber.enter(self.duplicate().into_span());
        Entered {
            prior,
            id: self.id.clone(),
            dispatch: self.subscriber.clone(),
        }
    }

    /// Exits the span entry represented by an `Entered` guard, consuming it,
    /// and updates `self` to canonically represent the referenced span's state
    /// after the other entry has exited.
    ///
    /// If the other handle to the span wanted to close the span on exit, it
    /// will not do so. Instead, the responsibility for performing the `close`
    /// will be transferred to `self` --- if `self` did not previously want to
    /// close, but `other` did, `self` will now want to close.
    ///
    /// This means that dropping `other` will no longer close the span, even if
    /// it previously would have.
    ///
    /// This function is intended to be used by span handle implementations to
    /// ensuring that multiple entering handles are kept consistent. Probably
    /// don't use this unless you know what you're doing.
    pub fn exit_and_join(&self, other: Entered) {
        if let Some(other) = other.exit() {
            self.handles.store(other.handle_count(), Ordering::Release);
            self.has_entered
                .store(other.has_entered(), Ordering::Release);
            self.wants_close
                .store(other.take_close(), Ordering::Release);
        }
    }

    fn duplicate_handle(&self) -> Self {
        self.handles.fetch_add(1, Ordering::Release);
        self.duplicate()
    }
}

impl<'a> cmp::PartialEq for Inner<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<'a> Hash for Inner<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<'a> Drop for Inner<'a> {
    fn drop(&mut self) {
        self.subscriber.drop_span(self.id.clone());
        // If this handle wants to be closed, try to close it --- either by
        // closing it now if it is idle, or telling the current span to close
        // when it exits, if it is the current span.
        if self.has_entered() && self.wants_close() {
            match self.subscriber.current_span().inner {
                // If the `enter` being dropped corresponds to the same span as
                // the current span, then we cannot close it yet. Instead, we
                // signal to the copy of this span that currently occupies
                // CURRENT_SPAN that it should try to close when it is exited.
                Some(ref current) if current == self => {
                    current.handles.fetch_sub(1, Ordering::Release);
                    current
                        .wants_close
                        .store(self.wants_close(), Ordering::Release);
                }
                // If this span is not the current span, then it may close now,
                // if there are no other handles with the capacity to re-enter it.
                _ if self.handle_count() <= 1 => {
                    self.subscriber.close(self.id());
                }
                _ => {}
            }
        }
    }
}

impl Entered {
    /// Exit the `Entered` guard, returning an `Enter` handle that may be used
    /// to re-enter the span, or `None` if the span closed while performing the
    /// exit.
    pub fn exit(self) -> Option<Enter> {
        let prior = self
            .prior
            .map(Enter::into_span)
            .unwrap_or_else(Span::new_disabled);
        Enter::from_span(self.dispatch.exit(self.id, prior)).and_then(|inner| {
            if inner.should_close() {
                // Dropping `inner` will allow it to perform the closure if
                // able.
                None
            } else {
                // We are returning a new `Enter`. Increment the number of
                // handles that may enter the span.
                inner.handles.fetch_add(1, Ordering::Release);
                // The span will want to close if it is dropped prior to
                // being re-entered.
                inner.wants_close.store(true, Ordering::Release);
                Some(inner)
            }
        })
    }
}

impl ::sealed::Sealed for Span {}

impl SpanExt for Span {
    fn has_field_for<Q: ?Sized>(&self, field: &Q) -> bool
    where
        Q: field::AsKey,
    {
        self.metadata()
            .and_then(|meta| field.as_key(meta))
            .is_some()
    }

    fn record<Q: ?Sized, V: ?Sized>(&mut self, field: &Q, value: &V) -> &mut Self
    where
        Q: field::AsKey,
        V: field::Value,
    {
        value.record(field, self);
        self
    }
}

impl field::Record for Span {
    #[inline]
    fn record_i64<Q: ?Sized>(&mut self, field: &Q, value: i64)
    where
        Q: field::AsKey,
    {
        if let Some(key) = self.metadata().and_then(|meta| field.as_key(meta)) {
            self.record_value_i64(&key, value);
        }
    }

    #[inline]
    fn record_u64<Q: ?Sized>(&mut self, field: &Q, value: u64)
    where
        Q: field::AsKey,
    {
        if let Some(key) = self.metadata().and_then(|meta| field.as_key(meta)) {
            self.record_value_u64(&key, value);
        }
    }

    #[inline]
    fn record_bool<Q: ?Sized>(&mut self, field: &Q, value: bool)
    where
        Q: field::AsKey,
    {
        if let Some(key) = self.metadata().and_then(|meta| field.as_key(meta)) {
            self.record_value_bool(&key, value);
        }
    }

    #[inline]
    fn record_str<Q: ?Sized>(&mut self, field: &Q, value: &str)
    where
        Q: field::AsKey,
    {
        if let Some(key) = self.metadata().and_then(|meta| field.as_key(meta)) {
            self.record_value_str(&key, value);
        }
    }

    #[inline]
    fn record_fmt<Q: ?Sized>(&mut self, field: &Q, value: fmt::Arguments)
    where
        Q: field::AsKey,
    {
        if let Some(key) = self.metadata().and_then(|meta| field.as_key(meta)) {
            self.record_value_fmt(&key, value);
        }
    }
}

impl<'a> field::Record for Event<'a> {
    #[inline]
    fn record_i64<Q: ?Sized>(&mut self, field: &Q, value: i64)
    where
        Q: field::AsKey,
    {
        if let Some(key) = self.metadata().and_then(|meta| field.as_key(meta)) {
            self.record_value_i64(&key, value);
        }
    }

    #[inline]
    fn record_u64<Q: ?Sized>(&mut self, field: &Q, value: u64)
    where
        Q: field::AsKey,
    {
        if let Some(key) = self.metadata().and_then(|meta| field.as_key(meta)) {
            self.record_value_u64(&key, value);
        }
    }

    #[inline]
    fn record_bool<Q: ?Sized>(&mut self, field: &Q, value: bool)
    where
        Q: field::AsKey,
    {
        if let Some(key) = self.metadata().and_then(|meta| field.as_key(meta)) {
            self.record_value_bool(&key, value);
        }
    }

    #[inline]
    fn record_str<Q: ?Sized>(&mut self, field: &Q, value: &str)
    where
        Q: field::AsKey,
    {
        if let Some(key) = self.metadata().and_then(|meta| field.as_key(meta)) {
            self.record_value_str(&key, value);
        }
    }

    #[inline]
    fn record_fmt<Q: ?Sized>(&mut self, field: &Q, value: fmt::Arguments)
    where
        Q: field::AsKey,
    {
        if let Some(key) = self.metadata().and_then(|meta| field.as_key(meta)) {
            self.record_value_fmt(&key, value);
        }
    }
}

impl<'a> ::sealed::Sealed for Event<'a> {}

impl<'a> SpanExt for Event<'a> {
    fn has_field_for<Q: ?Sized>(&self, field: &Q) -> bool
    where
        Q: field::AsKey,
    {
        self.metadata()
            .and_then(|meta| field.as_key(meta))
            .is_some()
    }

    fn record<Q: ?Sized, V: ?Sized>(&mut self, field: &Q, value: &V) -> &mut Self
    where
        Q: field::AsKey,
        V: field::Value,
    {
        value.record(field, self);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;
    use {span, subscriber, Dispatch};

    #[test]
    fn exit_doesnt_finish_while_handles_still_exist() {
        // Test that exiting a span only marks it as "done" when no handles
        // that can re-enter the span exist.
        let subscriber = subscriber::mock()
            .enter(span::mock().named(Some("foo")))
            .enter(span::mock().named(Some("bar")))
            // The first time we exit "bar", there will be another handle with
            // which we could potentially re-enter bar.
            .exit(span::mock().named(Some("bar")))
            // Re-enter "bar", using the cloned handle.
            .enter(span::mock().named(Some("bar")))
            // Now, when we exit "bar", there is no handle to re-enter it, so
            // it should become "done".
            .exit(span::mock().named(Some("bar")))
            .close(span::mock().named(Some("bar")))
            .exit(span::mock().named(Some("foo")))
            .run();

        Dispatch::new(subscriber).as_default(|| {
            span!("foo").enter(|| {
                let mut bar = span!("bar",);
                let another_bar = bar.enter(|| {
                    // do nothing. exiting "bar" should leave it idle, since it can
                    // be re-entered.
                    let mut another_bar = Span::current();
                    another_bar.close();
                    another_bar
                });
                // Enter "bar" again. This time, the previously-requested
                // closure should be honored.
                bar.enter(move || {
                    // Drop the other handle to bar. Now, the span should be allowed
                    // to close.
                    drop(another_bar);
                });
            });
        });
    }

    #[test]
    fn handles_to_the_same_span_are_equal() {
        // Create a mock subscriber that will return `true` on calls to
        // `Subscriber::enabled`, so that the spans will be constructed. We
        // won't enter any spans in this test, so the subscriber won't actually
        // expect to see any spans.
        Dispatch::new(subscriber::mock().run()).as_default(|| {
            span!("foo").enter(|| {
                let foo1 = Span::current();
                let foo2 = Span::current();
                // Two handles that point to the same span are equal.
                assert_eq!(foo1, foo2);
            })
        });
    }

    #[test]
    fn handles_to_different_spans_are_not_equal() {
        Dispatch::new(subscriber::mock().run()).as_default(|| {
            // Even though these spans have the same name and fields, they will have
            // differing metadata, since they were created on different lines.
            let foo1 = span!("foo", bar = 1u64, baz = false);
            let foo2 = span!("foo", bar = 1u64, baz = false);

            assert_ne!(foo1, foo2);
            // assert_ne!(foo1.data(), foo2.data());
        });
    }

    #[test]
    fn handles_to_different_spans_with_the_same_metadata_are_not_equal() {
        // Every time time this function is called, it will return a _new
        // instance_ of a span with the same metadata, name, and fields.
        fn make_span() -> Span {
            span!("foo", bar = 1u64, baz = false)
        }

        Dispatch::new(subscriber::mock().run()).as_default(|| {
            let foo1 = make_span();
            let foo2 = make_span();

            assert_ne!(foo1, foo2);
            // assert_ne!(foo1.data(), foo2.data());
        });
    }

    #[test]
    fn spans_always_go_to_the_subscriber_that_tagged_them() {
        let subscriber1 = subscriber::mock()
            .enter(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")))
            .enter(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")))
            .close(span::mock().named(Some("foo")))
            .done();
        let subscriber1 = Dispatch::new(subscriber1.run());
        let subscriber2 = Dispatch::new(subscriber::mock().run());

        let mut foo = subscriber1.as_default(|| {
            let mut foo = span!("foo");
            foo.enter(|| {});
            foo
        });
        // Even though we enter subscriber 2's context, the subscriber that
        // tagged the span should see the enter/exit.
        subscriber2.as_default(move || foo.enter(|| {}));
    }

    #[test]
    fn spans_always_go_to_the_subscriber_that_tagged_them_even_across_threads() {
        let subscriber1 = subscriber::mock()
            .enter(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")))
            .enter(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")))
            .close(span::mock().named(Some("foo")))
            .done();
        let subscriber1 = Dispatch::new(subscriber1.run());
        let mut foo = subscriber1.as_default(|| {
            let mut foo = span!("foo");
            foo.enter(|| {});
            foo
        });

        // Even though we enter subscriber 2's context, the subscriber that
        // tagged the span should see the enter/exit.
        thread::spawn(move || {
            Dispatch::new(subscriber::mock().run()).as_default(|| {
                foo.enter(|| {});
            })
        }).join()
        .unwrap();
    }

    #[test]
    fn span_closes_on_drop() {
        let (subscriber, handle) = subscriber::mock()
            .enter(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")))
            .close(span::mock().named(Some("foo")))
            .done()
            .run_with_handle();
        Dispatch::new(subscriber).as_default(|| {
            let mut span = span!("foo");
            span.enter(|| {});
            drop(span);
        });

        handle.assert_finished();
    }

    #[test]
    fn span_closes_after_event() {
        let (subscriber, handle) = subscriber::mock()
            .enter(span::mock().named(Some("foo")))
            .event()
            .exit(span::mock().named(Some("foo")))
            .close(span::mock().named(Some("foo")))
            .done()
            .run_with_handle();
        Dispatch::new(subscriber).as_default(|| {
            span!("foo").enter(|| {
                Span::current().close();
                event!(::Level::Debug, {}, "my event!");
            });
        });

        handle.assert_finished();
    }

    #[test]
    fn new_span_after_event() {
        let (subscriber, handle) = subscriber::mock()
            .enter(span::mock().named(Some("foo")))
            .event()
            .exit(span::mock().named(Some("foo")))
            .close(span::mock().named(Some("foo")))
            .enter(span::mock().named(Some("bar")))
            .exit(span::mock().named(Some("bar")))
            .close(span::mock().named(Some("bar")))
            .done()
            .run_with_handle();
        Dispatch::new(subscriber).as_default(|| {
            span!("foo").enter(|| {
                Span::current().close();
                event!(::Level::Debug, {}, "my event!");
            });
            span!("bar").enter(|| {
                Span::current().close();
            });
        });

        handle.assert_finished();
    }

    #[test]
    fn event_outside_of_span() {
        let (subscriber, handle) = subscriber::mock()
            .event()
            .enter(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")))
            .close(span::mock().named(Some("foo")))
            .done()
            .run_with_handle();
        Dispatch::new(subscriber).as_default(|| {
            debug!("my event!");
            span!("foo").enter(|| {
                Span::current().close();
            });
        });

        handle.assert_finished();
    }

    #[test]
    fn dropping_a_span_calls_drop_span() {
        let (subscriber, handle) = subscriber::mock()
            .enter(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")))
            .drop_span(span::mock().named(Some("foo")))
            .drop_span(span::mock().named(Some("foo")))
            .run_with_handle();
        Dispatch::new(subscriber).as_default(|| {
            let mut span = span!("foo");
            span.enter(|| {});
            drop(span);
        });

        handle.assert_finished();
    }

    #[test]
    fn span_current_calls_clone_span() {
        let (subscriber, handle) = subscriber::mock()
            .enter(span::mock().named(Some("foo")))
            .clone_span(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")))
            .run_with_handle();
        Dispatch::new(subscriber).as_default(|| {
            let mut span = span!("foo");
            let _span2 = span.enter(|| Span::current());
        });

        handle.assert_finished();
    }

    #[test]
    fn drop_span_when_exiting_dispatchers_context() {
        let (subscriber, handle) = subscriber::mock()
            .enter(span::mock().named(Some("foo")))
            .clone_span(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")))
            .drop_span(span::mock().named(Some("foo")))
            .drop_span(span::mock().named(Some("foo")))
            .run_with_handle();
        Dispatch::new(subscriber).as_default(|| {
            let mut span = span!("foo");
            let _span2 = span.enter(|| Span::current());
            drop(span);
        });

        handle.assert_finished();
    }

    #[test]
    fn clone_and_drop_span_always_go_to_the_subscriber_that_tagged_the_span() {
        let (subscriber1, handle1) = subscriber::mock()
            .enter(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")))
            .enter(span::mock().named(Some("foo")))
            .clone_span(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")))
            .drop_span(span::mock().named(Some("foo")))
            .drop_span(span::mock().named(Some("foo")))
            .run_with_handle();
        let subscriber1 = Dispatch::new(subscriber1);
        let subscriber2 = Dispatch::new(subscriber::mock().done().run());

        let mut foo = subscriber1.as_default(|| {
            let mut foo = span!("foo");
            foo.enter(|| {});
            foo
        });
        // Even though we enter subscriber 2's context, the subscriber that
        // tagged the span should see the enter/exit.
        subscriber2.as_default(move || {
            let foo2 = foo.enter(|| Span::current());
            drop(foo);
            drop(foo2);
        });

        handle1.assert_finished();
    }

    #[test]
    fn span_closes_when_idle() {
        let (subscriber, handle) = subscriber::mock()
            .enter(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")))
            // A second span is entered so that the mock subscriber will
            // expect "foo" at a separate point in time from when it is exited.
            .enter(span::mock().named(Some("bar")))
            .close(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("bar")))
            .run_with_handle();
        Dispatch::new(subscriber).as_default(|| {
            let mut foo = span!("foo");
            foo.enter(|| {});

            span!("bar").enter(|| {
                // Since `foo` is not executing, it should close immediately.
                foo.close();
            });

            assert!(foo.is_closed());
        });

        handle.assert_finished();
    }

    #[test]
    fn entering_a_closed_span_is_a_no_op() {
        let (subscriber, handle) = subscriber::mock()
            .enter(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")))
            .close(span::mock().named(Some("foo")))
            .done()
            .run_with_handle();
        Dispatch::new(subscriber).as_default(|| {
            let mut foo = span!("foo");
            foo.enter(|| {});

            foo.close();

            foo.enter(|| {
                // The subscriber expects nothing else to happen after the first
                // exit.
            });
            assert!(foo.is_closed());
        });

        handle.assert_finished();
    }

    #[test]
    fn span_doesnt_close_if_it_never_opened() {
        let subscriber = subscriber::mock().done().run();
        Dispatch::new(subscriber).as_default(|| {
            let span = span!("foo");
            drop(span);
        })
    }

    mod shared {
        use super::*;

        #[test]
        fn span_closes_on_drop() {
            let (subscriber, handle) = subscriber::mock()
                .enter(span::mock().named(Some("foo")))
                .exit(span::mock().named(Some("foo")))
                .close(span::mock().named(Some("foo")))
                .done()
                .run_with_handle();
            Dispatch::new(subscriber).as_default(|| span!("foo").into_shared().enter(|| {}));

            handle.assert_finished();
        }

        #[test]
        fn span_doesnt_close_if_it_never_opened() {
            let subscriber = subscriber::mock().done().run();
            Dispatch::new(subscriber).as_default(|| {
                let span = span!("foo").into_shared();
                drop(span);
            })
        }

        #[test]
        fn shared_only_calls_drop_span_when_the_last_handle_is_dropped() {
            let (subscriber, handle) = subscriber::mock()
                .enter(span::mock().named(Some("foo")))
                .exit(span::mock().named(Some("foo")))
                .drop_span(span::mock().named(Some("foo")))
                .run_with_handle();
            Dispatch::new(subscriber).as_default(|| {
                let span = span!("foo").into_shared();
                let foo1 = span.clone();
                let foo2 = span.clone();
                drop(foo1);
                drop(span);
                foo2.enter(|| {})
            });

            handle.assert_finished();
        }

        #[test]
        fn exit_doesnt_finish_concurrently_executing_spans() {
            // Test that exiting a span only marks it as "done" when no other
            // threads are still executing inside that span.
            use std::sync::{Arc, Barrier};

            let (subscriber, handle) = subscriber::mock()
                .enter(span::mock().named(Some("baz")))
                // Main thread enters "quux".
                .enter(span::mock().named(Some("quux")))
                // Spawned thread also enters "quux".
                .enter(span::mock().named(Some("quux")))
                // When the main thread exits "quux", it will still be running in the
                // spawned thread.
                .exit(span::mock().named(Some("quux")))
                // Now, when this thread exits "quux", there is no handle to re-enter it, so
                // it should become "done".
                .exit(span::mock().named(Some("quux")))
                .close(span::mock().named(Some("quux")))
                // "baz" never had more than one handle, so it should also become
                // "done" when we exit it.
                .exit(span::mock().named(Some("baz")))
                .close(span::mock().named(Some("baz")))
                .done()
                .run_with_handle();

            Dispatch::new(subscriber).as_default(|| {
                let barrier1 = Arc::new(Barrier::new(2));
                let barrier2 = Arc::new(Barrier::new(2));
                // Make copies of the barriers for thread 2 to wait on.
                let t2_barrier1 = barrier1.clone();
                let t2_barrier2 = barrier2.clone();

                span!("baz",).enter(move || {
                    let quux = span!("quux",).into_shared();
                    let quux2 = quux.clone();
                    let handle = thread::Builder::new()
                        .name("thread-2".to_string())
                        .spawn(move || {
                            quux2.enter(|| {
                                // Once this thread has entered "quux", allow thread 1
                                // to exit.
                                t2_barrier1.wait();
                                // Wait for the main thread to allow us to exit.
                                t2_barrier2.wait();
                            })
                        }).expect("spawn test thread");
                    quux.enter(|| {
                        // Wait for thread 2 to enter "quux". When we exit "quux", it
                        // should stay running, since it's running in the other thread.
                        barrier1.wait();
                    });
                    // After we exit "quux", wait for the second barrier, so the other
                    // thread unblocks and exits "quux".
                    barrier2.wait();
                    handle.join().unwrap();
                });
            });

            handle.assert_finished();
        }

        #[test]
        fn exit_doesnt_finish_while_handles_still_exist() {
            // Test that exiting a span only marks it as "done" when no handles
            // that can re-enter the span exist.
            let (subscriber, handle) = subscriber::mock()
                .enter(span::mock().named(Some("foo")))
                .enter(span::mock().named(Some("bar")))
                // The first time we exit "bar", there will be another handle with
                // which we could potentially re-enter bar.
                .exit(span::mock().named(Some("bar")))
                // Re-enter "bar", using the cloned handle.
                .enter(span::mock().named(Some("bar")))
                // Now, when we exit "bar", there is no handle to re-enter it, so
                // it should become "done".
                .exit(span::mock().named(Some("bar")))
                .close(span::mock().named(Some("bar")))
                .exit(span::mock().named(Some("foo")))
                .close(span::mock().named(Some("foo")))
                .done()
                .run_with_handle();

            Dispatch::new(subscriber).as_default(|| {
                span!("foo").enter(|| {
                    let bar = span!("bar").into_shared();
                    bar.clone().enter(|| {
                        // do nothing. exiting "bar" should leave it idle, since it can
                        // be re-entered.
                    });
                    // Enter "bar" again. consuming the last handle to `bar`
                    // close the span automatically.
                    bar.enter(|| {});
                });
            });

            handle.assert_finished();
        }
    }
}
