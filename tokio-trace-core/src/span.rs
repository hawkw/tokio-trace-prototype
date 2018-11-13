//! Spans represent periods of time in the execution of a program.
use std::{
    borrow::Borrow,
    cell::RefCell,
    cmp, fmt,
    hash::{Hash, Hasher},
    sync::atomic::{AtomicBool, AtomicUsize, Ordering},
};
use {
    callsite::Callsite,
    field::{self, Key},
    subscriber::{RecordError, FollowsError, Interest, Subscriber},
    Dispatch, Meta, StaticMeta,
};

thread_local! {
    // TODO: can this be a `Cell`?
    static CURRENT_SPAN: RefCell<Option<Enter>> = RefCell::new(None);
}

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

/// A set of attributes describing a new `Span`.
///
/// This may *not* be used to enter the span.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Attributes {
    /// The span ID of the parent span, or `None` if that span does not exist.
    parent: Option<Id>,

    /// Metadata describing this span.
    metadata: &'static Meta<'static>,
}

/// Identifies a span within the context of a process.
///
/// Span IDs are used primarily to determine of two handles refer to the same
/// span, without requiring the comparison of the span's fields.
///
/// They are generated by [`Subscriber`](::Subscriber)s for each span as it is
/// created, through the [`new_span_id`](::Subscriber::new_span_id) trait
/// method. See the documentation for that method for more information on span
/// ID generation.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Id(u64);

/// A handle representing the capacity to enter a span which is known to exist.
///
/// Unlike `Span`, this type is only constructed for spans which _have_ been
/// enabled by the current filter. This type is primarily used for implementing
/// span handles; users should typically not need to interact with it directly.
#[derive(Debug)]
pub struct Enter {
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

    meta: &'static StaticMeta,
}

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
pub struct Entered {
    prior: Option<Enter>,
}

// ===== impl Span =====

impl Span {
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
            return Span {
                inner: None,
                is_closed: false,
            };
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
            let inner = Some(Enter::new(id, dispatch, parent, meta));
            let mut span = Self {
                inner,
                is_closed: false,
            };
            if_enabled(&mut span);
            span
        })
    }

    /// Returns a reference to the span that this thread is currently
    /// executing.
    // TODO: should the subscriber be responsible for tracking this?
    pub fn current() -> Self {
        Self {
            inner: Enter::clone_current(),
            is_closed: false,
        }
    }

    /// Returns a reference to the dispatcher that tracks this span, or `None`
    /// if the span is disabled.
    pub(crate) fn dispatch(&self) -> Option<&Dispatch> {
        self.inner.as_ref().map(|inner| &inner.subscriber)
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
            Some(inner) => {
                let guard = inner.enter();
                let result = f();
                self.inner = guard.exit();
                result
            }
            None => f(),
        }
    }

    /// Returns the `Id` of the parent of this span, if one exists.
    pub fn parent(&self) -> Option<Id> {
        self.inner.as_ref().and_then(Enter::parent)
    }

    /// Returns a [`Key`](::field::Key) for the field with the given `name`, if
    /// one exists,
    pub fn key_for<Q>(&self, name: &Q) -> Option<Key<'static>>
    where
        Q: Borrow<str>,
    {
        self.inner
            .as_ref()
            .and_then(|inner| inner.meta.key_for(name))
    }

    /// Sets the field on this span named `name` to the given `value`.
    ///
    /// `name` must name a field already defined by this span's metadata, and
    /// the field must not already have a value. If this is not the case, this
    /// function returns an [`RecordError`](::subscriber::RecordError).
    pub fn record<T: field::Value>(&self, field: &Key, value: &T) -> Result<(), RecordError> {
        if let Some(ref inner) = self.inner {
            inner.record(field, value)
        } else {
            Ok(())
        }
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
    /// If this span is disabled, this function will do nothing. Otherwise, it
    /// returns `Ok(())` if the other span was added as a precedent of this
    /// span, or an error if this was not possible.
    pub fn follows_from(&self, from: Id) -> Result<(), FollowsError> {
        self.inner
            .as_ref()
            .map(move |inner| inner.follows_from(from))
            .unwrap_or(Ok(()))
    }

    /// Returns this span's `Id`, if it is enabled.
    pub fn id(&self) -> Option<Id> {
        self.inner.as_ref().map(Enter::id)
    }

    /// Returns this span's `Meta`, if it is enabled.
    pub fn metadata(&self) -> Option<&'static Meta<'static>> {
        self.inner.as_ref().map(|inner| inner.metadata())
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

// ===== impl Attributes =====

impl Attributes {
    fn new(parent: Option<Id>, metadata: &'static StaticMeta) -> Self {
        Attributes { parent, metadata }
    }

    /// Returns the name of this span, or `None` if it is unnamed,
    pub fn name(&self) -> Option<&'static str> {
        self.metadata.name
    }

    /// Returns the `Id` of the parent of this span, if one exists.
    pub fn parent(&self) -> Option<&Id> {
        self.parent.as_ref()
    }

    /// Borrows this span's metadata.
    pub fn metadata(&self) -> &'static StaticMeta {
        self.metadata
    }

    /// Returns an iterator over the names of all the fields on this span.
    pub fn field_keys(&self) -> impl Iterator<Item = Key<'static>> {
        self.metadata.fields()
    }

    /// Returns a [`Key`](::field::Key) for the field with the given `name`, if
    /// one exists,
    pub fn key_for<Q>(&self, name: &Q) -> Option<Key<'static>>
    where
        Q: Borrow<str>,
    {
        self.metadata.key_for(name)
    }

    /// Returns true if a field named 'name' has been declared on this span,
    /// even if the field does not currently have a value.
    #[inline]
    pub fn has_field(&self, key: &Key) -> bool {
        self.metadata.contains_key(key)
    }
}

// ===== impl Id =====

impl Id {
    /// Constructs a new span ID from the given `u64`.
    pub fn from_u64(u: u64) -> Self {
        Id(u)
    }

    /// Returns the ID of the currently-executing span.
    pub fn current() -> Option<Self> {
        CURRENT_SPAN.with(|c| c.borrow().as_ref().map(|c| c.id.clone()))
    }
}

// ===== impl Enter =====

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
        self.subscriber.enter(self.id());
        let prior = CURRENT_SPAN.with(|current_span| current_span.replace(Some(self.duplicate())));
        self.wants_close.store(false, Ordering::Release);
        Entered { prior }
    }

    /// Indicates the span _should_ be closed the next time it exits or this
    /// handle is dropped.
    pub fn close(&self) {
        self.wants_close.store(true, Ordering::Release);
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

    /// Sets the field on this span named `name` to the given `value`.
    ///
    /// `name` must name a field already defined by this span's metadata, and
    /// the field must not already have a value. If this is not the case, this
    /// function returns an [`RecordError`](::subscriber::RecordError).
    pub fn record<T: field::Value>(&self, field: &Key, value: &T) -> Result<(), RecordError> {
        if !self.meta.contains_key(field) {
            return Err(RecordError::NoField);
        }

        match value.record(&field, self.subscriber.span_recorder(&self.id)) {
            Ok(()) => Ok(()),
            Err(RecordError::NoSpan) => panic!("span should still exist!"),
            Err(e) => Err(e),
        }
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
    pub fn follows_from(&self, from: Id) -> Result<(), FollowsError> {
        match self.subscriber.add_follows_from(&self.id, from) {
            Ok(()) => Ok(()),
            Err(FollowsError::NoSpan(ref id)) if id == &self.id => {
                panic!("span {:?} should exist to add a preceeding span", self.id)
            }
            Err(e) => Err(e),
        }
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
    pub fn metadata(&self) -> &'static Meta<'static> {
        self.meta
    }

    fn new(id: Id, subscriber: &Dispatch, parent: Option<Id>, meta: &'static StaticMeta) -> Self {
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

    fn clone_current() -> Option<Self> {
        CURRENT_SPAN.with(|current| {
            current.borrow().as_ref().map(|ref current| {
                current.handles.fetch_add(1, Ordering::Release);
                current.duplicate()
            })
        })
    }

    fn duplicate(&self) -> Self {
        Self {
            id: self.id.clone(),
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

impl cmp::PartialEq for Enter {
    fn eq(&self, other: &Enter) -> bool {
        self.id == other.id
    }
}

impl Hash for Enter {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl Drop for Enter {
    fn drop(&mut self) {
        // If this handle wants to be closed, try to close it --- either by
        // closing it now if it is idle, or telling the current span to close
        // when it exits, if it is the current span.
        if self.has_entered() && self.wants_close() {
            CURRENT_SPAN.with(|c| match *c.borrow() {
                // If the `enter` being dropped corresponds to the same span as
                // the current span, then we cannot close it yet. Instead, we
                // signal to the copy of this span that currently occupies
                // CURRENT_SPAN that it should try to close when it is exited.
                Some(ref current) if current == self => {
                    current.handles.fetch_sub(1, Ordering::Release);
                    current.wants_close.store(true, Ordering::Release);
                }
                // If this span is not the current span, then it may close now,
                // if there are no other handles with the capacity to re-enter it.
                _ if self.handle_count() <= 1 => {
                    self.subscriber.close(self.id());
                }
                _ => {}
            })
        }
    }
}

impl Entered {
    /// Exit the `Entered` guard, returning an `Enter` handle that may be used
    /// to re-enter the span, or `None` if the span closed while performing the
    /// exit.
    pub fn exit(self) -> Option<Enter> {
        CURRENT_SPAN.with(|current_span| {
            let inner = current_span
                .replace(self.prior)
                .expect("cannot exit span that wasn't entered");
            inner.subscriber.exit(inner.id());
            if inner.should_close() {
                // Dropping `inner` will allow it to perform the closure if
                // able.
                None
            } else {
                // We are returning a new `Enter`. Increment the number of
                // handles that may enter the span.
                inner.handles.fetch_add(1, Ordering::Release);
                Some(inner)
            }
        })
    }
}

#[cfg(any(test, feature = "test-support"))]
pub use self::test_support::*;

#[cfg(any(test, feature = "test-support"))]
mod test_support {
    #![allow(missing_docs)]
    use field;
    use std::collections::HashMap;

    /// A mock span.
    ///
    /// This is intended for use with the mock subscriber API in the
    /// `subscriber` module.
    #[derive(Default)]
    pub struct MockSpan {
        pub name: Option<Option<&'static str>>,
        pub fields: HashMap<String, Box<dyn field::Value>>,
        // TODO: more
    }

    pub fn mock() -> MockSpan {
        MockSpan {
            ..MockSpan::default()
        }
    }

    impl MockSpan {
        pub fn named(mut self, name: Option<&'static str>) -> Self {
            self.name = Some(name);
            self
        }

        // TODO: fields, etc
    }
}
