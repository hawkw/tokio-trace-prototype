//! Spans represent periods of time in the execution of a program.
use std::{
    cell::RefCell,
    cmp, fmt,
    hash::{Hash, Hasher},
    iter, slice,
    sync::atomic::{AtomicUsize, AtomicBool, Ordering},
};
use {
    subscriber::{AddValueError, FollowsError, Subscriber},
    value::{IntoValue, OwnedValue},
    DebugFields, Dispatch, StaticMeta,
};

thread_local! {
    // TODO: can this be a `Cell`?
    static CURRENT_SPAN: RefCell<Option<Enter>> = RefCell::new(None);
}

/// A handle that represents a span in the process of executing.
#[derive(PartialEq, Hash)]
pub struct Span {
    inner: Option<Enter>,
    is_closed: bool,
}

/// Representation of the data associated with a span.
///
/// This has the potential to outlive the span itself if it exists after the
/// span completes executing --- such as if it is still being processed by a
/// subscriber.
///
/// This may *not* be used to enter the span.
pub struct Data {
    /// The span ID of the parent span, or `None` if that span does not exist.
    pub parent: Option<Id>,

    /// Metadata describing this span.
    pub static_meta: &'static StaticMeta,

    /// The values of the fields attached to this span.
    ///
    /// These may be `None` if a field was defined but the value has yet to be
    /// attached. The name of the field at each index is defined by
    /// `self.static_meta.field_names[i]`.
    pub field_values: Vec<Option<OwnedValue>>,
}

/// Identifies a span within the context of a process.
///
/// Span IDs are used primarily to determine of two handles refer to the same
/// span, without requiring the comparison of the span's fields.
///
/// They are generated by [`Subscriber`](::Subscriber)s for each span as it is created, through
/// the [`new_span_id`](::Subscriber::new_span_id) trait method. See the documentation for that
/// method for more information on span ID generation.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Id(u64);

/// Trait representing something which is associated with a span `Id`.
///
/// This is used primarily to allow [`Span::follows`](::Span::follows) to accept both `Span`s and
/// `Id`s as valid arguments.
pub trait AsId {
    /// Returns the span `Id` that `self` is associated with, or `None` if that
    /// span is disabled.
    fn as_id(&self) -> Option<Id>;
}

/// Internal representation of the inner state of a span which has not yet
/// completed.
///
/// This is kept separate from the `Data`, which holds the data about the
/// span, because this type is referenced only by *entering* (`Span`) handles.
/// It is only necessary to track this state while the capacity still exists to
/// re-enter the span; once it can no longer be re-entered, the `ActiveInner`
/// can be dropped (and *should* be dropped, as this may allow the parent span
/// to finish as well, if the `ActiveInner` holds the only remaining entering
/// reference to the parent span).
///
/// This type is purely internal to the `span` module and is not intended to be
/// interacted with directly by downstream users of `tokio-trace`. Instead, all
/// interaction with an active span's state is carried out through `Span`
/// references.
#[derive(Debug)]
pub(crate) struct Enter {
    id: Id,

    subscriber: Dispatch,

    parent: Option<Id>,

    wants_close: AtomicBool,

    has_entered: AtomicBool,

    handles: AtomicUsize,
}

#[derive(Debug)]
#[must_use = "once a span has been entered, it should be exited"]
struct Entered {
    prior: Option<Enter>,
}

// ===== impl Span =====

impl Span {
    #[doc(hidden)]
    pub fn new(dispatch: Dispatch, static_meta: &'static StaticMeta) -> Span {
        let parent = Id::current();
        let data = Data::new(parent.clone(), static_meta);
        let id = dispatch.new_span(data);
        let inner = Some(Enter::new(id, dispatch, parent));
        Self {
            inner,
            is_closed: false,
        }
    }

    /// This is primarily used by the `span!` macro, so it has to be public,
    /// but it's not intended for use by consumers of the tokio-trace API
    /// directly.
    #[doc(hidden)]
    pub fn new_disabled() -> Self {
        Span {
            inner: None,
            is_closed: false,
        }
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

    /// Sets the field on this span named `name` to the given `value`.
    ///
    /// `name` must name a field already defined by this span's metadata, and
    /// the field must not already have a value. If this is not the case, this
    /// function returns an [`AddValueError`](::subscriber::AddValueError).
    pub fn add_value(
        &self,
        field: &'static str,
        value: &dyn IntoValue,
    ) -> Result<(), AddValueError> {
        if let Some(ref inner) = self.inner {
            match inner.subscriber.add_value(&inner.id, field, value) {
                Ok(()) => Ok(()),
                Err(AddValueError::NoSpan) => panic!("span should still exist!"),
                Err(e) => Err(e),
            }
        } else {
            // If the span doesn't exist, silently do nothing.
            Ok(())
        }
    }

    /// Signals that this span should close the next time it is exited, or when
    /// it is dropped.
    pub fn close(&mut self) {
        self.is_closed = true;
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
    pub fn follows_from<I: AsId>(&self, from: I) -> Result<(), FollowsError> {
        if let Some(ref inner) = self.inner {
            let from_id = from.as_id().ok_or(FollowsError::NoPreceedingId)?;
            match inner.subscriber.add_follows_from(&inner.id, from_id) {
                Ok(()) => Ok(()),
                Err(FollowsError::NoSpan(ref id)) if id == &inner.id => {
                    panic!("span {:?} should exist to add a preceeding span", inner.id)
                }
                Err(e) => Err(e),
            }
        } else {
            // If the span doesn't exist, silently do nothing.
            Ok(())
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

impl AsId for Span {
    fn as_id(&self) -> Option<Id> {
        self.inner.as_ref().map(Enter::id)
    }
}

// ===== impl Data =====

impl Data {
    fn new(parent: Option<Id>, static_meta: &'static StaticMeta) -> Self {
        // Preallocate enough `None`s to hold the unset state of every field
        // name.
        let field_values = iter::repeat(())
            .map(|_| None)
            .take(static_meta.field_names.len())
            .collect();
        Data {
            parent,
            static_meta,
            field_values,
        }
    }

    /// Returns the name of this span, or `None` if it is unnamed,
    pub fn name(&self) -> Option<&'static str> {
        self.static_meta.name
    }

    /// Returns the `Id` of the parent of this span, if one exists.
    pub fn parent(&self) -> Option<&Id> {
        self.parent.as_ref()
    }

    /// Borrows this span's metadata.
    pub fn meta(&self) -> &'static StaticMeta {
        self.static_meta
    }

    /// Returns an iterator over the names of all the fields on this span.
    pub fn field_names<'a>(&self) -> slice::Iter<&'a str> {
        self.static_meta.field_names.iter()
    }

    /// Returns true if a field named 'name' has been declared on this span,
    /// even if the field does not currently have a value.
    pub fn has_field<Q>(&self, key: Q) -> bool
    where
        &'static str: PartialEq<Q>,
    {
        self.field_names().any(|&name| name == key)
    }

    /// Borrows the value of the field named `name`, if it exists. Otherwise,
    /// returns `None`.
    pub fn field<Q>(&self, key: Q) -> Option<&OwnedValue>
    where
        &'static str: PartialEq<Q>,
    {
        self.field_names()
            .position(|&field_name| field_name == key)
            .and_then(|i| self.field_values.get(i)?.as_ref())
    }

    /// Returns an iterator over all the field names and values on this span.
    pub fn fields<'a>(&'a self) -> impl Iterator<Item = (&'a str, &'a OwnedValue)> {
        self.field_names()
            .filter_map(move |&name| self.field(name).map(move |val| (name, val)))
    }

    /// Edits the span data to add the given `value` to the field named `name`.
    ///
    /// `name` must name a field already defined by this span's metadata, and
    /// the field must not already have a value. If this is not the case, this
    /// function returns an [`AddValueError`](::subscriber::AddValueError).
    pub fn add_value(
        &mut self,
        name: &'static str,
        value: &dyn IntoValue,
    ) -> Result<(), AddValueError> {
        if let Some(i) = self
            .field_names()
            .position(|&field_name| field_name == name)
        {
            let field = &mut self.field_values[i];
            if field.is_some() {
                Err(AddValueError::FieldAlreadyExists)
            } else {
                *field = Some(value.into_value());
                Ok(())
            }
        } else {
            Err(AddValueError::NoField)
        }
    }

    /// Returns a struct that can be used to format all the fields on this
    /// span with `fmt::Debug`.
    pub fn debug_fields<'a>(&'a self) -> DebugFields<'a, Self, &'a OwnedValue> {
        DebugFields(self)
    }
}

impl<'a> IntoIterator for &'a Data {
    type Item = (&'a str, &'a OwnedValue);
    type IntoIter = Box<Iterator<Item = (&'a str, &'a OwnedValue)> + 'a>; // TODO: unbox
    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.fields())
    }
}

impl fmt::Debug for Data {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Span")
            .field("name", &self.name())
            .field("parent", &self.parent)
            .field("fields", &self.debug_fields())
            .field("meta", &self.meta())
            .finish()
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

impl AsId for Id {
    fn as_id(&self) -> Option<Id> {
        Some(self.clone())
    }
}

// ===== impl Enter =====

impl Enter {
    fn new(id: Id, subscriber: Dispatch, parent: Option<Id>) -> Self {
       Self {
            id,
            subscriber,
            parent,
            wants_close: AtomicBool::from(false),
            has_entered: AtomicBool::from(false),
            handles: AtomicUsize::from(1),
        }
    }

    pub(crate) fn clone_current() -> Option<Self> {
        CURRENT_SPAN.with(|current| {
            current.borrow().as_ref().map(|ref current| {
                current.handles.fetch_add(1, Ordering::Release);
                Self {
                    id: current.id.clone(),
                    subscriber: current.subscriber.clone(),
                    parent: current.parent.clone(),
                    wants_close: AtomicBool::from(current.wants_close()),
                    has_entered: AtomicBool::from(current.has_entered()),
                    handles: AtomicUsize::from(current.handle_count()),
                }
            })
        })
    }

    fn enter(self) -> Entered {
        self.handles.fetch_sub(1, Ordering::Release);
        self.has_entered.store(true, Ordering::Release);
        self.subscriber.enter(self.id());
        let prior = CURRENT_SPAN.with(|current_span| {
            current_span.replace(Some(self))
        });
        Entered {
            prior,
        }
    }

    fn close(&self) {
        self.wants_close.store(true, Ordering::Release);
    }

    fn should_close(&self) -> bool {
        self.wants_close() && self.has_entered() && self.handle_count() == 1
    }

    fn has_entered(&self) -> bool {
        self.has_entered.load(Ordering::Acquire)
    }

    fn wants_close(&self) -> bool {
        self.wants_close.load(Ordering::Acquire)
    }

    fn handle_count(&self) -> usize {
        self.handles.load(Ordering::Acquire)
    }

    fn id(&self) -> Id {
        self.id.clone()
    }

    fn parent(&self) -> Option<Id> {
        self.parent.clone()
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
        if self.has_entered() && self.wants_close() {
            CURRENT_SPAN.with(|c| match *c.borrow() {
                Some(ref current) if current == self => {
                    current.handles.fetch_sub(1, Ordering::Release);
                    current.wants_close.store(true, Ordering::Release);
                }
                _ if self.handle_count() <= 1 => {
                    self.subscriber.close(self.id());
                }
                _ => {},
            })

        }
    }
}

impl Entered {
    fn exit(self) -> Option<Enter> {
        CURRENT_SPAN.with(|current_span| {
            let inner = current_span.replace(self.prior)
                .expect("cannot exit span that wasn't entered");
            inner.subscriber.exit(inner.id());
            if inner.should_close() {
                // Dropping `inner` will allow it to perform the closure if
                // able.
                None
            } else {
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
    use std::collections::HashMap;
    use value::OwnedValue;

    /// A mock span.
    ///
    /// This is intended for use with the mock subscriber API in the
    /// `subscriber` module.
    #[derive(Default)]
    pub struct MockSpan {
        pub name: Option<Option<&'static str>>,
        pub fields: HashMap<String, Box<OwnedValue>>,
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
