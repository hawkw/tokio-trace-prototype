use ::{Observe, Filter};
use tokio_trace::{Event, SpanData, Meta};

pub trait ObserveExt: Observe {
    /// Construct a new observer that sends events to both `self` and `other.
    fn tee_to<I>(self, other: I) -> Tee<Self, I::Observer>
    where
        I: IntoObserver,
        Self: Sized
    {
        Tee {
            a: self,
            b: other.into_observer(),
        }
    }

    /// Construct a new observer that filters events with the given `filter`.
    fn with_filter<F>(self, filter: F) -> WithFilter<Self, F>
    where
        F: Filter,
        Self: Sized,
    {
        WithFilter {
            inner: self,
            filter
        }
    }
}

pub trait IntoObserver {
    type Observer: Observe;
    fn into_observer(self) -> Self::Observer;
}

pub struct NoObserver;

#[derive(Copy, Clone)]
pub enum Either<A, B> {
    A(A),
    B(B),
}

#[derive(Copy, Clone)]
pub struct Tee<A, B> {
    a: A,
    b: B,
}

#[derive(Debug, Clone)]
pub struct WithFilter<O, F> {
    inner: O,
    filter: F
}

impl<O, F> Filter for WithFilter<O, F>
where
    O: Observe,
    F: Filter,
{
    #[inline]
    fn enabled(&self, metadata: &Meta) -> bool {
        self.filter.enabled(metadata) && self.inner.filter().enabled(metadata)
    }

    #[inline]
    fn should_invalidate_filter(&self, metadata: &Meta) -> bool {
         self.filter.should_invalidate_filter(metadata) ||
         self.inner.filter().should_invalidate_filter(metadata)
    }
}

impl<O, F> Observe for WithFilter<O, F>
where
    O: Observe,
    F: Filter,
{
    #[inline]
    fn observe_event<'event, 'meta: 'event>(&self, event: &'event Event<'event, 'meta>) {
        self.inner.observe_event(event)
    }

    #[inline]
    fn enter(&self, span: &SpanData) {
        self.inner.enter(span)
    }

    #[inline]
    fn exit(&self, span: &SpanData) {
        self.inner.exit(span)
    }

    fn filter(&self) -> &dyn Filter {
        self
    }
}

pub fn none() -> NoObserver {
    NoObserver
}

impl<T> ObserveExt for T
where
    T: Observe,
{ }

impl<T> IntoObserver for T
where
    T: Observe,
{
    type Observer = Self;
    fn into_observer(self) -> Self::Observer {
        self
    }
}

// XXX: maybe this should just be an impl of `Observe` for tuples of `(Observe, Observe)`...?
impl<A, B> Observe for Tee<A, B>
where
    A: Observe,
    B: Observe,
{
    fn observe_event<'event, 'meta: 'event>(&self, event: &'event Event<'event, 'meta>) {
        self.a.observe_event(event);
        self.b.observe_event(event);
    }

    fn enter(&self, span: &SpanData) {
        self.a.enter(span);
        self.b.enter(span);
    }

    fn exit(&self, span: &SpanData) {
        self.a.exit(span);
        self.b.exit(span);
    }

    fn filter(&self) -> &dyn Filter {
        self
    }
}

impl<A, B> Filter for Tee<A, B>
where
    A: Observe,
    B: Observe,
{
    fn enabled(&self, metadata: &Meta) -> bool {
        self.a.filter().enabled(metadata) || self.b.filter().enabled(metadata)
    }

    fn should_invalidate_filter(&self, metadata: &Meta) -> bool {
        self.a.filter().should_invalidate_filter(metadata) ||
        self.b.filter().should_invalidate_filter(metadata)
    }
}

impl<A, B> Observe for Either<A, B>
where
    A: Observe,
    B: Observe,
{
    fn observe_event<'event, 'meta: 'event>(&self, event: &'event Event<'event, 'meta>) {
        match self {
            Either::A(a) => a.observe_event(event),
            Either::B(b) => b.observe_event(event),
        }
    }

    fn enter(&self, span: &SpanData) {
        match self {
            Either::A(a) => a.enter(span),
            Either::B(b) => b.enter(span),
        }
    }

    fn exit(&self, span: &SpanData) {
        match self {
            Either::A(a) => a.exit(span),
            Either::B(b) => b.exit(span),
        }
    }
}

impl<A, B> Filter for Either<A, B>
where
    A: Observe,
    B: Observe,
{
    fn enabled(&self, metadata: &Meta) -> bool {
        match self {
            Either::A(a) => a.filter().enabled(metadata),
            Either::B(b) => b.filter().enabled(metadata),
        }
    }

    fn should_invalidate_filter(&self, metadata: &Meta) -> bool {
        match self {
            Either::A(a) => a.filter().should_invalidate_filter(metadata),
            Either::B(b) => b.filter().should_invalidate_filter(metadata),
        }
    }
}

impl Observe for NoObserver {
    fn observe_event<'event, 'meta: 'event>(&self, _event: &'event Event<'event, 'meta>) { }

    fn enter(&self, _span: &SpanData) { }

    fn exit(&self, _span: &SpanData) { }

    fn filter(&self) -> &dyn Filter {
        self
    }
}

impl Filter for NoObserver {
    fn enabled(&self, metadata: &Meta) -> bool {
        false
    }

    fn should_invalidate_filter(&self, metadata: &Meta) -> bool {
        false
    }
}
