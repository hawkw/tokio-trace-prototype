use super::Observe;
use tokio_trace::{Event, SpanData};

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

impl Observe for NoObserver {
    fn observe_event<'event, 'meta: 'event>(&self, _event: &'event Event<'event, 'meta>) { }

    fn enter(&self, _span: &SpanData) { }

    fn exit(&self, _span: &SpanData) { }
}
