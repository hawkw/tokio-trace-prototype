use {span, subscriber::Subscriber, Event, SpanData, Meta};

use std::{
    cell::RefCell,
    fmt,
    sync::Arc,
};

thread_local! {
    static CURRENT_DISPATCH: RefCell<Dispatch> = RefCell::new(Dispatch::none());
}

#[derive(Clone)]
pub struct Dispatch(Arc<dyn Subscriber + Send + Sync>);

impl Dispatch {
    pub fn none() -> Self {
        Dispatch(Arc::new(NoSubscriber))
    }

    pub fn current() -> Dispatch {
        CURRENT_DISPATCH.with(|current| {
            current.borrow().clone()
        })
    }

    pub fn to<S>(subscriber: S) -> Self
    // TODO: Add some kind of `UnsyncDispatch`?
    where
        S: Subscriber + Send + Sync + 'static,
    {
        Dispatch(Arc::new(subscriber))
    }

    pub fn with<T>(&self, f: impl Fn() -> T) -> T {
        let (prior, result) = CURRENT_DISPATCH.with(|current| {
            let prior = current.replace(self.clone());
            (prior, (f)())
        });
        CURRENT_DISPATCH.with(move |current| {
            *current.borrow_mut() = prior;
        });
        result
    }
}

impl fmt::Debug for Dispatch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.pad("Dispatch(...)")
    }
}

impl Subscriber for Dispatch {
    #[inline]
    fn enabled(&self, metadata: &Meta) -> bool {
        self.0.enabled(metadata)
    }

    #[inline]
    fn new_span(&self, new_span: &span::NewSpan) -> span::Id {
        self.0.new_span(new_span)
    }

    #[inline]
    fn observe_event<'event, 'meta: 'event>(&self, event: &'event Event<'event, 'meta>) {
        self.0.observe_event(event)
    }

    #[inline]
    fn enter(&self, span: &SpanData) {
        self.0.enter(span)
    }

    #[inline]
    fn exit(&self, span: &SpanData) {
        self.0.exit(span)
    }
}

struct NoDispatcher;

#[derive(Debug)]
pub struct InitError;

impl Subscriber for NoDispatcher {
    fn enabled(&self, _metadata: &Meta) -> bool {
        false
    }

    fn new_span(&self, _new_span: &span::NewSpan) -> span::Id {
        span::Id::from_u64(0)
    }

    fn observe_event<'event, 'meta: 'event>(&self, _event: &'event Event<'event, 'meta>) {
        // Do nothing.
    }

    fn enter(&self, _span: &SpanData) {}

    fn exit(&self, _span: &SpanData) {}
}
