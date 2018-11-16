use tokio_trace::{
    field, span,
    subscriber::{FollowsError, RecordError, Subscriber},
    Event, Meta, Id,
};
use {filter::NoFilter, observe::NoObserver, Filter, Observe, RegisterSpan};

#[derive(Debug, Clone)]
pub struct Composed<F, O, R> {
    filter: F,
    observer: O,
    registry: R,
}

impl Composed<NoFilter, NoObserver, ()> {
    /// Returns a new instance of `Composed` which can be built using the
    /// [`with_filter`], [`with_observer`], and [`with_registry`] methods.
    pub fn builder() -> Self {
        Composed {
            filter: NoFilter,
            observer: NoObserver,
            registry: (),
        }
    }
}

impl<O, R> Composed<NoFilter, O, R> {
    /// Sets the [filter] to be used by the composed `Subscriber`.
    ///
    /// [filter]: ../trait.Filter.html
    pub fn with_filter<F>(self, filter: F) -> Composed<F, O, R>
    where
        F: Filter,
    {
        Composed {
            filter,
            observer: self.observer,
            registry: self.registry,
        }
    }
}

impl<F, R> Composed<F, NoObserver, R> {
    /// Sets the [observer] to be used by the composed `Subscriber`.
    ///
    /// [observer]: ../trait.Observe.html
    pub fn with_observer<O>(self, observer: O) -> Composed<F, O, R>
    where
        O: Observe,
    {
        Composed {
            filter: self.filter,
            observer,
            registry: self.registry,
        }
    }
}

impl<F, O> Composed<F, O, ()> {
    /// Sets the [span registry] to be used by the composed `Subscriber`.
    ///
    /// [span registry]: ../trait.Register.html
    pub fn with_registry<R>(self, registry: R) -> Composed<F, O, R>
    where
        R: RegisterSpan,
    {
        Composed {
            filter: self.filter,
            observer: self.observer,
            registry,
        }
    }
}

impl<F, O, R> Composed<F, O, R> {
    /// Construct a new composed `Subscriber`, given a [filter], an
    /// [observer], and a [span registry].
    ///
    /// [filter]: ../trait.Filter.html
    /// [observer]: ../trait.Observe.html
    /// [span registry]: ../trait.Register.html
    pub fn new(filter: F, observer: O, registry: R) -> Self {
        Composed {
            filter,
            observer,
            registry,
        }
    }
}

impl<F, O, R> Subscriber for Composed<F, O, R>
where
    F: Filter,
    O: Observe,
    R: RegisterSpan,
{
    fn enabled(&self, metadata: &Meta) -> bool {
        self.filter.enabled(metadata) && self.observer.filter().enabled(metadata)
    }

    fn new_span(&self, new_span: span::SpanAttributes) -> Id {
        self.registry.new_span(new_span)
    }

    fn new_id(&self, new_id: span::Attributes) -> Id {
        self.registry.new_id(new_id)
    }

    fn record_fmt(
        &self,
        _span: &Id,
        _name: &tokio_trace::field::Key,
        _value: ::std::fmt::Arguments,
    ) -> Result<(), RecordError> {
        unimplemented!()
    }

    fn add_follows_from(&self, span: &Id, follows: Id) -> Result<(), FollowsError> {
        self.registry.add_follows_from(span, follows)
    }

    fn enter(&self, id: Id) {
        self.registry.with_span(&id, |span| {
            self.observer.enter(span);
        });
    }

    fn exit(&self, id: Id) {
        self.registry.with_span(&id, |span| {
            self.observer.exit(span);
        });
    }

    fn close(&self, id: Id) {
        self.registry.with_span(&id, |span| {
            self.observer.close(span);
        });
    }

    fn clone_span(&self, id: Id) -> Id {
        self.registry.clone_span(id)
    }

    fn drop_span(&self, id: Id) {
        self.registry.drop_span(id)
    }
}
