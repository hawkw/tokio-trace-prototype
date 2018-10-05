//! Utilities and helpers for implementing and composing subscribers.

extern crate tokio_trace;

use tokio_trace::{span, Event, Meta, SpanData};

mod compose;
pub use compose::Composed;

pub mod filter;
pub use filter::FilterExt;

pub mod observe;
pub use observe::ObserveExt;

/// The notification processing portion of the [`Subscriber`] trait.
///
/// Implementations of this trait describe the logic needed to process envent
/// and span notifications, but don't implement filtering or span registration.
pub trait Observe {
    fn observe_event<'event, 'meta: 'event>(&self, event: &'event Event<'event, 'meta>);
    fn enter(&self, span: &SpanData);
    fn exit(&self, span: &SpanData);
}

/// The filtering portion of the [`Subscriber`] trait.
///
/// Implementations of this trait represent _just_ the logic necessary to filter
/// events and spans, but none of the processing or registration logic.
pub trait Filter {
    /// Determines if a span or event with the specified metadata would be recorded.
    ///
    /// This is used by the dispatcher to avoid allocating for span construction
    /// if the span would be discarded anyway.
    fn enabled(&self, metadata: &Meta) -> bool;

    /// Returns `true` if the cached result to a call to `enabled` for a span
    /// with the given metadata is still valid.
    ///
    /// By default, this function assumes that cached filter results will remain
    /// valid, but should be overridden when this is not the case.
    ///
    /// If this returns `false`, then the prior value may be used.
    /// `Subscriber`s which require their filters to be run every time an event
    /// occurs or a span is entered/exited should always return `true`.
    ///
    /// For example, suppose a sampling subscriber is implemented by incrementing a
    /// counter every time `enabled` is called and only returning `true` when
    /// the counter is divisible by a specified sampling rate. If that
    /// subscriber returns `false` from `should_invalidate_filter`, then the
    /// filter will not be re-evaluated once it has been applied to a given set
    /// of metadata. Thus, the counter will not be incremented, and the span or
    /// event that correspands to the metadata will never be `enabled`.
    ///
    /// Similarly, if a `Subscriber` has a filtering strategy that can be
    /// changed dynamically at runtime, it would need to invalidate any cached
    /// filter results when the filtering rules change.
    ///
    /// A subscriber which manages fanout to multiple other subscribers should
    /// proxy this decision to all of its child subscribers, returning `false`
    /// only if _all_ such children return `false`. If the set of subscribers to
    /// which spans are broadcast may change dynamically, adding a new
    /// subscriber should also invalidate cached filters.
    fn should_invalidate_filter(&self, metadata: &Meta) -> bool;
}

/// The span registration portion of the [`Subscriber`] trait.
///
/// Implementations of this trait represent the logic run on span creation. They
/// handle span ID generation.
pub trait RegisterSpan {
    /// Record the construction of a new [`Span`], returning a a new [span ID] for
    /// the span being constructed.
    ///
    /// Span IDs are used to uniquely identify spans, so span equality will be
    /// based on the returned ID. Thus, if the subscriber wishes for all spans
    /// with the same metadata to be considered equal, it should return the same
    /// ID every time it is given a particular set of metadata. Similarly, if it
    /// wishes for two separate instances of a span with the same metadata to *not*
    /// be equal, it should return a distinct ID every time this function is called,
    /// regardless of the metadata.
    ///
    /// Subscribers which do not rely on the implementations of `PartialEq`,
    /// `Eq`, and `Hash` for `Span`s are free to return span IDs with value 0
    /// from all calls to this function, if they so choose.
    ///
    /// [span ID]: ../span/struct.Id.html
    fn new_span(&self, new_span: &span::NewSpan) -> span::Id;
}
