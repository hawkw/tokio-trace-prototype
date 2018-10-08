use super::Filter;
use tokio_trace::Meta;

use std::sync::atomic::{Ordering, AtomicUsize};

pub trait FilterExt: Filter {
    /// Construct a new `Filter` that enables a span or event if both `self`
    /// *AND* `other` consider it enabled.
    fn and<B>(self, other: B) -> And<Self, B>
    where
        B: Filter + Sized,
        Self: Sized
    {
        And {
            a: self,
            b: other,
        }
    }

    /// Construct a new `Filter` that enables a span or event if either `self`
    /// *OR* `other` consider it enabled.
    fn or<B>(self, other: B) -> Or<Self, B>
    where
        B: Filter + Sized,
        Self: Sized
    {
        Or {
            a: self,
            b: other,
        }
    }
}

#[derive(Debug, Clone)]
pub struct And<A, B> {
    a: A,
    b: B,
}

#[derive(Debug, Clone)]
pub struct Or<A, B> {
    a: A,
    b: B,
}

/// A filter that enables some fraction of events.
#[derive(Debug)]
pub struct Sample {
    every: usize,
    count: AtomicUsize,
}

impl<A, B> Filter for And<A, B>
where
    A: Filter,
    B: Filter,
{
    fn enabled(&self, metadata: &Meta) -> bool {
        self.a.enabled(metadata) && self.b.enabled(metadata)
    }

    fn should_invalidate_filter(&self, metadata: &Meta) -> bool {
        // Even though this is the `And` composition, that only applies to the
        // actual filter result, not whether or not the filter needs to be
        // invalidated. If either of the composed filters requests its cached
        // results be invalidated, we need to honor that.
        self.a.should_invalidate_filter(metadata) ||
        self.b.should_invalidate_filter(metadata)
    }
}

impl<A, B> Filter for Or<A, B>
where
    A: Filter,
    B: Filter,
{
    fn enabled(&self, metadata: &Meta) -> bool {
        self.a.enabled(metadata) || self.b.enabled(metadata)
    }

    fn should_invalidate_filter(&self, metadata: &Meta) -> bool {
        self.a.should_invalidate_filter(metadata) ||
        self.b.should_invalidate_filter(metadata)
    }
}

impl Sample {
    /// Construct a new filter that is enabled for every `every` spans/events.
    pub fn every(every: usize) -> Self {
        Self {
            every,
            count: AtomicUsize::new(0),
        }
    }

    // TODO: constructors with ratios, percentages, etc?
}

impl Filter for Sample {
    fn enabled(&self, metadata: &Meta) -> bool {
        // TODO: it would be nice to be able to have a definition of sampling
        // that also enables all the children of a sampled span...figure that out.
        let current = self.count.fetch_add(1, Ordering::Acquire);
        if current % self.every == 0 {
            self.count.store(0, Ordering::Release);
            true
        } else {
            false
        }
    }

    fn should_invalidate_filter(&self, metadata: &Meta) -> bool {
        // The filter _needs_ to be re-evaluated every time, or else the counter
        // won't be updated.
        true
    }
}

impl<F> FilterExt for F
where
    F: Filter
{ }
