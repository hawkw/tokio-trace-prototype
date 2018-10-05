use super::Filter;
use tokio_trace::Meta;

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

impl<F> FilterExt for F
where
    F: Filter
{ }
