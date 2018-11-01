use std::cell::Cell;
use {Dispatch, Meta, Subscriber};

#[doc(hidden)]
pub struct Cache<'a> {
    last_filtered_by: Cell<usize>,
    cached_filter: Cell<Option<bool>>,
    meta: &'a Meta<'a>,
}

impl<'a> Cache<'a> {
    pub fn new(meta: &'a Meta<'a>) -> Self {
        Self {
            last_filtered_by: Cell::new(0),
            cached_filter: Cell::new(None),
            meta,
        }
    }

    #[inline]
    pub fn is_invalid(&self, dispatch: &Dispatch) -> bool {
        let id = dispatch.id();

        // If the callsite was last filtered by a different subscriber, assume
        // the filter is no longer valid.
        if self.cached_filter.get().is_none() || self.last_filtered_by.get() != id {
            // Update the stamp on the call site so this subscriber is now the
            // last to filter it.
            self.last_filtered_by.set(id);
            return true;
        }

        // Otherwise, just ask the subscriber what it thinks.
        dispatch.should_invalidate_filter(self.metadata())
    }

    #[inline]
    pub fn is_enabled(&self, dispatch: &Dispatch) -> bool {
        if self.is_invalid(dispatch) {
            let enabled = dispatch.enabled(self.metadata());
            self.cached_filter.set(Some(enabled));
            enabled
        } else if let Some(cached) = self.cached_filter.get() {
            cached
        } else {
            let enabled = dispatch.enabled(self.metadata());
            self.cached_filter.set(Some(enabled));
            enabled
        }
    }

    #[inline]
    pub fn metadata(&self) -> &'a Meta<'a> {
        self.meta
    }
}
