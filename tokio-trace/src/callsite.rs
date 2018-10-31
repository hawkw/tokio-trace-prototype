use std::cell::Cell;
use ::{
    Dispatch,
    Meta,
};

/// Caches callsite-specific data.
pub struct Callsite<'a> {
    last_filtered_by: Cell<usize>,
    filtered: Cell<Filtered>,
    meta: &'a Meta<'a>,
}

enum Filtered {
    Unknown,
    Enabled,
    Disabled,
}

impl<'a> Callsite<'a> {
    pub const fn new(meta: &'a Meta<'a>) -> Self {
        Self {
            last_filtered_by: Cell::new(0),
            filtered: Cell::new(Filtered::Unknown),
            meta,
        }
    }

    #[inline]
    pub fn is_enabled(&self, dispatch: &Dispatch) -> bool {
        let id = dispatch.id();

        // If the callsite was last filtered by a different subscriber, assume
        // the filter is no longer valid.
        if self.last_filtered_by.get() != id {
            // Update the stamp on the call site so this subscriber is now the
            // last to filter it.
            self.last_filtered_by.set(id);
            return true;
        }

        // Otherwise, just ask the subscriber what it thinks.
        dispatch.should_invalidate_filter(&self.meta)
    }

    #[inline]
    pub fn metadata(&self) -> &'a Meta<'a> {
        self.meta
    }
}
