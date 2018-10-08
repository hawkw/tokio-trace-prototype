use tokio_trace::span::{NewSpan, Id};

use std::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};

/// Registers new span IDs with an increasing `usize` counter.
///
/// This should not be used on 32-bit machines.
pub fn increasing_counter(_new_span: &NewSpan) -> Id {
    static NEXT_ID: AtomicUsize = ATOMIC_USIZE_INIT;
    let next = NEXT_ID.fetch_add(1, Ordering::SeqCst);
    Id::from_u64(next as u64)
}
