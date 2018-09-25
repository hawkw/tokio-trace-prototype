//! Adapters for connecting unstructured log records from the `log` crate into
//! the `tokio_trace` ecosystem.
extern crate tokio_trace;
extern crate log;

use std::{io, time::Instant};
use tokio_trace::Subscriber;

pub fn format_trace(record: &log::Record) -> io::Result<()> {
    let parent = tokio_trace::Span::current();
    let meta: tokio_trace::Meta = record.into();
    let event = tokio_trace::Event {
        timestamp: Instant::now(),
        parent,
        follows_from: &[],
        meta: &meta,
        field_values: &[],
        message: record.args().clone()
    };
    tokio_trace::Dispatcher::current().observe_event(&event);
    Ok(())
}
