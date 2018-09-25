//! Adapters for connecting unstructured log records from the `log` crate into
//! the `tokio_trace` ecosystem.
extern crate tokio_trace;
extern crate log;
extern crate env_logger;

use std::{io, time::Instant};
use tokio_trace::subscriber::Subscriber;

pub fn try_init() -> Result<(), log::SetLoggerError> {
    env_logger::Builder::from_default_env()
        .format(|_, record| format_trace(record))
        .try_init()
}

pub fn init() {
    try_init().unwrap()
}

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
