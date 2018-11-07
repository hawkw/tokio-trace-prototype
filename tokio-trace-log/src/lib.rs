//! Adapters for connecting unstructured log records from the `log` crate into
//! the `tokio_trace` ecosystem.
//!
//! This conversion does not convert unstructured data in log records (such as
//! values passed as format arguments to the `log!` macro) to structured
//! `tokio_trace` fields. However, it *does* attach these new events to to the
//! span that was currently executing when the record was logged. This is the
//! primary use-case for this library: making it possible to locate the log
//! records emitted by dependencies which use `log` within the context of a
//! trace.
//!
//! Note that logger implementations that convert log records to trace events
//! should not be used with `Subscriber`s that convert trace events _back_ into
//! log records (such as the `TraceLogger`), as doing so will result in the
//! event recursing between the subscriber and the logger forever (or, in real
//! life, probably overflowing the call stack).
//!
//! If the logging of trace events generated from log records produced by the
//! `log` crate is desired, either the `log` crate should not be used to
//! implement this logging, or an additional layer of filtering will be
//! required to avoid infinitely converting between `Event` and `log::Record`.
extern crate log;
extern crate tokio_trace;
extern crate tokio_trace_subscriber;

use std::{
    fmt, io,
    sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT},
};
use tokio_trace::{
    field,
    span,
    subscriber::{self, Subscriber},
    Event, IntoValue, Meta,
};
use tokio_trace_subscriber::SpanRef;

/// Format a log record as a trace event in the current span.
pub fn format_trace(record: &log::Record) -> io::Result<()> {
    let meta: tokio_trace::Meta = record.as_trace();
    let event = Event {
        parent: tokio_trace::SpanId::current(),
        follows_from: &[],
        meta: &meta,
        field_values: &[],
        message: record.args().clone(),
    };
    tokio_trace::Dispatch::current().observe_event(&event);
    Ok(())
}

pub trait AsLog {
    type Log;
    fn as_log(&self) -> Self::Log;
}

pub trait AsTrace {
    type Trace;
    fn as_trace(&self) -> Self::Trace;
}

impl<'a> AsLog for Meta<'a> {
    type Log = log::Metadata<'a>;
    fn as_log(&self) -> Self::Log {
        log::Metadata::builder()
            .level(self.level.as_log())
            .target(self.target)
            .build()
    }
}

impl<'a> AsTrace for log::Record<'a> {
    type Trace = Meta<'a>;
    fn as_trace(&self) -> Self::Trace {
        Meta::new_event(
            self.target(),
            self.level().as_trace(),
            self.module_path(),
            self.file(),
            self.line(),
            &[],
        )
    }
}

impl AsLog for tokio_trace::Level {
    type Log = log::Level;
    fn as_log(&self) -> log::Level {
        match self {
            tokio_trace::Level::Error => log::Level::Error,
            tokio_trace::Level::Warn => log::Level::Warn,
            tokio_trace::Level::Info => log::Level::Info,
            tokio_trace::Level::Debug => log::Level::Debug,
            tokio_trace::Level::Trace => log::Level::Trace,
        }
    }
}

impl AsTrace for log::Level {
    type Trace = tokio_trace::Level;
    fn as_trace(&self) -> tokio_trace::Level {
        match self {
            log::Level::Error => tokio_trace::Level::Error,
            log::Level::Warn => tokio_trace::Level::Warn,
            log::Level::Info => tokio_trace::Level::Info,
            log::Level::Debug => tokio_trace::Level::Debug,
            log::Level::Trace => tokio_trace::Level::Trace,
        }
    }
}

/// A simple "logger" that converts all log records into `tokio_trace` `Event`s,
/// with an optional level filter.
#[derive(Debug)]
pub struct LogTracer {
    filter: log::LevelFilter,
}

/// A `tokio_trace_subscriber::Observe` implementation that logs all recorded
/// trace events.
pub struct TraceLogger;

struct LogFields<'a, I: 'a, T: 'a>(&'a I)
where
    &'a I: IntoIterator<Item = (&'a str, T)>;

// ===== impl LogTracer =====

impl LogTracer {
    pub fn with_filter(filter: log::LevelFilter) -> Self {
        Self { filter }
    }
}

impl Default for LogTracer {
    fn default() -> Self {
        Self::with_filter(log::LevelFilter::Info)
    }
}

impl log::Log for LogTracer {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        metadata.level() <= self.filter
    }

    fn log(&self, record: &log::Record) {
        format_trace(record).unwrap();
    }

    fn flush(&self) {}
}

// ===== impl TraceLogger =====

impl TraceLogger {
    pub fn new() -> Self {
        TraceLogger
    }
}

impl Subscriber for TraceLogger {
    fn enabled(&self, metadata: &Meta) -> bool {
        log::logger().enabled(&metadata.as_log())
    }

    fn new_span(&self, new_span: span::Data) -> span::Id {
        static NEXT_ID: AtomicUsize = ATOMIC_USIZE_INIT;
        let id = span::Id::from_u64(NEXT_ID.fetch_add(1, Ordering::SeqCst) as u64);
        let meta = new_span.meta();
        let logger = log::logger();
        logger.log(
            &log::Record::builder()
                .metadata(meta.as_log())
                .module_path(meta.module_path)
                .file(meta.file)
                .line(meta.line)
                .args(format_args!(
                    "new_span: {}; span={:?}; parent={:?}; {:?}",
                    meta.name.unwrap_or(""),
                    id,
                    new_span.parent(),
                    LogFields(&new_span),
                )).build(),
        );
        id
    }

    fn add_value(
        &self,
        _span: &span::Id,
        _name: &field::Key,
        _value: &dyn IntoValue,
    ) -> Result<(), subscriber::AddValueError> {
        // XXX eventually this should Do Something...
        Ok(())
    }

    fn add_follows_from(
        &self,
        span: &span::Id,
        follows: span::Id,
    ) -> Result<(), subscriber::FollowsError> {
        // TODO: this should eventually track the relationship?
        log::logger().log(
            &log::Record::builder()
                .level(log::Level::Trace)
                .args(format_args!("span {:?} follows_from={:?};", span, follows))
                .build(),
        );
        Ok(())
    }

    fn observe_event<'a>(&self, event: &'a Event<'a>) {
        let meta = event.meta.as_log();
        let logger = log::logger();
        if logger.enabled(&meta) {
            logger.log(
                &log::Record::builder()
                    .metadata(meta)
                    .module_path(event.meta.module_path)
                    .file(event.meta.file)
                    .line(event.meta.line)
                    .args(format_args!(
                        "{}; in_span={:?}; {:?}",
                        event.message,
                        event.parent,
                        LogFields(event),
                    )).build(),
            );
        }
    }

    fn enter(&self, span: span::Id) {
        let logger = log::logger();
        logger.log(
            &log::Record::builder()
                .level(log::Level::Trace)
                .args(format_args!("enter: span={:?};", span))
                .build(),
        );
    }

    fn exit(&self, span: span::Id) {
        let logger = log::logger();
        logger.log(
            &log::Record::builder()
                .level(log::Level::Trace)
                .args(format_args!("exit: id={:?};", span))
                .build(),
        );
    }

    fn close(&self, span: span::Id) {
        let logger = log::logger();
        logger.log(
            &log::Record::builder()
                .level(log::Level::Trace)
                .args(format_args!("close: id={:?};", span))
                .build(),
        );
    }
}

impl tokio_trace_subscriber::Observe for TraceLogger {
    fn observe_event<'a>(&self, event: &'a Event<'a>) {
        <Self as Subscriber>::observe_event(&self, event)
    }

    fn enter(&self, span: &SpanRef) {
        if let Some(data) = span.data {
            let meta = data.meta();
            let log_meta = meta.as_log();
            let logger = log::logger();
            if logger.enabled(&log_meta) {
                logger.log(
                    &log::Record::builder()
                        .metadata(log_meta)
                        .module_path(meta.module_path)
                        .file(meta.file)
                        .line(meta.line)
                        .args(format_args!(
                            "enter: {}; span={:?}; parent={:?}; {:?}",
                            meta.name.unwrap_or(""),
                            span.id,
                            data.parent,
                            LogFields(span),
                        )).build(),
                );
            }
        } else {
            <Self as Subscriber>::enter(&self, span.id.clone())
        }
    }

    fn exit(&self, span: &SpanRef) {
        if let Some(data) = span.data {
            let meta = data.meta();
            let log_meta = meta.as_log();
            let logger = log::logger();
            if logger.enabled(&log_meta) {
                logger.log(
                    &log::Record::builder()
                        .metadata(log_meta)
                        .module_path(meta.module_path)
                        .file(meta.file)
                        .line(meta.line)
                        .args(format_args!(
                            "exit: {}; span={:?}; parent={:?};",
                            meta.name.unwrap_or(""),
                            span.id,
                            data.parent,
                        )).build(),
                );
            }
        } else {
            <Self as Subscriber>::exit(&self, span.id.clone())
        }
    }

    fn close(&self, span: &SpanRef) {
        if let Some(data) = span.data {
            let meta = data.meta();
            let log_meta = meta.as_log();
            let logger = log::logger();
            if logger.enabled(&log_meta) {
                logger.log(
                    &log::Record::builder()
                        .metadata(log_meta)
                        .module_path(meta.module_path)
                        .file(meta.file)
                        .line(meta.line)
                        .args(format_args!(
                            "close: {}; span={:?}; parent={:?};",
                            meta.name.unwrap_or(""),
                            span.id,
                            data.parent,
                        )).build(),
                );
            }
        } else {
            <Self as Subscriber>::close(&self, span.id.clone())
        }
    }

    fn filter(&self) -> &dyn tokio_trace_subscriber::Filter {
        self
    }
}

impl tokio_trace_subscriber::Filter for TraceLogger {
    fn enabled(&self, metadata: &Meta) -> bool {
        <Self as Subscriber>::enabled(&self, metadata)
    }

    fn should_invalidate_filter(&self, _metadata: &Meta) -> bool {
        false
    }
}

impl<'a, I: 'a, T: 'a> fmt::Debug for LogFields<'a, I, T>
where
    &'a I: IntoIterator<Item = (&'a str, T)>,
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut fields = self.0.into_iter();
        if let Some((field, value)) = fields.next() {
            // Format the first field without a leading space, in case it's the
            // only field.
            write!(f, "{}={:?};", field, value)?;
            for (field, value) in fields {
                write!(f, " {}={:?};", field, value)?;
            }
        }

        Ok(())
    }
}
