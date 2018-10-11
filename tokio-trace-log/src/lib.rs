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
extern crate tokio_trace;
extern crate tokio_trace_subscriber;
extern crate log;

use std::io;
use tokio_trace::{span, Subscriber, Event, SpanData, Meta};

/// Format a log record as a trace event in the current span.
pub fn format_trace(record: &log::Record) -> io::Result<()> {
    let meta: tokio_trace::Meta = record.as_trace();
    let event = Event {
        parent: tokio_trace::SpanData::current(),
        follows_from: &[],
        meta: &meta,
        field_values: &[],
        message: record.args().clone()
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

// ===== impl LogTracer =====

impl LogTracer {
    pub fn with_filter(filter: log::LevelFilter) -> Self {
       Self {
           filter,
       }
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

    fn new_span(&self, _new_span: span::Data) -> span::Id {
        span::Id::from_u64(0)
    }

    fn span_data(&self, _id: &span::Id) -> Option<&span::Data> {
        // TODO: should this type just be an observer, or should it also impl
        // subscriber?
        unimplemented!("no registry")
    }

    fn observe_event<'event, 'meta: 'event>(&self, event: &'event Event<'event, 'meta>) {
        let fields = event.debug_fields();
        let meta = event.meta.as_log();
        let logger = log::logger();
        let parents = event.parents().filter_map(SpanData::name).collect::<Vec<_>>();
        if logger.enabled(&meta) {
            logger.log(
                &log::Record::builder()
                    .metadata(meta)
                    .module_path(event.meta.module_path)
                    .file(event.meta.file)
                    .line(event.meta.line)
                    .args(format_args!(
                        "[{}] {:?} {}",
                        parents.join(":"),
                        fields,
                        event.message
                    )).build(),
            );
        }
    }

    fn enter(&self, span: span::Id, state: span::State) {
        // let logger = log::logger();
        // logger.log(&log::Record::builder()
        //     .args(format_args!("-> {:?}", span.name()))
        //     .build()
        // )
        unimplemented!()
    }

    fn exit(&self, span: span::Id, state: span::State) {
        // let logger = log::logger();
        // logger.log(&log::Record::builder().args(format_args!("<- {:?}", span.name())).build())
        unimplemented!()
    }
}

impl tokio_trace_subscriber::Observe for TraceLogger {
    fn observe_event<'event, 'meta: 'event>(&self, event: &'event Event<'event, 'meta>) {
        <Self as Subscriber>::observe_event(&self, event)
    }

    fn enter(&self, span: &SpanData) {
        // <Self as Subscriber>::enter(&self, span)
        unimplemented!()
    }

    fn exit(&self, span: &SpanData) {
        // <Self as Subscriber>::exit(&self, span)
        unimplemented!()
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
