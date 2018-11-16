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
    collections::HashMap,
    fmt, io,
    sync::{
        atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT},
        Mutex,
    },
};
use tokio_trace::{
    field, span,
    subscriber::{self, Subscriber},
    Event, Meta, Id,
};

/// Format a log record as a trace event in the current span.
pub fn format_trace(record: &log::Record) -> io::Result<()> {
    struct LogCallsite<'a>(tokio_trace::Meta<'a>);
    impl<'a> tokio_trace::Callsite for LogCallsite<'a> {
        fn interest(&self) -> tokio_trace::subscriber::Interest {
            tokio_trace::subscriber::Interest::SOMETIMES
        }

        fn add_interest(&self, _interest: tokio_trace::subscriber::Interest) {
            // Since these callsites can't be registered (they're not known to
            // be valid for the 'static lifetime), we don't need to track
            // interest --- do nothing.
        }

        fn remove_interest(&self) {
            // Again, we don't cache interest for these.
        }

        fn metadata(&self) -> &Meta {
            &self.0
        }
    }
    let callsite = LogCallsite(record.as_trace());
    let k = callsite.0.key_for(&"message").unwrap();
    drop(tokio_trace::Event::new(
        &callsite,
        |event| {
            event.message(&k, record.args().clone()).unwrap();
        }
    ));
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
        Meta::new_id(
            self.target(),
            self.level().as_trace(),
            self.module_path(),
            self.file(),
            self.line(),
            &["message"],
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
pub struct TraceLogger {
    // TODO: the hashmap can definitely be replaced with some kind of arena eventually.
    in_progress: Mutex<HashMap<Id, LineBuilder>>,
}

struct LogFields<'a, 'b: 'a, I: 'a>(&'a I)
where
    &'a I: IntoIterator<Item = (field::Key<'b>, &'b field::Value)>;

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
        TraceLogger {
            in_progress: Mutex::new(HashMap::new()),
        }
    }
}

struct LineBuilder {
    file: Option<String>,
    line: Option<u32>,
    module_path: Option<String>,
    target: String,
    log_line: String,
}

impl LineBuilder {
    fn new(attrs: span::Attributes, id: Id) -> Self {
        use std::fmt::Write;
        let mut log_line = String::new();
        let meta = attrs.metadata();
        write!(&mut log_line, "{}{}{}={:?}; parent={:?};",
            meta.name.unwrap_or(""),
            if meta.name.is_some() { " " } else { "" },
            if meta.is_span() { "span" } else { "event" },
            id,
            attrs.parent(),
        ).unwrap();
        Self {
            file: meta.file.map(|s| s.to_owned()),
            line: meta.line,
            module_path: meta.module_path.map(|s| s.to_owned()),
            target: meta.target.to_owned(),
            log_line,
        }
    }

    fn record(&mut self, key: &field::Key, val: fmt::Arguments) -> fmt::Result {
        use std::fmt::Write;
        write!(&mut self.log_line, "{}=", key.name().unwrap_or("???"))?;
        self.log_line.write_fmt(val)?;
        self.log_line.write_str("; ")
    }

    fn finish(self) {
        log::logger().log(
            &log::Record::builder()
                .target(self.target.as_ref())
                .module_path(self.module_path.as_ref().map(String::as_ref))
                .file(self.file.as_ref().map(String::as_ref))
                .line(self.line)
                .args(format_args!("close {}", self.log_line,))
                .build(),
        );
    }
}

impl Subscriber for TraceLogger {
    fn enabled(&self, metadata: &Meta) -> bool {
        log::logger().enabled(&metadata.as_log())
    }

    fn new_id(&self, new_span: span::Attributes) -> Id {
        static NEXT_ID: AtomicUsize = ATOMIC_USIZE_INIT;
        let id = Id::from_u64(NEXT_ID.fetch_add(1, Ordering::SeqCst) as u64);
        self.in_progress
            .lock()
            .unwrap()
            .insert(id.clone(), LineBuilder::new(new_span, id.clone()));
        id
    }

    fn record_fmt(
        &self,
        span: &Id,
        key: &field::Key,
        val: fmt::Arguments,
    ) -> Result<(), subscriber::RecordError> {
        if let Some(span) = self.in_progress.lock().unwrap().get_mut(span) {
            span.record(key, val)?;
            Ok(())
        } else {
            Err(subscriber::RecordError::no_span(span.clone()))
        }
    }

    fn add_follows_from(
        &self,
        span: &Id,
        follows: Id,
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

    fn enter(&self, span: Id) {
        let logger = log::logger();
        logger.log(
            &log::Record::builder()
                .level(log::Level::Trace)
                .args(format_args!("enter: span={:?};", span))
                .build(),
        );
    }

    fn exit(&self, span: Id) {
        let logger = log::logger();
        logger.log(
            &log::Record::builder()
                .level(log::Level::Trace)
                .args(format_args!("exit: id={:?};", span))
                .build(),
        );
    }

    fn close(&self, span: Id) {
        if let Some(line) = self.in_progress.lock().unwrap().remove(&span) {
            line.finish()
        }
    }
}

// impl tokio_trace_subscriber::Observe for TraceLogger {
//     fn observe_event<'a>(&self, event: &'a Event<'a>) {
//         <Self as Subscriber>::observe_event(&self, event)
//     }

//     fn enter(&self, span: &SpanRef) {
//         if let Some(data) = span.data {
//             let meta = data.metadata();
//             let log_meta = meta.as_log();
//             let logger = log::logger();
//             if logger.enabled(&log_meta) {
//                 logger.log(
//                     &log::Record::builder()
//                         .metadata(log_meta)
//                         .module_path(meta.module_path)
//                         .file(meta.file)
//                         .line(meta.line)
//                         .args(format_args!(
//                             "enter: {}; span={:?}; parent={:?}; {:?}",
//                             meta.name.unwrap_or(""),
//                             span.id,
//                             data.parent(),
//                             LogFields(span),
//                         )).build(),
//                 );
//             }
//         } else {
//             <Self as Subscriber>::enter(&self, span.id.clone())
//         }
//     }

//     fn exit(&self, span: &SpanRef) {
//         if let Some(data) = span.data {
//             let meta = data.metadata();
//             let log_meta = meta.as_log();
//             let logger = log::logger();
//             if logger.enabled(&log_meta) {
//                 logger.log(
//                     &log::Record::builder()
//                         .metadata(log_meta)
//                         .module_path(meta.module_path)
//                         .file(meta.file)
//                         .line(meta.line)
//                         .args(format_args!(
//                             "exit: {}; span={:?}; parent={:?};",
//                             meta.name.unwrap_or(""),
//                             span.id,
//                             data.parent(),
//                         )).build(),
//                 );
//             }
//         } else {
//             <Self as Subscriber>::exit(&self, span.id.clone())
//         }
//     }

//     fn close(&self, span: &SpanRef) {
//         if let Some(data) = span.data {
//             let meta = data.metadata();
//             let log_meta = meta.as_log();
//             let logger = log::logger();
//             if logger.enabled(&log_meta) {
//                 logger.log(
//                     &log::Record::builder()
//                         .metadata(log_meta)
//                         .module_path(meta.module_path)
//                         .file(meta.file)
//                         .line(meta.line)
//                         .args(format_args!(
//                             "close: {}; span={:?}; parent={:?};",
//                             meta.name.unwrap_or(""),
//                             span.id,
//                             data.parent(),
//                         )).build(),
//                 );
//             }
//         } else {
//             <Self as Subscriber>::close(&self, span.id.clone())
//         }
//     }

//     fn filter(&self) -> &dyn tokio_trace_subscriber::Filter {
//         self
//     }
// }

impl tokio_trace_subscriber::Filter for TraceLogger {
    fn enabled(&self, metadata: &Meta) -> bool {
        <Self as Subscriber>::enabled(&self, metadata)
    }

    fn should_invalidate_filter(&self, _metadata: &Meta) -> bool {
        false
    }
}
