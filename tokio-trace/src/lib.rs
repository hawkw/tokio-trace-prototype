//!
//! # Core Concepts
//!
//! The core of `tokio-trace`'s API is composed of `Events`, `Spans`, and
//! `Subscribers`. We'll cover these in turn.
//!
//! # Spans
//!
//! A `Span` represents a _period of time_ during which a program was executing
//! in some context. A thread of execution is said to _enter_ a span when it
//! begins executing in that context and _exit_s the span when switching to
//! another context. The span in which a thread is currently executing is
//! referred to as the _current_ span.
//!
//! Spans form a tree structure --- unless it is the root span, all spans have a
//! _parent_, and may have one or more _children_. When a new span is created,
//! the current span becomes the new span's parent. The total execution time of
//! a span consists of the time spent in that span and in the entire subtree
//! represented by its children. Thus, a parent span always lasts for at least
//! as long as the longest-executing span in its subtree.
//!
//! Furthermore, execution may enter and exit a span multiple times before that
//! span is _completed_. Consider, for example, a future which has an associated
//! span and enters that span every time it is polled:
//! ```rust
//! # extern crate futures;
//! # use futures::{Future, Poll, Async};
//! struct MyFuture {
//!    // data
//!    span: tokio_trace::Span,
//! }
//!
//! impl Future for MyFuture {
//!     type Item = ();
//!     type Error = ();
//!
//!     fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
//!         self.span.enter(|| {
//!             // Do actual future work
//! # Ok(Async::Ready(()))
//!         })
//!     }
//! }
//! ```
//!
//! If this future was spawned on an executor, it might yield one or more times
//! before `poll` returns `Ok(Async::Ready)`. If the future were to yield, then
//! the executor would move on to poll the next future, which may _also_ enter
//! an associated span or series of spans. Therefore, it is valid for a span to
//! be entered repeatedly before it completes. Only the time when that span or
//! one of its children was the current span is considered to be time spent in
//! that span.
//!
//! In addition, data may be associated with spans. A span may have _fields_ ---
//! a set of key-value pairs describing the state of the program during that
//! span; an optional name, and metadata describing the source code location
//! where the span was originally entered.
//!
//! # Events
//!
//! An `Event` represents a _point_ in time. It signifies something that
//! happened while the trace was executing. `Event`s are comparable to the log
//! records emitted by unstructured logging code, but unlike a typical log line,
//! an `Event` always occurs within the context of a `Span`. Like a `Span`, it
//! may have fields, and implicitly inherits any of the fields present on its
//! parent span. Additionally, it may be linked with one or more additional
//! spans that are not its parent; in this case, the event is said to _follow
//! from_ those spans.
//!
//! Essentially, `Event`s exist to bridge the gap between traditional
//! unstructured logging and span-based tracing. Similar to log records, they
//! may be recorded at a number of levels, and can have unstructured,
//! human-readable messages; however, they also carry key-value data and exist
//! within the context of the tree of spans that comprise a trase. Thus,
//! individual log record-like events can be pinpointed not only in time, but in
//! the logical execution flow of the system.
extern crate futures;
extern crate log;
#[macro_use]
extern crate lazy_static;
pub use log::Level;

use std::{fmt, slice, time::Instant};

use self::dedup::IteratorDedup;

#[doc(hidden)]
#[macro_export]
macro_rules! static_meta {
    ($($k:ident),*) => (
        static_meta!(@ None, $crate::Level::Trace, $($k),* )
    );
    (level: $lvl:expr, $($k:ident),*) => (
        static_meta!(@ None, $lvl, $($k),* )
    );
    (target: $target:expr, level: $lvl:expr, $($k:ident),*) => (
        static_meta!(@ Some($target), $lvl, $($k),* )
    );
    (target: $target:expr, $($k:ident),*) => (
        static_meta!(@ Some($target), $crate::Level::Trace, $($k),* )
    );
    (@ $target:expr, $lvl:expr, $($k:ident),*) => (
        $crate::Meta {
            target: $target,
            level: $lvl,
            module_path: module_path!(),
            file: file!(),
            line: line!(),
            field_names: &[ $(stringify!($k)),* ],
        }
    )
}

#[macro_export]
macro_rules! span {
    ($name:expr, $($k:ident = $val:expr),*) => {
        $crate::Span::new(
            Some($name),
            ::std::time::Instant::now(),
            $crate::Span::current(),
            &static_meta!( $($k),* ),
            vec![ $(Box::new($val)),* ], // todo: wish this wasn't double-boxed...
        )
    }
}

#[macro_export]
macro_rules! event {
    (target: $target:expr, $lvl:expr, { $($k:ident = $val:expr),* }, $($arg:tt)+ ) => ({
    {       let field_values: &[& dyn $crate::Value] = &[ $( & $val),* ];
            use $crate::Subscriber;
            $crate::Dispatcher::current().observe_event(&$crate::Event {
                timestamp: ::std::time::Instant::now(),
                parent: $crate::Span::current().downgrade(),
                follows_from: &[],
                meta: &static_meta!(@ $target, $lvl, $($k),* ),
                field_values: &field_values[..],
                message: format_args!( $($arg)+ ),
            });
        }

    });
    ($lvl:expr, { $($k:ident = $val:expr),* }, $($arg:tt)+ ) => (event!(target: None, $lvl, { $($k = $val),* }, $($arg)+))
}

mod dedup;
mod dispatcher;
pub mod instrument;
pub mod span;
pub mod subscriber;

pub use self::{
    dispatcher::{Builder as DispatcherBuilder, Dispatcher},
    span::{Data as SpanData, Span},
    subscriber::Subscriber,
};

// XXX: im using fmt::Debug for prototyping purposes, it should probably leave.
pub trait Value: fmt::Debug + Send + Sync {
    // ... ?
}

impl<T> Value for T where T: fmt::Debug + Send + Sync {}

/// **Note**: `Event` must be generic over two lifetimes, that of `Event` itself
/// (the `'event` lifetime) *and* the lifetime of the event's metadata (the
/// `'meta` lifetime), which must be at least as long as the event's lifetime.
/// This is because the metadata may live as long as the lifetime, or it may be
/// `'static` and reused for all `Event`s generated from a particular source
/// code location (as is the case when the event is produced by the `event!`
/// macro). Consumers of `Event` probably do not need to actually care about
/// these lifetimes, however.
pub struct Event<'event, 'meta> {
    pub timestamp: Instant,

    pub parent: SpanData,
    pub follows_from: &'event [SpanData],

    pub meta: &'meta Meta<'meta>,
    // TODO: agh box
    pub field_values: &'event [&'event dyn Value],
    pub message: fmt::Arguments<'event>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Meta<'a> {
    pub target: Option<&'a str>,
    pub level: log::Level,

    pub module_path: &'a str,
    pub file: &'a str,
    pub line: u32,

    pub field_names: &'a [&'a str],
}

type StaticMeta = Meta<'static>;

/// Iterator over the parents of a span or event
pub struct Parents<'a> {
    next: Option<&'a SpanData>,
}

// ===== impl Event =====

impl<'event, 'meta: 'event> Event<'event, 'meta> {
    pub fn field_names(&self) -> slice::Iter<&'event str> {
        self.meta.field_names.iter()
    }

    pub fn fields(&'event self) -> impl Iterator<Item = (&'event str, &'event dyn Value)> {
        self.field_names()
            .enumerate()
            .filter_map(move |(idx, &name)| self.field_values.get(idx).map(|&val| (name, val)))
    }

    pub fn debug_fields<'a: 'meta>(&'a self) -> DebugFields<'a, Self> {
        DebugFields(self)
    }

    pub fn parents<'a>(&'a self) -> Parents<'a> {
        Parents {
            next: Some(&self.parent),
        }
    }

    pub fn all_fields<'a>(&'a self) -> impl Iterator<Item = (&'a str, &'a dyn Value)> {
        self.fields()
            .chain(self.parents().flat_map(|parent| parent.fields()))
            .dedup_by(|(k, _)| k)
    }
}

impl<'a, 'm: 'a> IntoIterator for &'a Event<'a, 'm> {
    type Item = (&'a str, &'a dyn Value);
    type IntoIter = Box<Iterator<Item = (&'a str, &'a dyn Value)> + 'a>; // TODO: unbox
    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.fields())
    }
}

pub struct DebugFields<'a, I: 'a>(&'a I)
where
    &'a I: IntoIterator<Item = (&'a str, &'a dyn Value)>;

impl<'a, I: 'a> fmt::Debug for DebugFields<'a, I>
where
    &'a I: IntoIterator<Item = (&'a str, &'a dyn Value)>,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map().entries(self.0.into_iter()).finish()
    }
}

// ===== impl Parents =====

impl<'a> Iterator for Parents<'a> {
    type Item = &'a SpanData;
    fn next(&mut self) -> Option<Self::Item> {
        self.next = self.next.and_then(SpanData::parent);
        self.next
    }
}

impl<'a, 'meta> From<&'a log::Record<'meta>> for Meta<'meta> {
    fn from(record: &'a log::Record<'meta>) -> Self {
        Meta {
            target: Some(record.target()),
            level: record.level(),
            module_path: record
                .module_path()
                // TODO: make symmetric
                .unwrap_or_else(|| record.target() ),
            line: record.line().unwrap_or(0),
            file: record.file().unwrap_or("???"),
            field_names: &[],
        }
    }
}
