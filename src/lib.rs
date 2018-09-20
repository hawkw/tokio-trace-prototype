extern crate futures;
extern crate log;

pub use log::Level;

use std::{
    cell::RefCell,
    fmt,
    time::Instant,
    sync::Arc,
    slice,
};

macro_rules! static_meta {
    ($($k:ident),*) => (
        static_meta!(@ None, $crate::Level::Trace, $($k),* )
    );
    ($lvl:expr, $($k:ident),*) => (
        static_meta!(@ None, $lvl, $($k),* )
    );
    (target: $target:expr, $lvl:expr, $($k:ident),*) => (
        static_meta!(@ Some($target), $lvl, $($k),* )
    );
    (target: $target:expr, $($k:ident),*) => (
        static_meta!(@ Some($target), $crate::Level::Trace, $($k),* )
    );
    (@ $target:expr, $lvl:expr, $($k:ident),*) => (
        $crate::StaticMeta {
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
        $crate::Span {
            inner: ::std::sync::Arc::new($crate::SpanInner {
                opened_at: ::std::time::Instant::now(),
                parent: $crate::Span::current(),
                static_meta: &static_meta!( $($k),* ),
                field_values: vec![ $(Box::new($val)),* ], // todo: wish this wasn't double-boxed...
                name: Some($name),
            })
        }
    }
}

#[macro_export]
macro_rules! event {
    (target: $target:expr, $lvl:expr, $($arg:tt)+, $($k:ident = $val:expr),*) => ({
        $crate::Event {
            timestamp: ::std::time::Instant::now(),
            parent: $crate::Span::current(),
            follows_from: &[],
            static_meta: &static_meta!(target: $target, $lvl, $($k),* ),
            field_values: &[ $($val),* ]
        }
    });
    ($lvl:expr, $($arg:tt)+, $($k:ident = $val:expr),*) => (event!(target: None, $lvl, $($arg)+, $($k = $val),*))
}

thread_local! {
    static CURRENT_SPAN: RefCell<Span> = RefCell::new(span!("root",));
}

pub mod subscriber;
mod dispatcher;

pub use dispatcher::{Dispatcher, Builder as DispatcherBuilder};

// XXX: im using fmt::Debug for prototyping purposes, it should probably leave.
pub trait Value: fmt::Debug {
    // ... ?
}

pub struct Event<'event> {
    pub timestamp: Instant,

    pub parent: Span,
    pub follows_from: &'event [Span],

    pub static_meta: &'event StaticMeta,
    pub field_values: &'event [&'event dyn Value],
    pub message: fmt::Arguments<'event>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StaticMeta {
    pub target: Option<&'static str>,
    pub level: log::Level,

    pub module_path: &'static str,
    pub file: &'static str,
    pub line: u32,

    pub field_names: &'static [&'static str],
}

#[derive(Clone)]
pub struct Span {
    inner: Arc<SpanInner>,
}

#[derive(Debug)]
struct SpanInner {
    pub name: Option<&'static str>,
    pub opened_at: Instant,

    pub parent: Span,

    pub static_meta: &'static StaticMeta,

    pub field_values: Vec<Box<dyn Value>>,

    // ...
}

// ===== impl Event =====

impl<'event> Event<'event> {
    pub fn field_names(&self) -> slice::Iter<&'static str> {
        self.static_meta
            .field_names
            .iter()
    }

    pub fn fields(&'event self) -> impl Iterator<Item = (&'static str, &'event dyn Value)> {
        self.field_names()
            .enumerate()
            .map(move |(idx, &name)| (name, self.field_values[idx]))
            .chain(self.parent.fields())
    }

    pub fn debug_fields(&'event self) -> DebugFields<'event, Self> {
        DebugFields(self)
    }
}

impl<'event> Drop for Event<'event> {
    fn drop(&mut self) {
        Dispatcher::current().broadcast(self);
    }
}

// ===== impl Span =====

impl Span {

    pub fn current() -> Self {
        CURRENT_SPAN.with(|span| {
            span.borrow().clone()
        })
    }

    pub fn name(&self) -> Option<&'static str> {
        self.inner.name
    }

    pub fn parent(&self) -> &Span {
        &self.inner.parent
    }

    pub fn meta(&self) -> &'static StaticMeta {
        self.inner.static_meta
    }

    pub fn field_names(&self) -> slice::Iter<&'static str> {
        self.inner
            .static_meta
            .field_names
            .iter()
    }

    pub fn fields<'a>(&'a self) -> impl Iterator<Item = (&'static str, &'a dyn Value)> {
        self.field_names()
            .enumerate()
            .map(move |(idx, &name)| (name, self.inner.field_values[idx].as_ref()))
    }

    pub fn enter<F: FnOnce() -> T, T>(&self, f: F) -> T {
        CURRENT_SPAN.with(|current_span| {
            current_span.replace(self.clone());

            let result = f();

            current_span.replace(self.parent().clone());

            result
        })
    }

    pub fn debug_fields<'a>(&'a self) -> DebugFields<'a, Self> {
        DebugFields(self)
    }
}

impl<'a> IntoIterator for &'a Span {
    type Item = (&'static str, &'a dyn Value);
    type IntoIter = Box<Iterator<Item = (&'static str, &'a dyn Value)> +'a>; // TODO: unbox
    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.fields())
    }
}

impl<'a> IntoIterator for &'a Event<'a> {
    type Item = (&'static str, &'a dyn Value);
    type IntoIter = Box<Iterator<Item = (&'static str, &'a dyn Value)> +'a>; // TODO: unbox
    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.fields())
    }
}

pub struct DebugFields<'a, I: 'a>(&'a I)
where
    &'a I: IntoIterator<Item = (&'static str, &'a dyn Value)>;

impl<'a, I:'a > fmt::Debug for DebugFields<'a, I>
where
    &'a I: IntoIterator<Item = (&'static str, &'a dyn Value)>,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map().entries(self.0.into_iter()).finish()
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Span")
            .field("name", &self.inner.name)
            .field("opened_at", &self.inner.opened_at)
            .field("parent", &self.inner.parent.name())
            .field("fields", &self.debug_fields())
            .field("meta", &self.meta())
            .finish()
    }
}
