extern crate futures;
use std::{
    cell::RefCell,
    fmt,
    time::Instant,
    sync::Arc,
};

macro_rules! static_meta {
    ($($k:ident),*) => {
        $crate::StaticMeta {
            module_path: module_path!(),
            file: file!(),
            line: line!(),
            field_names: &[ $(stringify!($k)),* ],
        }
    }
}

#[macro_export]
macro_rules! span {
    ($name:expr, $($k:ident = $val:expr),*) => {
        $crate::Span {
            inner: ::std::sync::Arc::new($crate::SpanInner {
                opened_at: ::std::time::Instant::now(),
                parent: CURRENT_SPAN.with(|span| {
                    span.borrow().clone()
                }),
                static_meta: &static_meta!( $($k),* ),
                field_values: vec![ $(Box::new($val)),* ], // todo: wish this wasn't double-boxed...
                name: Some($name),
            })
        }
    }
}

thread_local! {
    static CURRENT_SPAN: RefCell<Span> = RefCell::new(span!("root",));
}

// XXX: im using fmt::Debug for prototyping purposes, it should probably leave.
pub trait Value: fmt::Debug {
    // ... ?
}

pub trait Subscriber {
    fn consume<'event>(&self, event: Event<'event>);
}

pub struct Event<'event> {
    pub timestamp: Instant,

    pub parent: Span,
    pub follows_from: &'event [Span],

    pub static_meta: &'event StaticMeta,
    pub field_values: &'event [&'event dyn Value],
    pub message: fmt::Arguments<'event>,
}

#[derive(Clone, Debug)]
pub struct StaticMeta {
    pub module_path: &'static str,
    pub file: &'static str,
    pub line: u32,

    pub field_names: &'static [&'static str],
}

#[derive(Clone)]
pub struct Span {
    inner: Arc<SpanInner>,
}

struct SpanInner {
    pub opened_at: Instant,

    pub parent: Span,

    pub static_meta: &'static StaticMeta,
    pub name: Option<&'static str>,

    pub field_values: Vec<Box<dyn Value>>,

    // ...
}
