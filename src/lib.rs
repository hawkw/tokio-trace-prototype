extern crate futures;
use std::{
    cell::RefCell,
    fmt,
    time::Instant,
    ptr,
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
            opened_at: ::std::time::Instant::now(),
            parent: CURRENT_SPAN.with(|spans| {
                spans.borrow_mut()
                    .last_mut()
                    .and_then(|head| {
                        ::std::ptr::NonNull::new( head as *mut _ )
                    })
            }),
            static_meta: &static_meta!( $($k),* ),
            field_values: Box::new([ $(Box::new($val)),* ]), // todo: wish this wasn't boxed
            name: Some($name),
        }
    }
}

thread_local! {
    static CURRENT_SPAN: RefCell<Vec<Span>> = RefCell::new(vec![ span!("root",) ]);
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

    pub parent: &'event Span,
    pub follows_from: &'event [&'event Span],

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

pub struct Span {
    pub opened_at: Instant,

    pub parent: Option<ptr::NonNull<Span>>,

    pub static_meta: &'static StaticMeta,
    pub field_values: Box<[Box<dyn Value>]>, // TODO: awful
    pub name: Option<&'static str>,
    // ...
}
