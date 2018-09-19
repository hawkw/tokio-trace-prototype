extern crate futures;
use std::{
    fmt,
    time::Instant,
};

// XXX: im using fmt::Debug for prototyping purposes, it should probably leave.
pub trait Value: fmt::Debug {
    // ... ?
}

pub trait Subscriber {
    fn consume<'event>(&self, event: Event<'event>);
}

pub struct Event<'event> {
    pub timestamp: Instant,

    pub parent: &'event Span<'event>,
    pub follows_from: &'event [&'event Span<'event>],

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

pub struct Span<'span> {
    pub opened_at: Instant,

    pub parent: Option<&'span Span<'span>>,

    pub static_meta: &'span StaticMeta,
    pub field_values: &'span[&'span dyn Value],
    pub name: Option<&'span str>,
    // ...
}

macro_rules! static_meta {
    ($($k:ident),*) => {
        $crate::StaticMeta {
            module_path: module_path!(),
            file: file!(),
            line: line!(),
            field_values: &[ $(stringify!($k)),* ],
        }
    }
}

#[macro_export]
macro_rules! span {
    ($name:expr, $($k:ident = $val:expr),*) => {
        $crate::Span {
            opened_at: ::std::time::Instant::now(),
            parent: None, // TODO: get currently-executing span from TLS.
            static_meta: &static_meta!( $($k),* ),
            field_values: &[ $($val),* ],

        }
    }
}

// pub struct OpenSpan<'span, T> {
//     opened_at: Instant,
//     fields: &'span [(&'span str, &'span T)],

//     module_path: Option<&'span str>,
//     file: Option<&'span str>,
//     line: Option<u32>,

//     parent: Option<&'span OpenSpan<'span, T>>,
// }
