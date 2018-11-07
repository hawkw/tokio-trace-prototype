extern crate tokio_trace_core;

/// Constructs a new span.
///
/// # Examples
///
/// Creating a new span with no fields:
/// ```
/// # #[macro_use]
/// # extern crate tokio_trace;
/// # fn main() {
/// let mut span = span!("my span");
/// span.enter(|| {
///     // do work inside the span...
/// });
/// # }
/// ```
///
/// Creating a span with fields:
/// ```
/// # #[macro_use]
/// # extern crate tokio_trace;
/// # fn main() {
/// span!("my span", foo = &2, bar = &"a string").enter(|| {
///     // do work inside the span...
/// });
/// # }
/// ```
#[macro_export]
macro_rules! span {
    ($name:expr) => { span!($name,) };
    ($name:expr, $($k:ident $( = $val:expr )* ) ,*) => {
        {
            use $crate::{callsite, Dispatch, Span};
            let callsite = callsite! { span: $name, $( $k ),* };
            Dispatch::current().if_enabled(&callsite, |dispatch, meta| {
                let span = Span::new(dispatch.clone(), meta);
                // Depending on how many fields are generated, this may or may
                // not actually be used, but it doesn't make sense to repeat it.
                #[allow(unused_variables, unused_mut)]
                let mut key = meta.first_field();
                $(
                    span!(@ add_value: span, $k, &key, $($val)*);
                    // Similarly, if this is the last key, the incremented value
                    // won't be used...
                    #[allow(unused_assignments)] {
                        key = key.next();
                    }
                )*
                span
            }).unwrap_or_else(Span::new_disabled)
        }
    };
    (@ add_value: $span:expr, $k:expr, $i:expr, $val:expr) => (
        $span.add_value($i, $val)
            .expect(concat!("adding value for field ", stringify!($k), " failed"));
    );
    (@ add_value: $span:expr, $k:expr, $i:expr,) => (
        // skip
    );
}

#[macro_export]
macro_rules! event {
    (target: $target:expr, $lvl:expr, { $($k:ident = $val:expr),* }, $($arg:tt)+ ) => ({
        {
            use $crate::{callsite, Dispatch, SpanData, SpanId, Subscriber, Event, value::AsValue};
            let callsite = callsite! { event:
                $lvl,
                target:
                $target, $( $k ),*
            };
            Dispatch::current().if_enabled(&callsite, |dispatch, meta| {
                let field_values: &[ &dyn AsValue ] = &[ $( &$val ),* ];
                dispatch.observe_event(&Event {
                    parent: SpanId::current(),
                    follows_from: &[],
                    meta,
                    field_values: &field_values[..],
                    message: format_args!( $($arg)+ ),
                });
            });
        }
    });
    ($lvl:expr, { $($k:ident = $val:expr),* }, $($arg:tt)+ ) => (
        event!(target: module_path!(), $lvl, { $($k = $val),* }, $($arg)+)
    )
}

mod dispatcher;
pub mod span;
pub mod subscriber;

pub use self::{
    dispatcher::Dispatch,
    span::{Data as SpanData, Id as SpanId, Span},
    subscriber::Subscriber,
    tokio_trace_core::{
        callsite,
        value::{self, AsValue, IntoValue, Value},
        Event, Field, Level, Meta,
    },
};

#[doc(hidden)]
pub use tokio_trace_core::Kind;
