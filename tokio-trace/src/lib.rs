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
            use $crate::{callsite, span::AsId, Dispatch, Span};
            let callsite = callsite! { span: $name, $( $k ),* };
            Dispatch::current().if_enabled(&callsite, |dispatch, meta| {
                let span = Span::new(dispatch.clone(), meta);
                // Depending on how many fields are generated, this may or may
                // not actually be used, but it doesn't make sense to repeat it.
                #[allow(unused_variables, unused_mut, unused_imports)] {
                    use $crate::Subscriber;
                    let id = span.as_id().expect("must have ID");
                    let mut keys = meta.fields();
                    $(
                        let key = keys.next()
                            .expect(concat!("metadata should define a key for '", stringify!($k), "'"));
                        span!(@ add_value: &dispatch, &id, $k, &key, $($val)*);
                    )*
                }

                span
            }).unwrap_or_else(Span::new_disabled)
        }
    };
    (@ add_value: $dispatch:expr, $id:expr, $k:expr, $i:expr, $val:expr) => (
        $dispatch.add_value($id, $i, $val)
            .expect(concat!("adding value for field '", stringify!($k), "' failed"));
    );
    (@ add_value: $dispatch:expr, $id:expr, $k:expr, $i:expr,) => (
        // skip
    );
}

#[macro_export]
macro_rules! event {
    (target: $target:expr, $lvl:expr, { $($k:ident = $val:expr),* }, $($arg:tt)+ ) => ({
        {
            use $crate::{callsite, Dispatch, SpanData, SpanId, Subscriber, Event, field::AsValue};
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
pub mod field;
pub mod span;
pub mod subscriber;

pub use self::{
    dispatcher::Dispatch,
    field::{AsValue, IntoValue, Value},
    span::{Data as SpanData, Id as SpanId, Span},
    subscriber::Subscriber,
    tokio_trace_core::{callsite, Event, Level, Meta},
};

#[doc(hidden)]
pub use tokio_trace_core::Kind;
