//! tokio-trace-core
#![warn(missing_docs)]

#[macro_use]
extern crate lazy_static;

#[macro_export]
macro_rules! metadata {
    (
        name: $name:expr,
        target: $target:expr,
        level: $level:expr,
        fields: $field:expr,
        callsite: $callsite:expr
    ) => (
        metadata! {
            name: $name,
            target: $target,
            level: $level,
            fields: $fields,
            callsite: $callsite,
        }
    );
    (
        name: $name:expr,
        target: $target:expr,
        level: $level:expr,
        fields: $fields:expr,
        callsite: $callsite:expr,
    ) => {
        $crate::metadata::Meta {
            name: $name,
            target: $target,
            level: $level,
            file: Some(file!()),
            line: Some(line!()),
            module_path: Some(module_path!()),
            fields: $crate::field::Fields {
                names: $fields,
                callsite: $crate::callsite::Identifier($callsite),
            }
        }
    }
}

pub mod callsite;
pub mod dispatcher;
pub mod field;
pub mod metadata;
pub mod span;
pub mod subscriber;

pub use self::{
    callsite::Callsite,
    dispatcher::Dispatch,
    field::Key,
    metadata::{Level, Meta},
    span::Span,
    subscriber::{Interest, Subscriber},
};
