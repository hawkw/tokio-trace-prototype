pub use tokio_trace_core::dispatcher::*;

use std::{cell::RefCell, thread};

thread_local! {
    static CURRENT_DISPATCH: RefCell<Dispatch> = RefCell::new(Dispatch::none());
}

/// Sets this dispatch as the default for the duration of a closure.
///
/// The default dispatcher is used when creating a new [`Span`] or
/// [`Event`], _if no span is currently executing_. If a span is currently
/// executing, new spans or events are dispatched to the subscriber that
/// tagged that span, instead.
///
/// [`Span`]: ::span::Span
/// [`Subscriber`]: ::Subscriber
/// [`Event`]: ::Event
pub fn with_default<T>(dispatcher: Dispatch, f: impl FnOnce() -> T) -> T {
    let prior = CURRENT_DISPATCH.try_with(|current| {
        current.replace(dispatcher)
    });
    let result = f();
    if let Ok(prior) = prior {
        let _ = CURRENT_DISPATCH.try_with(|current| {
            *current.borrow_mut() = prior;
        });
    }
    result
}

pub(crate) fn with_current<T, F>(f: F) -> T
where
    F: FnOnce(&Dispatch) -> T,
{
    let mut f = Some(f);

    CURRENT_DISPATCH.try_with(|current| {
        // Since `f` is a `FnOnce`, we have to move it to call it. We know the
        // `unwrap_or_else` only happens if the closure passed to `try_with`
        // *isn't* executed, but the compiler doesn't know this, so the weird
        // option dance is unfortunately necessary.
        let f = match f.take() {
            Some(f) => f,
            _ => unreachable!(),
        };
        f(&*current.borrow())
    }).unwrap_or_else(|_| {
        let f = match f.take() {
            Some(f) => f,
            _ => unreachable!(),
        };
        f(&Dispatch::none())
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use {dispatcher, span, subscriber};

    #[test]
    fn dispatcher_is_sticky() {
        // Test ensuring that entire trace trees are collected by the same
        // dispatcher, even across dispatcher context switches.
        let (subscriber1, handle1) = subscriber::mock()
            .enter(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")))
            .enter(span::mock().named(Some("foo")))
            .enter(span::mock().named(Some("bar")))
            .exit(span::mock().named(Some("bar")))
            .drop_span(span::mock().named(Some("bar")))
            .exit(span::mock().named(Some("foo")))
            .drop_span(span::mock().named(Some("foo")))
            .done()
            .run_with_handle();
        let mut foo = dispatcher::with_default(Dispatch::new(subscriber1), || {
            let mut foo = span!("foo");
            foo.enter(|| {});
            foo
        });
        dispatcher::with_default(Dispatch::new(subscriber::mock().done().run()), move || {
            foo.enter(|| span!("bar").enter(|| {}))
        });

        handle1.assert_finished();
    }

    #[test]
    fn dispatcher_isnt_too_sticky() {
        // Test ensuring that new trace trees are collected by the current
        // dispatcher.
        let (subscriber1, handle1) = subscriber::mock()
            .enter(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")))
            .enter(span::mock().named(Some("foo")))
            .enter(span::mock().named(Some("bar")))
            .exit(span::mock().named(Some("bar")))
            .drop_span(span::mock().named(Some("bar")))
            .exit(span::mock().named(Some("foo")))
            .drop_span(span::mock().named(Some("foo")))
            .done()
            .run_with_handle();
        let (subscriber2, handle2) = subscriber::mock()
            .enter(span::mock().named(Some("baz")))
            .enter(span::mock().named(Some("quux")))
            .exit(span::mock().named(Some("quux")))
            .drop_span(span::mock().named(Some("quux")))
            .exit(span::mock().named(Some("baz")))
            .drop_span(span::mock().named(Some("baz")))
            .done()
            .run_with_handle();

        let mut foo = dispatcher::with_default(Dispatch::new(subscriber1), || {
            let mut foo = span!("foo");
            foo.enter(|| {});
            foo
        });
        let mut baz = dispatcher::with_default(Dispatch::new(subscriber2), || span!("baz"));
        dispatcher::with_default(Dispatch::new(subscriber::mock().done().run()), move || {
            foo.enter(|| span!("bar").enter(|| {}));
            baz.enter(|| span!("quux").enter(|| {}))
        });

        handle1.assert_finished();
        handle2.assert_finished();
    }

}
