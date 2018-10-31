pub use tokio_trace_core::span::*;

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;
    use {span, subscriber, Dispatch};

    #[test]
    fn exit_doesnt_finish_while_handles_still_exist() {
        // Test that exiting a span only marks it as "done" when no handles
        // that can re-enter the span exist.
        let subscriber = subscriber::mock()
            .enter(span::mock().named(Some("foo")))
            .enter(span::mock().named(Some("bar")))
            // The first time we exit "bar", there will be another handle with
            // which we could potentially re-enter bar.
            .exit(span::mock().named(Some("bar")).with_state(State::Idle))
            // Re-enter "bar", using the cloned handle.
            .enter(span::mock().named(Some("bar")))
            // Now, when we exit "bar", there is no handle to re-enter it, so
            // it should become "done".
            .exit(span::mock().named(Some("bar")).with_state(State::Done))
            // "foo" never had more than one handle, so it should also become
            // "done" when we exit it.
            .exit(span::mock().named(Some("foo")).with_state(State::Done))
            .run();

        Dispatch::to(subscriber).as_default(|| {
            span!("foo",).enter(|| {
                let bar = span!("bar",);
                bar.clone().enter(|| {
                    // do nothing. exiting "bar" should leave it idle, since it can
                    // be re-entered.
                });
                bar.enter(|| {
                    // enter "bar" again. this time, the last handle is used, so
                    // "bar" should be marked as done.
                });
            });
        });
    }

    #[test]
    fn exit_doesnt_finish_concurrently_executing_spans() {
        // Test that exiting a span only marks it as "done" when no other
        // threads are still executing inside that span.
        use std::sync::{Arc, Barrier};

        let subscriber = subscriber::mock()
            .enter(span::mock().named(Some("baz")))
            // Main thread enters "quux".
            .enter(span::mock().named(Some("quux")))
            // Spawned thread also enters "quux".
            .enter(span::mock().named(Some("quux")))
            // When the main thread exits "quux", it will still be running in the
            // spawned thread.
            .exit(span::mock().named(Some("quux")).with_state(State::Running))
            // Now, when this thread exits "quux", there is no handle to re-enter it, so
            // it should become "done".
            .exit(span::mock().named(Some("quux")).with_state(State::Done))
            // "baz" never had more than one handle, so it should also become
            // "done" when we exit it.
            .exit(span::mock().named(Some("baz")).with_state(State::Done))
            .run();

        Dispatch::to(subscriber).as_default(|| {
            let barrier1 = Arc::new(Barrier::new(2));
            let barrier2 = Arc::new(Barrier::new(2));
            // Make copies of the barriers for thread 2 to wait on.
            let t2_barrier1 = barrier1.clone();
            let t2_barrier2 = barrier2.clone();

            span!("baz",).enter(move || {
                let quux = span!("quux",);
                let quux2 = quux.clone();
                let handle = thread::Builder::new()
                    .name("thread-2".to_string())
                    .spawn(move || {
                        quux2.enter(|| {
                            // Once this thread has entered "quux", allow thread 1
                            // to exit.
                            t2_barrier1.wait();
                            // Wait for the main thread to allow us to exit.
                            t2_barrier2.wait();
                        })
                    }).expect("spawn test thread");
                quux.enter(|| {
                    // Wait for thread 2 to enter "quux". When we exit "quux", it
                    // should stay running, since it's running in the other thread.
                    barrier1.wait();
                });
                // After we exit "quux", wait for the second barrier, so the other
                // thread unblocks and exits "quux".
                barrier2.wait();
                handle.join().unwrap();
            });
        });
    }

    #[test]
    fn handles_to_the_same_span_are_equal() {
        // Create a mock subscriber that will return `true` on calls to
        // `Subscriber::enabled`, so that the spans will be constructed. We
        // won't enter any spans in this test, so the subscriber won't actually
        // expect to see any spans.
        Dispatch::to(subscriber::mock().run()).as_default(|| {
            let foo1 = span!("foo");
            let foo2 = foo1.clone();

            // Two handles that point to the same span are equal.
            assert_eq!(foo1, foo2);

            // // The two span's data handles are also equal.
            // assert_eq!(foo1.data(), foo2.data());
        });
    }

    #[test]
    fn handles_to_different_spans_are_not_equal() {
        Dispatch::to(subscriber::mock().run()).as_default(|| {
            // Even though these spans have the same name and fields, they will have
            // differing metadata, since they were created on different lines.
            let foo1 = span!("foo", bar = &1, baz = &false);
            let foo2 = span!("foo", bar = &1, baz = &false);

            assert_ne!(foo1, foo2);
            // assert_ne!(foo1.data(), foo2.data());
        });
    }

    #[test]
    fn handles_to_different_spans_with_the_same_metadata_are_not_equal() {
        // Every time time this function is called, it will return a _new
        // instance_ of a span with the same metadata, name, and fields.
        fn make_span() -> Span {
            span!("foo", bar = &1, baz = &false)
        }

        Dispatch::to(subscriber::mock().run()).as_default(|| {
            let foo1 = make_span();
            let foo2 = make_span();

            assert_ne!(foo1, foo2);
            // assert_ne!(foo1.data(), foo2.data());
        });
    }

    #[test]
    fn spans_always_go_to_the_subscriber_that_tagged_them() {
        let subscriber1 = subscriber::mock()
            .enter(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")).with_state(State::Idle))
            .enter(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")).with_state(State::Done));
        let subscriber1 = Dispatch::to(subscriber1.run());
        let subscriber2 = Dispatch::to(subscriber::mock().run());

        let foo = subscriber1.as_default(|| {
            let foo = span!("foo");
            foo.clone().enter(|| {});
            foo
        });
        // Even though we enter subscriber 2's context, the subscriber that
        // tagged the span should see the enter/exit.
        subscriber2.as_default(move || foo.enter(|| {}));
    }

    #[test]
    fn spans_always_go_to_the_subscriber_that_tagged_them_even_across_threads() {
        let subscriber1 = subscriber::mock()
            .enter(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")).with_state(State::Idle))
            .enter(span::mock().named(Some("foo")))
            .exit(span::mock().named(Some("foo")).with_state(State::Done));
        let subscriber1 = Dispatch::to(subscriber1.run());
        let foo = subscriber1.as_default(|| {
            let foo = span!("foo");
            foo.clone().enter(|| {});
            foo
        });

        // Even though we enter subscriber 2's context, the subscriber that
        // tagged the span should see the enter/exit.
        thread::spawn(move || {
            Dispatch::to(subscriber::mock().run()).as_default(|| {
                foo.enter(|| {});
            })
        }).join()
        .unwrap();
    }
}
