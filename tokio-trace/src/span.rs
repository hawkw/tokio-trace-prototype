use super::{DebugFields, Dispatcher, Parents, StaticMeta, Subscriber, Value};
use std::{
    cell::RefCell,
    cmp, fmt,
    hash::{Hash, Hasher},
    slice,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, Weak, Mutex,
    },
    time::Instant,
};

lazy_static! {
    static ref ROOT_SPAN: Span = {
        let enter_count = Arc::new(());
        let can_enter = Arc::downgrade(&enter_count);
        Span {
            inner: Arc::new(SpanInner {
                name: Some("root"),
                opened_at: Instant::now(),
                parent: None,
                static_meta: &static_meta!(),
                field_values: Vec::new(),
                state: AtomicUsize::new(State::Running as usize),
                enter_count,
                can_enter_parent: Mutex::new(None),
                currently_entered: AtomicUsize::new(0),
            }),
            can_enter,
        }
    };
}

thread_local! {
    static CURRENT_SPAN: RefCell<Span> = RefCell::new(ROOT_SPAN.clone());
}

/// A handle on a `Span` that allows access to the span's data and may be used
/// to enter the span.
#[derive(Clone)]
pub struct Span {
    inner: Arc<SpanInner>,
    can_enter: Weak<()>,
}

/// A handle on the data associated with a span.
///
/// This may be used to access the span but may *not* be used to enter the span.
#[derive(Clone, PartialEq, Hash)]
pub struct Data {
    inner: Arc<SpanInner>,
}

#[derive(Debug)]
struct SpanInner {
    pub name: Option<&'static str>,
    pub opened_at: Instant,

    pub parent: Option<Data>,

    pub static_meta: &'static StaticMeta,

    pub field_values: Vec<Box<dyn Value>>,

    state: AtomicUsize,

    /// Used for counting the number of currently-live `Span` references that
    /// may enter this span.
    enter_count: Arc<()>,

    // TODO: ag i hate this
    can_enter_parent: Mutex<Option<Weak<()>>>,

    /// The number of threads which have entered this span.
    ///
    /// Incremented on enter and decremented on exit.
    currently_entered: AtomicUsize,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
#[repr(usize)]
pub enum State {
    /// The span has been created but has yet to be entered.
    Unentered,
    /// A thread is currently executing inside the span or one of its children.
    Running,
    /// The span has previously been entered, but is not currently
    /// executing. However, it is not done and may be entered again.
    Idle,
    /// The span has completed.
    ///
    /// It will *not* be entered again (and may be dropped once all
    /// subscribers have finished processing it).
    Done,
}

// ===== impl Span =====

impl Span {
    pub fn new(
        name: Option<&'static str>,
        opened_at: Instant,
        parent: Span,
        static_meta: &'static StaticMeta,
        field_values: Vec<Box<dyn Value>>,
    ) -> Self {
        let enter_count = Arc::new(());
        let (parent, can_enter_parent) = parent.split();
        let can_enter_parent = Mutex::new(Some(can_enter_parent));
        let can_enter = Arc::downgrade(&enter_count);
        Span {
            inner: Arc::new(SpanInner {
                name,
                opened_at,
                parent: Some(parent),
                static_meta,
                field_values,
                state: AtomicUsize::new(State::Unentered as usize),
                enter_count,
                can_enter_parent,
                currently_entered: AtomicUsize::new(0),
            }),
            can_enter,
        }
    }

    pub fn current() -> Self {
        CURRENT_SPAN.with(|span| span.borrow().clone())
    }

    pub fn name(&self) -> Option<&'static str> {
        self.inner.name
    }

    pub fn parent(&self) -> Option<&Data> {
        self.inner.parent.as_ref()
    }

    pub fn meta(&self) -> &'static StaticMeta {
        self.inner.static_meta
    }

    pub fn field_names(&self) -> slice::Iter<&'static str> {
        self.inner.static_meta.field_names.iter()
    }

    pub fn fields<'a>(&'a self) -> impl Iterator<Item = (&'a str, &'a dyn Value)> {
        self.field_names()
            .enumerate()
            .map(move |(idx, &name)| (name, self.inner.field_values[idx].as_ref()))
    }

    pub fn state(&self) -> State {
        match self.inner.state.load(Ordering::Acquire) {
            s if s == State::Unentered as usize => State::Unentered,
            s if s == State::Running as usize => State::Running,
            s if s == State::Idle as usize => State::Idle,
            s if s == State::Done as usize => State::Done,
            invalid => panic!("invalid state: {:?}", invalid),
        }
    }

    pub fn enter<F: FnOnce() -> T, T>(self, f: F) -> T {
        let initial_state = self.state();
        match initial_state {
            // The span has been marked as done; it may not be reentered again.
            // TODO: maybe this should not crash the thread?
            State::Done => panic!("cannot re-enter completed span!"),
            _ => {
                let result = CURRENT_SPAN.with(|current_span| {
                    self.inner
                        .currently_entered
                        .fetch_add(1, Ordering::Release);
                    current_span.replace(self.clone());
                    self.inner.state.compare_and_swap(
                        initial_state as usize,
                        State::Running as usize,
                        Ordering::Release,
                    );
                    Dispatcher::current().enter(&self.clone().downgrade(), Instant::now());
                    f()
                });

                CURRENT_SPAN.with(|current_span| {
                    // Welcome to the "state transition on exit" logic, likely the
                    // most complex code in all of tokio-trace.
                    let timestamp = Instant::now();
                    if let Some(parent) = self.parent() {
                        current_span.replace(parent.clone().upgrade());
                    }
                    let remaining_exits = self.inner
                        .currently_entered
                        .fetch_sub(1, Ordering::AcqRel);
                    // Only advance the state if we are the last remaining
                    // thread to exit the span.
                    if remaining_exits == 1 {
                         // If we are the only remaining enter handle to this
                        // span, it can now transition to Done. Otherwise, it
                        // transitions to Idle.
                        let next_state = if self.inner.remaining_enters() <= 1 {
                            // This span can no longer enter its parent, so
                            // drop the can_enter_parent marker.
                            drop(self.inner
                                .can_enter_parent
                                .lock().expect("poisoned lock")
                                .take()
                            );
                            State::Done
                        } else {
                            State::Idle
                        };
                        self.inner.state.compare_and_swap(
                            State::Running as usize,
                            next_state as usize,
                            Ordering::Release,
                        );
                    }
                    Dispatcher::current().exit(&self.downgrade(), timestamp);
                });
                result
            }
        }
    }

    pub fn debug_fields<'a>(&'a self) -> DebugFields<'a, Self> {
        DebugFields(self)
    }

    pub fn parents<'a>(&'a self) -> Parents<'a> {
        // agh fix
        unimplemented!()
    }

    pub fn downgrade(self) -> Data {
        Data {
            inner: self.inner,
        }
    }

    fn split(self) -> (Data, Weak<()>) {
        let data = Data {
            inner: self.inner,
        };
        (data, self.can_enter)
    }
}


impl cmp::PartialEq for Span {
    fn eq(&self, other: &Span) -> bool {
        self.inner == other.inner
    }
}

impl Hash for Span {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}

impl<'a> IntoIterator for &'a Span {
    type Item = (&'static str, &'a dyn Value);
    type IntoIter = Box<Iterator<Item = (&'static str, &'a dyn Value)> + 'a>; // TODO: unbox
    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.fields())
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Span")
            .field("name", &self.inner.name)
            .field("opened_at", &self.inner.opened_at)
            .field("parent", &self.parent().map(Data::name).unwrap_or_else(|| self.name()))
            .field("fields", &self.debug_fields())
            .field("meta", &self.meta())
            .finish()
    }
}


// ===== impl Data =====

impl Data {
    fn upgrade(self) -> Span {
        let can_enter = Arc::downgrade(&self.inner.enter_count);
        Span {
            inner: self.inner,
            can_enter,
        }
    }


    pub fn name(&self) -> Option<&'static str> {
        self.inner.name
    }

    pub fn parent(&self) -> Option<&Data> {
        self.inner.parent.as_ref()
    }

    pub fn meta(&self) -> &'static StaticMeta {
        self.inner.static_meta
    }

    pub fn field_names(&self) -> slice::Iter<&'static str> {
        self.inner.static_meta.field_names.iter()
    }

    pub fn fields<'a>(&'a self) -> impl Iterator<Item = (&'static str, &'a dyn Value)> {
        self.field_names()
            .enumerate()
            .map(move |(idx, &name)| (name, self.inner.field_values[idx].as_ref()))
    }

    pub fn debug_fields<'a>(&'a self) -> DebugFields<'a, Self> {
        DebugFields(self)
    }

    pub fn state(&self) -> State {
        match self.inner.state.load(Ordering::Acquire) {
            s if s == State::Unentered as usize => State::Unentered,
            s if s == State::Running as usize => State::Running,
            s if s == State::Idle as usize => State::Idle,
            s if s == State::Done as usize => State::Done,
            invalid => panic!("invalid state: {:?}", invalid),
        }
    }
}

impl<'a> IntoIterator for &'a Data {
    type Item = (&'static str, &'a dyn Value);
    type IntoIter = Box<Iterator<Item = (&'static str, &'a dyn Value)> + 'a>; // TODO: unbox
    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.fields())
    }
}

impl fmt::Debug for Data {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Span")
            .field("name", &self.inner.name)
            .field("opened_at", &self.inner.opened_at)
            .field("parent", &self.parent().unwrap_or(self).name())
            .field("fields", &self.debug_fields())
            .field("meta", &self.meta())
            .finish()
    }
}

// ===== impl SpanInner =====

impl cmp::PartialEq for SpanInner {
    fn eq(&self, other: &SpanInner) -> bool {
        self.opened_at == other.opened_at
            && self.name == other.name
            && self.static_meta == other.static_meta
    }
}

impl Hash for SpanInner {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.opened_at.hash(state);
        self.name.hash(state);
        self.static_meta.hash(state);
    }
}

impl SpanInner {
    fn remaining_enters(&self) -> usize {
        Arc::weak_count(&self.enter_count)
    }
}


#[cfg(test)]
pub use self::test_support::*;
#[cfg(test)]
mod test_support {
    use ::{ Value, span::State };
    use std::collections::HashMap;

    pub struct MockSpan {
        pub name: Option<Option<&'static str>>,
        pub state: Option<State>,
        pub fields: HashMap<String, Box<dyn Value>>,
        // TODO: more
    }

    pub fn mock() -> MockSpan {
        MockSpan {
            name: None,
            state: None,
            fields: HashMap::new(),
        }
    }

    impl MockSpan {
        pub fn named(mut self, name: Option<&'static str>) -> Self {
            self.name = Some(name);
            self
        }

        pub fn with_state(mut self, state: State) -> Self {
            self.state = Some(state);
            self
        }

        // TODO: fields, etc
    }
}

#[cfg(test)]
mod tests {
    use ::{subscriber, span};
    use super::*;

    #[test]
    fn exit_doesnt_finish_while_handles_still_exist() {
        // Test that exiting a span only marks it as "done" when no handles
        // that can re-enter the span exist.
        let _running = subscriber::mock()
            .enter(span::mock().named(Some("foo")))
            .enter(span::mock().named(Some("bar")))
            // The first time we exit "bar", there will be another handle with
            // which we could potentially re-enter bar.
            .exit(span::mock().named(Some("bar"))
                .with_state(State::Idle))
            // Re-enter "bar", using the cloned handle.
            .enter(span::mock().named(Some("bar")))
            // Now, when we exit "bar", there is no handle to re-enter it, so
            // it should become "done".
            .exit(span::mock().named(Some("bar"))
                .with_state(State::Done)
            )
            // "foo" never had more than one handle, so it should also become
            // "done" when we exit it.
            .exit(span::mock().named(Some("foo"))
                .with_state(State::Done)
            )
            .run();

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
    }

    #[test]
    fn exit_doesnt_finish_concurrently_executing_spans() {
        // Test that exiting a span only marks it as "done" when no other
        // threads are still executing inside that span.
        use std::{thread, sync::{Arc, Barrier}};

        let barrier1 = Arc::new(Barrier::new(2));
        let barrier2 = Arc::new(Barrier::new(2));
        // Make copies of the barriers for thread 2 to wait on.
        let t2_barrier1 = barrier1.clone();
        let t2_barrier2 = barrier2.clone();

        subscriber::mock()
            .enter(span::mock().named(Some("baz")))
            .enter(span::mock().named(Some("quux")))
            // When the main thread exits "quux", it will still be running in the
            // spawned thread.
            .exit(span::mock().named(Some("quux"))
                .with_state(State::Running))
            // "baz" never had more than one handle, so it should also become
            // "done" when we exit it.
            .exit(span::mock().named(Some("baz"))
                .with_state(State::Done)
            )
            .run();

        span!("baz",).enter(|| {
            let quux = span!("quux",);
            let quux2 = quux.clone();
            let handle = thread::Builder::new()
                .name("thread-2".to_string())
                .spawn(move || {
                    subscriber::mock()
                        // Spawned thread also enters "quux".
                        .enter(span::mock().named(Some("quux")))
                        // Now, when this thread exits "quux", there is no handle to re-enter it, so
                        // it should become "done".
                        .exit(span::mock().named(Some("quux"))
                            .with_state(State::Done)
                        )
                        .run();
                    quux2.enter(|| {
                        // Once this thread has entered "quux", allow thread 1
                        // to exit.
                        t2_barrier1.wait();
                        // Wait for the main thread to allow us to exit.
                        t2_barrier2.wait();
                    })
                })
                .expect("spawn test thread");
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
    }

}
