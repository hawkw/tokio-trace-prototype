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

    pub state: AtomicUsize,

    /// Used for counting the number of currently-live `Span` references that
    /// may enter this span.
    enter_count: Arc<()>,

    // TODO: ag i hate this
    can_enter_parent: Mutex<Option<Weak<()>>>,
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
            // The span has already been entered, so we don't need to enter it
            // again. Just run the body and return the result.
            State::Running => f(),
            _ => {
                let result = CURRENT_SPAN.with(|current_span| {
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
                    if let Some(parent) = self.parent() {
                        current_span.replace(parent.clone().upgrade());
                        Dispatcher::current().exit(&self.clone().downgrade(), Instant::now());

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
                });
                result
            }
        }
    }

    pub fn debug_fields<'a>(&'a self) -> DebugFields<'a, Self> {
        DebugFields(self)
    }

    pub fn parents<'a>(&self) -> Parents<'a> {
        Parents { next: Some(&self.clone().downgrade()) }
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
