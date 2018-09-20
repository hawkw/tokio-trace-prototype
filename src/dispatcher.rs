use ::{
    Event,
    subscriber::Subscriber
};

use std::{
    fmt,
    sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT},
};

pub(crate) trait Dispatch {
    fn broadcast<'event>(&self, event: &'event Event<'event>);
}

static STATE: AtomicUsize = ATOMIC_USIZE_INIT;

// There are three different states that we care about: the logger's
// uninitialized, the logger's initializing (set_logger's been called but
// LOGGER hasn't actually been set yet), or the logger's active.
const UNINITIALIZED: usize = 0;
const INITIALIZING: usize = 1;
const INITIALIZED: usize = 2;
static mut DISPATCHER: &'static Dispatch = &NoDispatcher;

#[derive(Default)]
pub struct Builder {
    subscribers: Vec<Box<dyn Subscriber>>,
}

impl Builder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_subscriber<T: Subscriber + 'static>(&mut self, subscriber: T) -> &mut Self {
        self.subscribers.push(Box::new(subscriber));
        self
    }

    pub fn try_init(self) -> Result<(), InitError> {
        unsafe {
            match STATE.compare_and_swap(UNINITIALIZED, INITIALIZING, Ordering::SeqCst) {
                UNINITIALIZED => {
                    DISPATCHER = &*Box::into_raw(Box::new(self));
                    STATE.store(INITIALIZED, Ordering::SeqCst);
                    Ok(())
                }
                INITIALIZING => {
                    while STATE.load(Ordering::SeqCst) == INITIALIZING {}
                    Err(InitError)
                }
                _ => Err(InitError),
            }
        }
    }

    pub fn init(self) {
        self.try_init().unwrap()
    }
}

impl Dispatch for Builder {
    fn broadcast<'event>(&self, event: &'event Event<'event>) {
        for subscriber in &self.subscribers {
            subscriber.observe(event)
        }
    }
}

pub struct Dispatcher(&'static Dispatch);

impl Dispatcher {
    pub fn current() -> Self {
        Dispatcher(unsafe { DISPATCHER })
    }
}

impl Dispatch for Dispatcher {
    #[inline]
    fn broadcast<'event>(&self, event: &'event Event<'event>) {
        self.0.broadcast(event)
    }
}

struct NoDispatcher;

#[derive(Debug)]
pub struct InitError;

impl Dispatch for NoDispatcher {
    fn broadcast<'event>(&self, _: &'event Event<'event>) {
        // Do nothing.
        // TODO: should this panic instead?
    }
}
