use std::{cell::Cell, thread::LocalKey, sync::Mutex};
use {dispatcher::{self, Dispatch}, Meta, Span, Subscriber};

lazy_static! {
    static ref REGISTRY: Mutex<Registry> = Mutex::new(Registry {
        callsites: Vec::new(),
        dispatchers: Vec::new(),
    });
}

struct Registry {
    callsites: Vec<&'static dyn Callsite_>,
    dispatchers: Vec<dispatcher::FilterHandle>,
}

pub fn register(callsite: &'static dyn Callsite_) {
    let mut registry = REGISTRY.lock().unwrap();
    let meta = callsite.metadata();
    registry.dispatchers.retain(|dispatch| {
        match dispatch.enabled(meta) {
            Some(enabled) => {
                if enabled {
                    callsite.enable();
                }
                true
            },
            // TODO: if the dispatcher has been dropped, should we invalidate
            // any callsites that it previously enabled?
            None => false,
        }
    });
    registry.callsites.push(callsite);
}

pub(crate) fn register_dispatch(dispatch: &Dispatch) {
    let mut registry = REGISTRY.lock().unwrap();
    registry.dispatchers.push(dispatch.as_filter());
    for callsite in &registry.callsites {
        if dispatch.enabled(callsite.metadata()) {
            callsite.enable();
        }
    }
}

pub trait Callsite_: Sync {
    fn is_enabled(&self, dispatch: &Dispatch) -> bool;
    fn enable(&self);
    fn disable(&self);
    fn metadata(&self) -> &Meta;
}

#[derive(Debug)]
pub struct Callsite(pub(crate) &'static LocalKey<Cache<'static>>);

#[doc(hidden)]
pub struct Cache<'a> {
    pub(crate) last_filtered_by: Cell<usize>,
    pub(crate) cached_filter: Cell<Option<bool>>,
    pub(crate) meta: &'a Meta<'a>,
}

impl Callsite {
    #[doc(hidden)]
    pub fn new(cache: &'static LocalKey<Cache<'static>>) -> Self {
        Callsite(cache)
    }

    #[inline]
    pub(crate) fn metadata(&self) -> &'static Meta<'static> {
        self.0.with(|cache| cache.meta)
    }
}

impl<'a> Cache<'a> {
    pub fn new(meta: &'a Meta<'a>) -> Self {
        Self {
            last_filtered_by: Cell::new(0),
            cached_filter: Cell::new(None),
            meta,
        }
    }

    #[inline]
    pub fn metadata(&self) -> &'a Meta<'a> {
        self.meta
    }
}
