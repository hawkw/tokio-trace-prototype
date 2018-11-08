use std::{cell::Cell, thread::LocalKey, sync::Mutex};
use {Dispatch, Meta, Span, Subscriber};

lazy_static! {
    static ref REGISTRY: Mutex<Registry> = Mutex::new(Registry {
        callsites: Vec::new(),
        dispatchers: Vec::new(),
    });
}

struct Registry {
    callsites: Vec<&'static dyn Callsite_>,
    dispatchers: Vec<Dispatch>,
}

pub fn register_callsite(callsite: &'static dyn Callsite_) {
    let mut registry = REGISTRY.lock().unwrap();
    for dispatch in &registry.dispatchers {
        if dispatch.enabled(callsite.metadata()) {
            callsite.enable();
        }
    }
    registry.callsites.push(callsite);
}

pub(crate) fn register_dispatch(dispatch: &Dispatch) {
    let mut registry = REGISTRY.lock().unwrap();
    for callsite in &registry.callsites {
        if dispatch.enabled(callsite.metadata()) {
            callsite.enable();
        }
    }
    registry.dispatchers.push(dispatch.clone());
}

pub(crate) fn deregister_dispatch(id: usize) {
    let mut registry = REGISTRY.lock().unwrap();
    registry.dispatchers.retain(|d| d.id() != id);
    // TODO: should probably check if any callsites are no longer enabled...
}

pub trait Callsite_: Sync {
    fn is_enabled(&self, dispatch: &Dispatch) -> bool;
    fn enable(&self);
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
