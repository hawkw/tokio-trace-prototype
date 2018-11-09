use std::sync::Mutex;
use {dispatcher::{self, Dispatch}, Meta, subscriber::{Subscriber, Interest}};

lazy_static! {
    static ref REGISTRY: Mutex<Registry> = Mutex::new(Registry {
        callsites: Vec::new(),
        dispatchers: Vec::new(),
    });
}

struct Registry {
    callsites: Vec<&'static dyn Callsite>,
    dispatchers: Vec<dispatcher::Registrar>,
}

/// Register a new `Callsite` with the global registry.
///
/// This should be called once per callsite after the callsite has been constructed.
pub fn register(callsite: &'static dyn Callsite) {
    let mut registry = REGISTRY.lock().unwrap();
    let meta = callsite.metadata();
    registry.dispatchers.retain(|registrar| {
        match registrar.try_register(meta) {
            Some(interest) => {
                callsite.add_interest(interest);
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
    registry.dispatchers.push(dispatch.registrar());
    for callsite in &registry.callsites {
        let interest = dispatch.register_callsite(callsite.metadata());
        callsite.add_interest(interest);
    }
}

/// Reset the registry. This is typically only useful in tests.
#[cfg(any(test, feature = "test-support"))]
pub fn reset_registry() {
    let mut registry = REGISTRY.lock().unwrap();
    registry.callsites.clear();
    registry.dispatchers.clear();
}

pub trait Callsite: Sync {
    fn is_enabled(&self, dispatch: &Dispatch) -> bool;
    fn add_interest(&self, interest: Interest);
    fn remove_interest(&self);
    fn metadata(&self) -> &Meta;
}
