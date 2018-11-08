use std::{cell::RefCell};
use {dispatcher::{self, Dispatch}, Meta, Span, subscriber::{Subscriber, Interest}};

thread_local! {
    static REGISTRY: RefCell<Registry> = RefCell::new(Registry::default());
}

#[derive(Default)]
struct Registry {
    callsites: Vec<&'static dyn Callsite>,
    dispatchers: Vec<dispatcher::Registrar>,
}

pub fn register(callsite: &'static dyn Callsite) {
    REGISTRY.with(|registry| {
        let mut registry = registry.borrow_mut();
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
    })
}

pub(crate) fn register_dispatch(dispatch: &Dispatch) {
    REGISTRY.with(|registry| {
        let mut registry = registry.borrow_mut();
        registry.dispatchers.push(dispatch.registrar());
        for callsite in &registry.callsites {
            let interest = dispatch.register_callsite(callsite.metadata());
            callsite.add_interest(interest);
        }
    })
}

pub trait Callsite: Sync {
    fn is_enabled(&self, dispatch: &Dispatch) -> bool;
    fn add_interest(&self, interest: Interest);
    fn metadata(&self) -> &Meta;
}
