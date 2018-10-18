extern crate http;
extern crate tower_service;
#[macro_use]
extern crate tokio_trace;
extern crate futures;
extern crate tokio_trace_futures;

use futures::{Future, Poll};
use tokio_trace_futures::{Instrument, Instrumented};
use tower_service::{NewService, Service};

#[derive(Clone, Debug)]
pub struct InstrumentedHttpService<T> {
    inner: T,
    span: tokio_trace::Span,
}

impl<T> InstrumentedHttpService<T> {
    pub fn new<B>(inner: T, span: tokio_trace::Span) -> Self
    where
        T: Service<Request = http::Request<B>>,
    {
        Self { inner, span }
    }
}

#[derive(Clone, Debug)]
pub struct InstrumentedNewService<T> {
    inner: T,
}

impl<T> InstrumentedNewService<T> {
    pub fn new<B>(inner: T) -> Self
    where
        T: NewService<Request = http::Request<B>>,
    {
        Self { inner }
    }
}

impl<T, B> NewService for InstrumentedNewService<T>
where
    T: NewService<Request = http::Request<B>>,
{
    type Request = T::Request;
    type Response = T::Response;
    type Error = T::Error;
    type InitError = T::InitError;
    type Service = InstrumentedHttpService<T::Service>;
    type Future = InstrumentedNewServiceFuture<T>;

    fn new_service(&self) -> Self::Future {
        let span = tokio_trace::Span::current();
        let inner = self.inner.new_service();
        InstrumentedNewServiceFuture { inner, span }
    }
}

pub struct InstrumentedNewServiceFuture<T>
where
    T: NewService,
{
    inner: T::Future,
    span: tokio_trace::Span,
}

impl<T, B> Future for InstrumentedNewServiceFuture<T>
where
    T: NewService<Request = http::Request<B>>,
{
    type Item = InstrumentedHttpService<T::Service>;
    type Error = T::InitError;
    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        let span = self.span.clone();
        let inner = &mut self.inner;
        span.clone().enter(move || {
            inner
                .poll()
                .map(|ready| ready.map(|svc| InstrumentedHttpService::new(svc, span.clone())))
        })
    }
}

impl<T, B> Service for InstrumentedHttpService<T>
where
    T: Service<Request = http::Request<B>>,
{
    type Request = T::Request;
    type Response = T::Response;
    type Future = Instrumented<T::Future>;
    type Error = T::Error;

    fn poll_ready(&mut self) -> futures::Poll<(), Self::Error> {
        let span = self.span.clone();
        let inner = &mut self.inner;
        span.enter(move || inner.poll_ready())
    }

    fn call(&mut self, request: Self::Request) -> Self::Future {
        let span = self.span.clone();
        let inner = &mut self.inner;
        span.enter(move || {
            let request_span = span!(
                "request",
                method = request.method(),
                version = &request.version(),
                uri = request.uri(),
                headers = request.headers()
            );
            request_span
                .clone()
                .enter(move || inner.call(request).instrument(request_span))
        })
    }
}
