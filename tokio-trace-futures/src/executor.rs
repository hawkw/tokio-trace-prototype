use futures::{future::{Executor, ExecuteError}, Future};
use tokio_trace::Span;
use ::{Instrumented, Instrument};

pub trait InstrumentExecutor<F>
where
    Self: Executor<Instrumented<F>>,
    F: Future<Item = (), Error = ()>,
{
    fn instrument<G>(self, mk_span: G) -> InstrumentedExecutor<Self, G>
    where
        G: Fn() -> Span,
        Self: Sized,
    {
        InstrumentedExecutor {
            inner: self,
            mk_span,
        }
    }
}

#[derive(Clone, Debug)]
pub struct InstrumentedExecutor<T, G> {
    inner: T,
    mk_span: G,
}

impl<T, F> InstrumentExecutor<F> for T
where
    T: Executor<Instrumented<F>>,
    F: Future<Item = (), Error = ()>,
{ }

impl<T, F, N> Executor<F> for InstrumentedExecutor<T, N>
where
    T: Executor<Instrumented<F>>,
    F: Future<Item = (), Error = ()>,
    N: Fn() -> Span,
{
    fn execute(&self, future: F) -> Result<(), ExecuteError<F>> {
        self.inner
            .execute(future.instrument((self.mk_span)()))
            .map_err(|e| {
                let kind = e.kind();
                let future = e.into_future().inner;
                ExecuteError::new(kind, future)
            })
    }
}
