use super::{DebugFields, Dispatcher, Parents, StaticMeta, Subscriber, Value};
use std::{
    cell::RefCell,
    cmp, fmt,
    hash::{Hash, Hasher},
    slice,
    sync::Arc,
    time::Instant,
};

lazy_static! {
    static ref ROOT_SPAN: Span = Span {
        inner: Arc::new(SpanInner {
            name: Some("root"),
            opened_at: Instant::now(),
            parent: None,
            static_meta: &static_meta!(),
            field_values: Vec::new(),
        })
    };
}

thread_local! {
    static CURRENT_SPAN: RefCell<Span> = RefCell::new(ROOT_SPAN.clone());
}

#[derive(Clone, PartialEq, Hash)]
pub struct Span {
    inner: Arc<SpanInner>,
}

#[derive(Debug)]
pub(crate) struct SpanInner {
    pub name: Option<&'static str>,
    pub opened_at: Instant,

    pub parent: Option<Span>,

    pub static_meta: &'static StaticMeta,

    pub field_values: Vec<Box<dyn Value>>,
    // ...
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
        Span {
            inner: Arc::new(SpanInner {
                name,
                opened_at,
                parent: Some(parent),
                static_meta,
                field_values,
            }),
        }
    }

    pub fn current() -> Self {
        CURRENT_SPAN.with(|span| span.borrow().clone())
    }

    pub fn name(&self) -> Option<&'static str> {
        self.inner.name
    }

    pub fn parent(&self) -> Option<&Span> {
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

    pub fn enter<F: FnOnce() -> T, T>(&self, f: F) -> T {
        let result = CURRENT_SPAN.with(|current_span| {
            if *current_span.borrow() == *self
                || current_span.borrow().parents().any(|span| span == self)
            {
                return f();
            }

            current_span.replace(self.clone());
            Dispatcher::current().enter(&self, Instant::now());
            f()
        });

        CURRENT_SPAN.with(|current_span| {
            if let Some(parent) = self.parent() {
                current_span.replace(parent.clone());
                Dispatcher::current().exit(&self, Instant::now());
            }
        });

        result
    }

    pub fn debug_fields<'a>(&'a self) -> DebugFields<'a, Self> {
        DebugFields(self)
    }

    pub fn parents<'a>(&'a self) -> Parents<'a> {
        Parents { next: Some(self) }
    }
}

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

impl<'a> IntoIterator for &'a Span {
    type Item = (&'a str, &'a dyn Value);
    type IntoIter = Box<Iterator<Item = (&'a str, &'a dyn Value)> + 'a>; // TODO: unbox
    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.fields())
    }
}

impl fmt::Debug for Span {
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
