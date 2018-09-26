use super::{Event, SpanData, Meta};
use log;
use std::time::Instant;

pub trait Subscriber {
    /// Note that this function is generic over a pair of lifetimes because the
    /// `Event` type is. See the documentation for [`Event`] for details.
    fn observe_event<'event, 'meta: 'event>(&self, event: &'event Event<'event, 'meta>);
    fn enter(&self, span: &SpanData, at: Instant);
    fn exit(&self, span: &SpanData, at: Instant);
}

pub struct LogSubscriber;

impl LogSubscriber {
    pub fn new() -> Self {
        LogSubscriber
    }
}

impl Subscriber for LogSubscriber {
    fn observe_event<'event, 'meta: 'event>(&self, event: &'event Event<'event, 'meta>) {
        let fields = event.debug_fields();
        let meta = event.meta.into();
        let logger = log::logger();
        let parents = event.parents().filter_map(SpanData::name).collect::<Vec<_>>();
        if logger.enabled(&meta) {
            logger.log(
                &log::Record::builder()
                    .metadata(meta)
                    .module_path(Some(event.meta.module_path))
                    .file(Some(event.meta.file))
                    .line(Some(event.meta.line))
                    .args(format_args!(
                        "[{}] {:?} {}",
                        parents.join(":"),
                        fields,
                        event.message
                    )).build(),
            );
        }
    }

    fn enter(&self, span: &SpanData, _at: Instant) {
        let logger = log::logger();
        logger.log(&log::Record::builder()
            .args(format_args!("-> {:?}", span.name()))
            .build()
        )
    }
    fn exit(&self, span: &SpanData, _at: Instant) {
        let logger = log::logger();
        logger.log(&log::Record::builder().args(format_args!("<- {:?}", span.name())).build())
    }
}

impl<'a, 'b> Into<log::Metadata<'a>> for &'b Meta<'a> {
    fn into(self) -> log::Metadata<'a> {
        log::Metadata::builder()
            .level(self.level)
            .target(self.target.unwrap_or(""))
            .build()
    }
}

#[cfg(test)]
pub mod test_support {
    use super::Subscriber;
    use ::{Event, SpanData, Value};
    use std::{
        collections::{VecDeque, HashMap},
        sync::{Arc, Mutex},
        time::Instant,
    };

    struct ExpectEvent {
        // TODO: implement
    }

    pub struct ExpectSpan {
        name: Option<Option<&'static str>>,
        state: Option<::span::State>,
        fields: HashMap<String, Box<dyn Value>>,
        // TODO: more
    }

    enum Expect {
        Event(ExpectEvent),
        Enter(ExpectSpan),
        Exit(ExpectSpan),
    }

    struct MockSubscriber {
        expected: Arc<Mutex<VecDeque<Expect>>>,
    }

    pub struct MockSubscriberBuilder {
        expected: VecDeque<Expect>,
    }

    pub fn span() -> ExpectSpan {
        ExpectSpan {
            name: None,
            state: None,
            fields: HashMap::new(),
        }
    }

    pub fn mock() -> MockSubscriberBuilder {
        MockSubscriberBuilder {
            expected: VecDeque::new(),
        }
    }

    impl MockSubscriberBuilder {
        pub fn enter(&mut self, span: ExpectSpan) -> &mut Self {
            self.expected.push_back(Expect::Enter(span
                .with_state(::span::State::Running)));
            self
        }

        pub fn exit(&mut self, span: ExpectSpan) -> &mut Self {
            self.expected.push_back(Expect::Exit(span));
            self
        }

        pub fn set(self) {
            let subscriber = MockSubscriber {
                expected: Arc::new(Mutex::new(self.expected)),
            };
            ::Dispatcher::builder().add_subscriber(subscriber).init()
        }
    }

    impl ExpectSpan {
        pub fn named(mut self, name: Option<&'static str>) -> Self {
            self.name = Some(name);
            self
        }

        pub fn with_state(mut self, state: ::span::State) -> Self {
            self.state = Some(state);
            self
        }
    }

    impl Subscriber for MockSubscriber {
        fn observe_event<'event>(&self, event: &'event Event<'event>) {
            match self.expected.lock().unwrap().pop_front() {
                None => {}
                Some(Expect::Event(_)) => unimplemented!(),
                Some(Expect::Enter(expected_span)) => panic!("expected to enter span {:?}, but got an event", expected_span.name),
                Some(Expect::Exit(expected_span)) => panic!("expected to exit span {:?} but got an event", expected_span.name),
            }
        }

        fn enter(&self, span: &SpanData, _at: Instant) {
            println!("+ {:?}", span);
            match self.expected.lock().unwrap().pop_front() {
                None => {},
                Some(Expect::Event(_)) => panic!("expected an event, but entered span {:?} instead", span.name()),
                Some(Expect::Enter(expected_span)) => {
                    if let Some(name) = expected_span.name {
                        assert_eq!(name, span.name());
                    }
                    if let Some(state) = expected_span.state {
                        assert_eq!(state, span.state());
                    }
                    // TODO: expect fields
                }
                Some(Expect::Exit(expected_span)) => panic!(
                    "expected to exit span {:?}, but entered span {:?} instead",
                    expected_span.name,
                    span.name()),
            }
        }

        fn exit(&self, span: &SpanData, _at: Instant) {
            println!("- {:?}", span);
            match self.expected.lock().unwrap().pop_front() {
                None => {},
                Some(Expect::Event(_)) => panic!("expected an event, but exited span {:?} instead", span.name()),
                Some(Expect::Enter(expected_span)) => panic!(
                    "expected to enter span {:?}, but exited span {:?} instead",
                    expected_span.name,
                    span.name()),
                Some(Expect::Exit(expected_span)) => {
                    if let Some(name) = expected_span.name {
                        assert_eq!(name, span.name());
                    }
                    if let Some(state) = expected_span.state {
                        assert_eq!(state, span.state());
                    }
                    // TODO: expect fields
                }
            }
        }
    }
}
