use super::{Event, StaticMeta};
use ::log;

pub trait Subscriber {
    fn consume<'event>(&self, event: &'event Event<'event>);
}

pub struct LogSubscriber;

impl Subscriber for LogSubscriber {
    fn consume<'event>(&self, event: &'event Event<'event>) {
        let fields = event.debug_fields();
        let meta = event.static_meta.into();
        let logger = log::logger();
        if logger.enabled(&meta) {
            logger.log(&log::Record::builder()
                .metadata(meta)
                .module_path(Some(event.static_meta.module_path))
                .file(Some(event.static_meta.file))
                .line(Some(event.static_meta.line))
                .args(format_args!("{:?} {}", fields, event.message))
                .build());
        }
    }
}

impl<'a, 'b> Into<log::Metadata<'a>> for &'b StaticMeta {
    fn into(self) -> log::Metadata<'a> {
        log::Metadata::builder()
            .level(self.level)
            .target(self.target.unwrap_or(""))
            .build()
    }
}
