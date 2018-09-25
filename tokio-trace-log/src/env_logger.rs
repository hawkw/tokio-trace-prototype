
extern crate env_logger;
use log;

pub fn try_init() -> Result<(), log::SetLoggerError> {
    env_logger::Builder::from_default_env()
        .format(|_, record| ::format_trace(record))
        .try_init()
}

pub fn init() {
    try_init().unwrap()
}
