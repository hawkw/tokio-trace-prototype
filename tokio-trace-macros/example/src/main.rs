#[macro_use]
extern crate tokio_trace;
#[macro_use]
extern crate tokio_trace_macros;
use tokio_trace::Level;

#[path = "../../../tokio-trace/examples/sloggish/sloggish_subscriber.rs"]
mod sloggish;
use self::sloggish::SloggishSubscriber;

fn main() {
    tokio_trace::Dispatcher::builder()
        .add_subscriber(SloggishSubscriber::new(2))
        .init();

    let server_span = span!("server", local_port = 8888);
    server_span.clone().enter(move || {
        suggest_band("Parquet Courts");
    });
}

#[trace]
#[deprecated]
#[warn(missing_docs)]
fn suggest_band(band: &str) -> String {
    event!(Level::Info, { band = band.to_string() }, "suggested a band");
    format!("Have you listened to {}?", band)
}
