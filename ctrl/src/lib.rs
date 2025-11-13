pub mod error;
pub mod ethernet;
pub mod profiles;
pub mod uci;
pub mod utils;
pub mod wifi;

use clap::Parser;
pub use error::{Error, ErrorKind};
use rpc_toolkit::{Context, ParentHandler};
use tracing::subscriber::DefaultGuard;

#[derive(Clone, Parser)]
pub struct CliContext;
impl Context for CliContext {}

#[derive(Clone)]
pub struct ServerContext;
impl Context for ServerContext {}

pub fn main_api<C: Context + Clone>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("profiles", profiles::profiles::<C>())
        .subcommand("ethernet", ethernet::ethernet::<C>())
        .subcommand("wifi", wifi::wifi::<C>())
        .subcommand("uci", uci::uci::<C>())
}

pub fn init_logging(name: &str) -> DefaultGuard {
    use tracing_rfc_5424::{
        rfc3164::Rfc3164, tracing::TrivialTracingFormatter, transport::UnixSocket,
    };
    use tracing_subscriber::Registry;
    use tracing_subscriber::{
        layer::SubscriberExt, // Needed to get `with()`
    };

    // Setup the subsriber...
    let subscriber = Registry::default().with(
        tracing_rfc_5424::layer::Layer::<
            tracing_subscriber::Registry,
            Rfc3164,
            TrivialTracingFormatter,
            UnixSocket,
        >::try_default()
        .unwrap(),
    );
    // and install it.
    tracing::subscriber::set_default(subscriber)
}
