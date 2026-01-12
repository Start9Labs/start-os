pub mod error;
pub mod ethernet;
pub mod files;
pub mod profiles;
pub mod uci;
pub mod utils;
pub mod wifi;

use clap::Parser;
pub use error::{Error, ErrorKind};
use imbl_value::Value;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{
    call_remote_http,
    reqwest::{Client, Url},
    CallRemote, Context, Empty, ParentHandler,
};
use std::path::PathBuf;
use std::sync::{Arc, OnceLock};
use tokio::runtime::Runtime;
use tracing::subscriber::DefaultGuard;

pub trait CtrlContext: Context + Clone {
    fn uci_root(&self) -> PathBuf;
    fn effectful(&self) -> bool;
}

#[derive(Clone, Parser)]
pub struct CliContext {
    #[clap(long, default_value = "/etc/config")]
    pub config_root: PathBuf,
    #[clap(long)]
    pub configs_only: bool,
    #[clap(long, default_value = "http://127.0.0.1:3301")]
    pub host: Url,
    #[clap(skip)]
    client: OnceLock<Client>,
    #[clap(skip)]
    runtime: OnceLock<Arc<Runtime>>,
}

impl CliContext {
    pub fn client(&self) -> &Client {
        self.client.get_or_init(Client::new)
    }
}

impl Context for CliContext {
    fn runtime(&self) -> Option<Arc<Runtime>> {
        Some(
            self.runtime
                .get_or_init(|| {
                    Arc::new(
                        tokio::runtime::Builder::new_current_thread()
                            .enable_all()
                            .build()
                            .unwrap(),
                    )
                })
                .clone(),
        )
    }
}

impl CtrlContext for CliContext {
    fn uci_root(&self) -> PathBuf {
        self.config_root.clone()
    }

    fn effectful(&self) -> bool {
        self.configs_only
    }
}

impl CallRemote<ServerContext> for CliContext {
    async fn call_remote(
        &self,
        method: &str,
        params: Value,
        _extra: Empty,
    ) -> Result<Value, RpcError> {
        call_remote_http(self.client(), self.host.clone(), method, params).await
    }
}

#[derive(Clone)]
pub struct ServerContext;
impl Context for ServerContext {}
impl CtrlContext for ServerContext {
    fn uci_root(&self) -> PathBuf {
        "/etc/config/".into()
    }

    fn effectful(&self) -> bool {
        false
    }
}

pub fn main_api<C: CtrlContext + Clone>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("profiles", profiles::profiles::<C>())
        .subcommand("ethernet", ethernet::ethernet::<C>())
        .subcommand("wifi", wifi::wifi::<C>())
        .subcommand("uci", uci::uci::<C>())
        .subcommand("file", files::file::<C>())
        .subcommand("dir", files::dir::<C>())
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
