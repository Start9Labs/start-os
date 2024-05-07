use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::Parser;
use imbl_value::Value;
use once_cell::sync::OnceCell;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{call_remote_socket, yajrc, CallRemote, Context, Empty};
use tokio::runtime::Runtime;

use crate::lxc::HOST_RPC_SERVER_SOCKET;
use crate::service::service_effect_handler::EffectContext;

#[derive(Debug, Default, Parser)]
pub struct ContainerClientConfig {
    #[arg(long = "socket")]
    pub socket: Option<PathBuf>,
}

pub struct ContainerCliSeed {
    socket: PathBuf,
    runtime: OnceCell<Runtime>,
}

#[derive(Clone)]
pub struct ContainerCliContext(Arc<ContainerCliSeed>);
impl ContainerCliContext {
    pub fn init(cfg: ContainerClientConfig) -> Self {
        Self(Arc::new(ContainerCliSeed {
            socket: cfg
                .socket
                .unwrap_or_else(|| Path::new("/media/startos/rpc").join(HOST_RPC_SERVER_SOCKET)),
            runtime: OnceCell::new(),
        }))
    }
}
impl Context for ContainerCliContext {
    fn runtime(&self) -> tokio::runtime::Handle {
        self.0
            .runtime
            .get_or_init(|| {
                tokio::runtime::Builder::new_current_thread()
                    .enable_all()
                    .build()
                    .unwrap()
            })
            .handle()
            .clone()
    }
}

impl CallRemote<EffectContext> for ContainerCliContext {
    async fn call_remote(&self, method: &str, params: Value, _: Empty) -> Result<Value, RpcError> {
        call_remote_socket(
            tokio::net::UnixStream::connect(&self.0.socket)
                .await
                .map_err(|e| RpcError {
                    data: Some(e.to_string().into()),
                    ..yajrc::INTERNAL_ERROR
                })?,
            method,
            params,
        )
        .await
    }
}
