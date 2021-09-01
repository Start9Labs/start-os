mod cli;
mod rpc;

pub use cli::CliContext;
pub use rpc::RpcContext;

impl From<CliContext> for () {
    fn from(_: CliContext) -> Self {
        ()
    }
}
impl From<RpcContext> for () {
    fn from(_: RpcContext) -> Self {
        ()
    }
}

// TODO: these shouldn't be necessary

impl From<CliContext> for RpcContext {
    fn from(_: CliContext) -> Self {
        panic!("RPC Context used in CLI Handler")
    }
}

impl From<RpcContext> for CliContext {
    fn from(_: RpcContext) -> Self {
        panic!("CLI Context used in RPC Handler")
    }
}
