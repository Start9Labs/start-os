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
