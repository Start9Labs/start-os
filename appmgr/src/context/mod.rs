pub mod cli;
pub mod recovery;
pub mod rpc;
pub mod setup;

pub use cli::CliContext;
pub use recovery::RecoveryContext;
pub use rpc::RpcContext;
pub use setup::SetupContext;

impl From<CliContext> for () {
    fn from(_: CliContext) -> Self {
        ()
    }
}
impl From<RecoveryContext> for () {
    fn from(_: RecoveryContext) -> Self {
        ()
    }
}
impl From<RpcContext> for () {
    fn from(_: RpcContext) -> Self {
        ()
    }
}
impl From<SetupContext> for () {
    fn from(_: SetupContext) -> Self {
        ()
    }
}
