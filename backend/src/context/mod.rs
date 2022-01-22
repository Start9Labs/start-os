pub mod cli;
pub mod diagnostic;
pub mod rpc;
pub mod sdk;
pub mod setup;

pub use cli::CliContext;
pub use diagnostic::DiagnosticContext;
pub use rpc::RpcContext;
pub use sdk::SdkContext;
pub use setup::SetupContext;

impl From<CliContext> for () {
    fn from(_: CliContext) -> Self {
        ()
    }
}
impl From<DiagnosticContext> for () {
    fn from(_: DiagnosticContext) -> Self {
        ()
    }
}
impl From<RpcContext> for () {
    fn from(_: RpcContext) -> Self {
        ()
    }
}
impl From<SdkContext> for () {
    fn from(_: SdkContext) -> Self {
        ()
    }
}
impl From<SetupContext> for () {
    fn from(_: SetupContext) -> Self {
        ()
    }
}
