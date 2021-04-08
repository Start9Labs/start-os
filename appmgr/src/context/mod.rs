use rpc_toolkit::reqwest::Client;
use rpc_toolkit::url::{Host, Url};
use rpc_toolkit::Context;

mod cli;
mod rpc;

pub use cli::CliContext;
pub use rpc::RpcContext;

#[derive(Debug, Clone)]
pub struct ExtendedContext<T, U> {
    base: T,
    extension: U,
}
impl<T, U> ExtendedContext<T, U> {
    pub fn map<F: FnOnce(U) -> V, V>(self, f: F) -> ExtendedContext<T, V> {
        ExtendedContext {
            base: self.base,
            extension: f(self.extension),
        }
    }
    pub fn split(self) -> (T, U) {
        (self.base, self.extension)
    }
    pub fn base(&self) -> &T {
        &self.base
    }
    pub fn extension(&self) -> &U {
        &self.extension
    }
}
impl<T> From<T> for ExtendedContext<T, ()> {
    fn from(base: T) -> Self {
        ExtendedContext {
            base,
            extension: (),
        }
    }
}
impl<T: Context, U> Context for ExtendedContext<T, U> {
    fn host(&self) -> Host<&str> {
        self.base.host()
    }
    fn port(&self) -> u16 {
        self.base.port()
    }
    fn protocol(&self) -> &str {
        self.base.protocol()
    }
    fn url(&self) -> Url {
        self.base.url()
    }
    fn client(&self) -> &Client {
        self.base.client()
    }
}

#[derive(Clone)]
pub enum EitherContext {
    Cli(CliContext),
    Rpc(RpcContext),
}
impl EitherContext {
    pub fn as_cli(&self) -> Option<&CliContext> {
        match self {
            EitherContext::Cli(a) => Some(a),
            _ => None,
        }
    }
    pub fn as_rpc(&self) -> Option<&RpcContext> {
        match self {
            EitherContext::Rpc(a) => Some(a),
            _ => None,
        }
    }
}
impl Context for EitherContext {
    fn host(&self) -> Host<&str> {
        match self {
            EitherContext::Cli(a) => a.host(),
            EitherContext::Rpc(b) => b.host(),
        }
    }
    fn port(&self) -> u16 {
        match self {
            EitherContext::Cli(a) => a.port(),
            EitherContext::Rpc(b) => b.port(),
        }
    }
    fn protocol(&self) -> &str {
        match self {
            EitherContext::Cli(a) => a.protocol(),
            EitherContext::Rpc(b) => b.protocol(),
        }
    }
    fn url(&self) -> Url {
        match self {
            EitherContext::Cli(a) => a.url(),
            EitherContext::Rpc(b) => b.url(),
        }
    }
    fn client(&self) -> &Client {
        match self {
            EitherContext::Cli(a) => a.client(),
            EitherContext::Rpc(b) => b.client(),
        }
    }
}
