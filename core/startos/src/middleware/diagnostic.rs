use rpc_toolkit::yajrc::RpcMethod;
use rpc_toolkit::{Empty, Middleware, RpcRequest, RpcResponse};

use crate::context::DiagnosticContext;
use crate::prelude::*;

#[derive(Clone)]
pub struct DiagnosticMode {
    method: Option<String>,
}
impl DiagnosticMode {
    pub fn new() -> Self {
        Self { method: None }
    }
}

#[async_trait::async_trait]
impl Middleware<DiagnosticContext> for DiagnosticMode {
    type Metadata = Empty;
    async fn process_rpc_request(
        &mut self,
        context: &DiagnosticContext,
        metadata: Self::Metadata,
        request: &mut RpcRequest,
    ) -> Result<(), RpcResponse> {
        self.method = Some(request.method.as_str().to_owned());
        Ok(())
    }
    async fn process_rpc_response(
        &mut self,
        context: &DiagnosticContext,
        response: &mut RpcResponse,
    ) {
        if let Err(e) = &mut response.result {
            if e.code == -32601 {
                *e = Error::new(
                    eyre!(
                        "{} is not available on the Diagnostic API",
                        self.method.as_ref().map(|s| s.as_str()).unwrap_or_default()
                    ),
                    crate::ErrorKind::DiagnosticMode,
                )
                .into();
            }
        }
    }
}
