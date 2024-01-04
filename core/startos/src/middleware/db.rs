use axum::response::Response;
use http::HeaderValue;
use rpc_toolkit::{Middleware, RpcRequest, RpcResponse};
use serde::Deserialize;

use crate::context::RpcContext;

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
struct Metadata {
    #[serde(default)]
    sync_db: bool,
}

#[derive(Clone)]
pub struct SyncDb {
    sync_db: bool,
}
impl SyncDb {
    pub fn new() -> Self {
        SyncDb { sync_db: false }
    }
}

#[async_trait::async_trait]
impl Middleware<RpcContext> for SyncDb {
    type Metadata = Metadata;
    async fn process_rpc_request(
        &mut self,
        context: &RpcContext,
        metadata: Self::Metadata,
        request: &mut RpcRequest,
    ) -> Result<(), RpcResponse> {
        self.sync_db = metadata.sync_db;
        Ok(())
    }
    async fn process_http_response(&mut self, context: &RpcContext, response: &mut Response) {
        if self.sync_db {
            response.headers_mut().append(
                "X-Patch-Sequence",
                HeaderValue::from_str(&context.db.sequence().await.to_string())?,
            );
        }
    }
}
