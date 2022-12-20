use std::sync::atomic::AtomicUsize;

use reqwest::Url;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use yajrc::{Id, RpcError, RpcMethod, RpcRequest};

pub struct RpcClient {
    id: AtomicUsize,
    url: Url,
}
impl RpcClient {
    pub fn new(url: impl Into<Url>) -> Self {
        RpcClient {
            id: AtomicUsize::new(0),
            url: url.into(),
        }
    }

    pub async fn request<T: RpcMethod>(
        &self,
        method: T,
        params: T::Params,
    ) -> Result<T::Response, RpcError>
    where
        T: Serialize + Clone,
        T::Params: Serialize + Clone,
        T::Response: for<'de> Deserialize<'de>,
    {
        let id = Id::Number(
            self.id
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst)
                .into(),
        );
        let client = reqwest::Client::new();
        let response = client
            .post(self.url.clone())
            .json(&RpcRequest {
                id: Some(id.clone()),
                method,
                params,
            })
            .send()
            .await
            .map_err(|e| {
                let mut err = yajrc::INTERNAL_ERROR.clone();
                err.data = Some(json!(e.to_string()));
                err
            })?;
        if response.status().is_client_error() {
            let mut err = yajrc::INTERNAL_ERROR.clone();
            err.data = Some(json!(format!(
                "Client Error: {}",
                response.text().await.unwrap_or_default()
            )));
            return Err(err);
        }
        if response.status().is_server_error() {
            let mut err = yajrc::INTERNAL_ERROR.clone();
            err.data = Some(json!(format!(
                "Server Error: {}",
                response.text().await.unwrap_or_default()
            )));
            return Err(err);
        }
        if !response.status().is_success() {
            let mut err = yajrc::INTERNAL_ERROR.clone();
            err.data = Some(json!(format!(
                "Unknown RpcError: {}",
                response.text().await.unwrap_or_default()
            )));
            return Err(err);
        }
        let response: Value = response.json().await?;
        if let Some(error) = response["error"].as_str() {
            let mut err = yajrc::INTERNAL_ERROR.clone();
            err.data = Some(json!(format!("Server Error: {error}")));
            return Err(err);
        }

        Ok(serde_json::from_value(response["result"].to_owned())?)
    }
}
