use models::Error;
use models::PackageId;
use serde_json::Value;

pub struct RuntimeDropped;

pub type Callback = Box<dyn Fn(Value) -> Result<(), RuntimeDropped> + Send + Sync + 'static>; // bool indicating if

#[async_trait::async_trait]
pub trait OsApi: Send + Sync + 'static {
    async fn get_service_config(
        &self,
        id: PackageId,
        path: &str,
        callback: Callback,
    ) -> Result<Value, Error>;
}
