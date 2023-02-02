use color_eyre::eyre::eyre;
use models::Error;
use models::PackageId;
use serde_json::Value;

pub struct RuntimeDropped;

pub type Callback = Box<dyn Fn(Value) -> Result<(), RuntimeDropped> + Send + Sync + 'static>; // bool indicating if

fn method_not_available() -> Error {
    Error::new(
        eyre!("method not available"),
        models::ErrorKind::InvalidRequest,
    )
}

#[async_trait::async_trait]
#[allow(unused_variables)]
pub trait OsApi: Send + Sync + 'static {
    async fn get_service_config(
        &self,
        id: PackageId,
        path: &str,
        callback: Callback,
    ) -> Result<Value, Error> {
        Err(method_not_available())
    }
}
