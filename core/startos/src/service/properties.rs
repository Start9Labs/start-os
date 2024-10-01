use std::time::Duration;

use models::ProcedureName;

use crate::prelude::*;
use crate::rpc_continuations::Guid;
use crate::service::Service;

impl Service {
    // TODO: leave here or switch to Actor Message?
    pub async fn properties(&self) -> Result<Value, Error> {
        let container = &self.seed.persistent_container;
        container
            .execute::<Value>(
                Guid::new(),
                ProcedureName::Properties,
                Value::Null,
                Some(Duration::from_secs(30)),
            )
            .await
            .with_kind(ErrorKind::Unknown)
    }
}
