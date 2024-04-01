use models::ProcedureName;

use crate::config::ConfigureContext;
use crate::prelude::*;
use crate::service::Service;

impl Service {
    pub async fn configure(
        &self,
        ConfigureContext { timeout, config }: ConfigureContext,
    ) -> Result<(), Error> {
        let container = &self.seed.persistent_container;
        container
            .execute::<Value>(ProcedureName::SetConfig, to_value(&config)?, timeout)
            .await
            .with_kind(ErrorKind::Action)?;
        Ok(())
    }
}
