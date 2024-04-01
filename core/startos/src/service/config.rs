use models::ProcedureName;

use crate::config::{action::SetResult, ConfigureContext};
use crate::prelude::*;
use crate::service::Service;

impl Service {
    pub async fn configure(
        &self,
        ConfigureContext { timeout, config }: ConfigureContext,
    ) -> Result<(), Error> {
        let container = &self.seed.persistent_container;
        let package_id = &self.seed.id;

        container
            .execute::<SetResult>(ProcedureName::SetConfig, to_value(&config)?, timeout)
            .await
            .with_kind(ErrorKind::Action)?;

        self.seed
            .ctx
            .db
            .mutate(move |db| {
                db.as_public_mut()
                    .as_package_data_mut()
                    .as_idx_mut(package_id)
                    .or_not_found(package_id)?
                    .as_status_mut()
                    .as_configured_mut()
                    .ser(&true)
            })
            .await?;

        Ok(())
    }
}
