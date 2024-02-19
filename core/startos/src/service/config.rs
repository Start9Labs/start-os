use std::collections::BTreeMap;

use models::{ActionId, PackageId, ProcedureName};

use crate::config::ConfigureContext;
use crate::prelude::*;
use crate::service::Service;

impl Service {
    pub async fn configure(
        &self,
        ConfigureContext { timeout, config }: ConfigureContext,
    ) -> Result<BTreeMap<PackageId, String>, Error> {
        let container = &self.seed.persistent_container;
        container
            .execute::<BTreeMap<PackageId, String>>(
                ProcedureName::SetConfig,
                to_value(&config)?,
                timeout,
            )
            .await
            .with_kind(ErrorKind::Action)
    }
}
