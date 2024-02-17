use std::collections::BTreeMap;

use models::PackageId;

use crate::config::ConfigureContext;
use crate::prelude::*;
use crate::service::Service;

impl Service {
    pub async fn configure(
        &self,
        ConfigureContext {
            breakages,
            timeout,
            config,
            overrides,
            dry_run,
        }: ConfigureContext,
    ) -> Result<BTreeMap<PackageId, String>, Error> {
        todo!()
    }
}
