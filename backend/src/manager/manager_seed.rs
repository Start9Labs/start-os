use models::ErrorKind;
use tokio_stream::StreamExt;

use crate::context::RpcContext;
use crate::s9pk::manifest::Manifest;
use crate::util::docker::stop_container;
use crate::Error;

/// This is helper structure for a service, the seed of the data that is needed for the manager_container
pub struct ManagerSeed {
    pub ctx: RpcContext,
    pub manifest: Manifest,
    pub container_name: String,
}

impl ManagerSeed {
    pub async fn stop_container(&self) -> Result<(), Error> {
        match stop_container(
            &self.container_name,
            self.manifest
                .containers
                .as_ref()
                .and_then(|c| c.main.sigterm_timeout)
                .map(|d| *d),
            None,
        )
        .await
        {
            Err(e) if e.kind == ErrorKind::NotFound => (), // Already stopped
            a => a?,
        }
        Ok(())
    }
}
