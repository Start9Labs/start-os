use models::ErrorKind;

use crate::context::RpcContext;
use crate::procedure::docker::DockerProcedure;
use crate::procedure::PackageProcedure;
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
            match &self.manifest.main {
                PackageProcedure::Docker(DockerProcedure {
                    sigterm_timeout: Some(sigterm_timeout),
                    ..
                }) => Some(**sigterm_timeout),
                _ => None,
            },
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
