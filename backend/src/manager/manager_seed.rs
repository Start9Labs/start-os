use bollard::container::{StopContainerOptions, WaitContainerOptions};
use tokio_stream::StreamExt;

use crate::context::RpcContext;
use crate::s9pk::manifest::Manifest;
use crate::Error;

/// This is helper structure for a service, the seed of the data that is needed for the manager_container
pub struct ManagerSeed {
    pub ctx: RpcContext,
    pub manifest: Manifest,
    pub container_name: String,
}

impl ManagerSeed {
    pub async fn stop_container(&self) -> Result<(), Error> {
        match self
            .ctx
            .docker
            .stop_container(
                &self.container_name,
                Some(StopContainerOptions {
                    t: self
                        .manifest
                        .containers
                        .as_ref()
                        .and_then(|c| c.main.sigterm_timeout)
                        .map(|d| d.as_secs())
                        .unwrap_or(30) as i64,
                }),
            )
            .await
        {
            Err(bollard::errors::Error::DockerResponseServerError {
                status_code: 404, // NOT FOUND
                ..
            })
            | Err(bollard::errors::Error::DockerResponseServerError {
                status_code: 409, // CONFLICT
                ..
            })
            | Err(bollard::errors::Error::DockerResponseServerError {
                status_code: 304, // NOT MODIFIED
                ..
            }) => (), // Already stopped
            a => a?,
        }

        // Wait for the container to stop
        {
            let mut waiting = self.ctx.docker.wait_container(
                &self.container_name,
                Some(WaitContainerOptions {
                    condition: "not-running",
                }),
            );
            while let Some(_) = waiting.next().await {
                tokio::time::sleep(std::time::Duration::from_millis(100)).await;
            }
        }
        Ok(())
    }
}
