use bollard::container::StopContainerOptions;
use reqwest::Url;

use crate::context::RpcContext;
use crate::prelude::*;
use crate::s9pk::manifest::Manifest;

pub struct ManagerSeed {
    pub ctx: RpcContext,
    pub manifest: Manifest,
    pub container_name: String,
    pub marketplace_url: Option<Url>,
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
        Ok(())
    }
}
