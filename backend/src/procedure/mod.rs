use std::collections::BTreeSet;
use std::time::Duration;

use color_eyre::eyre::eyre;
use models::ImageId;
use patch_db::HasModel;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use tracing::instrument;

use self::docker::{DockerContainers, DockerProcedure};
use crate::context::RpcContext;
use crate::s9pk::manifest::PackageId;
use crate::util::Version;
use crate::volume::Volumes;
use crate::{Error, ErrorKind};

pub mod docker;
pub use models::ProcedureName;

// TODO: create RPC endpoint that looks up the appropriate action and calls `execute`

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[serde(tag = "type")]
pub enum PackageProcedure {
    Docker(DockerProcedure),
}

impl PackageProcedure {
    pub fn is_script(&self) -> bool {
        match self {
            _ => false,
        }
    }
    #[instrument(skip_all)]
    pub fn validate(
        &self,
        container: &Option<DockerContainers>,
        eos_version: &Version,
        volumes: &Volumes,
        image_ids: &BTreeSet<ImageId>,
        expected_io: bool,
    ) -> Result<(), color_eyre::eyre::Report> {
        match self {
            PackageProcedure::Docker(action) => {
                action.validate(eos_version, volumes, image_ids, expected_io)
            }
        }
    }

    #[instrument(skip_all)]
    pub async fn execute<I: Serialize, O: DeserializeOwned + 'static>(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        name: ProcedureName,
        volumes: &Volumes,
        input: Option<I>,
        timeout: Option<Duration>,
    ) -> Result<Result<O, (i32, String)>, Error> {
        /** BLU
         * So, this needs to run a command for the already running docker and send the command via a RPC
         */
        tracing::trace!("Procedure execute {} {} - {:?}", self, pkg_id, name);
        match self {
            PackageProcedure::Docker(procedure) if procedure.inject == true => {
                procedure
                    .inject(ctx, pkg_id, pkg_version, name, volumes, input, timeout)
                    .await
            }
            PackageProcedure::Docker(procedure) => {
                todo!("BLUJ")
                // procedure.execute(ctx, name, input, timeout).await
            }
        }
    }

    #[instrument(skip_all)]
    pub async fn sandboxed<I: Serialize, O: DeserializeOwned>(
        &self,
        container: &Option<DockerContainers>,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
        input: Option<I>,
        timeout: Option<Duration>,
        name: ProcedureName,
    ) -> Result<Result<O, (i32, String)>, Error> {
        tracing::trace!("Procedure sandboxed {} {} - {:?}", self, pkg_id, name);
        match self {
            PackageProcedure::Docker(procedure) => {
                todo!("BLUJ")
                // procedure
                //     .sandboxed(ctx, pkg_id, pkg_version, volumes, input, timeout)
                //     .await
            }
        }
    }
}

impl std::fmt::Display for PackageProcedure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PackageProcedure::Docker(_) => write!(f, "Docker")?,
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct NoOutput;
impl<'de> Deserialize<'de> for NoOutput {
    fn deserialize<D>(_: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(NoOutput)
    }
}
