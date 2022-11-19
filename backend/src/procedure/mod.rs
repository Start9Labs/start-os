use std::collections::BTreeSet;
use std::time::Duration;

use color_eyre::eyre::eyre;
use patch_db::HasModel;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use tracing::instrument;

use self::docker::{DockerContainers, DockerProcedure};
use crate::context::RpcContext;
use crate::id::ImageId;
use crate::s9pk::manifest::PackageId;
use crate::util::Version;
use crate::volume::Volumes;
use crate::{Error, ErrorKind};

pub mod docker;
#[cfg(feature = "js_engine")]
pub mod js_scripts;
pub use models::ProcedureName;

// TODO: create RPC endpoint that looks up the appropriate action and calls `execute`

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[serde(tag = "type")]
pub enum PackageProcedure {
    Docker(DockerProcedure),

    #[cfg(feature = "js_engine")]
    Script(js_scripts::JsProcedure),
}

impl PackageProcedure {
    pub fn is_script(&self) -> bool {
        match self {
            #[cfg(feature = "js_engine")]
            Self::Script(_) => true,
            _ => false,
        }
    }
    #[instrument]
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
            #[cfg(feature = "js_engine")]
            PackageProcedure::Script(action) => action.validate(volumes),
        }
    }

    #[instrument(skip(ctx, input))]
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
        tracing::trace!("Procedure execute {} {} - {:?}", self, pkg_id, name);
        match self {
            PackageProcedure::Docker(procedure) if procedure.inject == true => {
                procedure
                    .inject(ctx, pkg_id, pkg_version, name, volumes, input, timeout)
                    .await
            }
            PackageProcedure::Docker(procedure) => {
                procedure
                    .execute(ctx, pkg_id, pkg_version, name, volumes, input, timeout)
                    .await
            }
            #[cfg(feature = "js_engine")]
            PackageProcedure::Script(procedure) => {
                let (gid, rpc_client) = match ctx
                    .managers
                    .get(&(pkg_id.clone(), pkg_version.clone()))
                    .await
                {
                    None => {
                        return Err(Error::new(
                            eyre!("No manager found for {}", pkg_id),
                            ErrorKind::NotFound,
                        ))
                    }
                    Some(man) => (
                        if matches!(name, ProcedureName::Main) {
                            man.new_main_gid()
                        } else {
                            man.new_gid()
                        },
                        man.rpc_client(),
                    ),
                };

                procedure
                    .execute(
                        &ctx.datadir,
                        pkg_id,
                        pkg_version,
                        name,
                        volumes,
                        input,
                        timeout,
                        gid,
                        rpc_client,
                    )
                    .await
            }
        }
    }

    #[instrument(skip(ctx, input))]
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
                procedure
                    .sandboxed(ctx, pkg_id, pkg_version, volumes, input, timeout)
                    .await
            }
            #[cfg(feature = "js_engine")]
            PackageProcedure::Script(procedure) => {
                procedure
                    .sandboxed(ctx, pkg_id, pkg_version, volumes, input, timeout, name)
                    .await
            }
        }
    }
}

impl std::fmt::Display for PackageProcedure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PackageProcedure::Docker(_) => write!(f, "Docker")?,
            #[cfg(feature = "js_engine")]
            PackageProcedure::Script(_) => write!(f, "JS")?,
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
