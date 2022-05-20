use std::collections::BTreeSet;
use std::time::Duration;

use patch_db::HasModel;
use serde::{Deserialize, Serialize};
use tracing::instrument;

use self::docker::DockerProcedure;
use self::js_scripts::JsProcedure;
use crate::action::ActionId;
use crate::context::RpcContext;
use crate::id::ImageId;
use crate::s9pk::manifest::PackageId;
use crate::status::health_check::HealthCheckId;
use crate::util::Version;
use crate::volume::Volumes;
use crate::Error;

pub mod docker;
pub mod js_scripts;

#[derive(Debug, Clone)]
pub enum ProcedureName {
    Main, // Usually just run container
    CreateBackup,
    RestoreBackup,
    GetConfig,
    SetConfig,
    Migration,
    Properties,
    Health(HealthCheckId),
    Action(ActionId),
}

impl ProcedureName {
    fn docker_name(&self) -> Option<String> {
        match self {
            ProcedureName::Main => None,
            ProcedureName::CreateBackup => Some("CreateBackup".to_string()),
            ProcedureName::RestoreBackup => Some("RestoreBackup".to_string()),
            ProcedureName::GetConfig => Some("GetConfig".to_string()),
            ProcedureName::SetConfig => Some("SetConfig".to_string()),
            ProcedureName::Migration => Some("Migration".to_string()),
            ProcedureName::Properties => Some(format!("Properties-{}", rand::random::<u64>())),
            ProcedureName::Health(id) => Some(format!("{}Health", id)),
            ProcedureName::Action(id) => Some(format!("{}Action", id)),
        }
    }
    fn js_function_name(&self) -> String {
        match self {
            ProcedureName::Main => todo!(),
            ProcedureName::CreateBackup => "/createBackup".to_string(),
            ProcedureName::RestoreBackup => "/restoreBackup".to_string(),
            ProcedureName::GetConfig => "/getConfig".to_string(),
            ProcedureName::SetConfig => "/setConfig".to_string(),
            ProcedureName::Migration => "/migration".to_string(),
            ProcedureName::Properties => "/properties".to_string(),
            ProcedureName::Health(id) => format!("/health/{}", id),
            ProcedureName::Action(id) => format!("/action/{}", id),
        }
    }
}

// TODO: create RPC endpoint that looks up the appropriate action and calls `execute`

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[serde(tag = "type")]
pub enum PackageProcedure {
    Docker(DockerProcedure),
    Script(JsProcedure),
}
impl PackageProcedure {
    #[instrument]
    pub fn validate(
        &self,
        volumes: &Volumes,
        image_ids: &BTreeSet<ImageId>,
        expected_io: bool,
    ) -> Result<(), color_eyre::eyre::Report> {
        match self {
            PackageProcedure::Docker(action) => action.validate(volumes, image_ids, expected_io),

            PackageProcedure::Script(action) => action.validate(volumes),
        }
    }

    #[instrument(skip(ctx, input))]
    pub async fn execute<I: Serialize, O: for<'de> Deserialize<'de>>(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        name: ProcedureName,
        volumes: &Volumes,
        input: Option<I>,
        allow_inject: bool,
        timeout: Option<Duration>,
    ) -> Result<Result<O, (i32, String)>, Error> {
        match self {
            PackageProcedure::Docker(procedure) => {
                procedure
                    .execute(
                        ctx,
                        pkg_id,
                        pkg_version,
                        name,
                        volumes,
                        input,
                        allow_inject,
                        timeout,
                    )
                    .await
            }
            PackageProcedure::Script(procedure) => {
                procedure
                    .execute(
                        &ctx.datadir,
                        pkg_id,
                        pkg_version,
                        name,
                        volumes,
                        input,
                        timeout,
                    )
                    .await
            }
        }
    }
    #[instrument(skip(ctx, input))]
    pub async fn sandboxed<I: Serialize, O: for<'de> Deserialize<'de>>(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
        input: Option<I>,
        timeout: Option<Duration>,
    ) -> Result<Result<O, (i32, String)>, Error> {
        match self {
            PackageProcedure::Docker(procedure) => {
                procedure
                    .sandboxed(ctx, pkg_id, pkg_version, volumes, input, timeout)
                    .await
            }
            PackageProcedure::Script(procedure) => {
                procedure
                    .sandboxed(ctx, pkg_id, pkg_version, volumes, input, timeout)
                    .await
            }
        }
    }
}

pub struct NoOutput;
impl<'de> Deserialize<'de> for NoOutput {
    fn deserialize<D>(_: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(NoOutput)
    }
}
