use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;
use std::str::FromStr;
use std::time::Duration;

use clap::ArgMatches;
use color_eyre::eyre::eyre;
use indexmap::IndexSet;
use patch_db::HasModel;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use tracing::instrument;

use self::docker::DockerProcedure;
use crate::config::{Config, ConfigSpec};
use crate::context::RpcContext;
use crate::id::{Id, ImageId, InvalidId};
use crate::s9pk::manifest::PackageId;
use crate::util::serde::{display_serializable, parse_stdin_deserializable, IoFormat};
use crate::util::Version;
use crate::volume::Volumes;
use crate::{Error, ResultExt};

pub mod docker;

// TODO: create RPC endpoint that looks up the appropriate action and calls `execute`

#[derive(Clone, Debug, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "kebab-case")]
#[serde(tag = "type")]
pub enum PackageProcedure {
    Docker(DockerProcedure),
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
        }
    }

    #[instrument(skip(ctx, input))]
    pub async fn execute<I: Serialize, O: for<'de> Deserialize<'de>>(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        name: Option<&str>,
        volumes: &Volumes,
        input: Option<I>,
        allow_inject: bool,
        timeout: Option<Duration>,
    ) -> Result<Result<O, (i32, String)>, Error> {
        match self {
            PackageProcedure::Docker(action) => {
                action
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
            PackageProcedure::Docker(action) => {
                action
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
