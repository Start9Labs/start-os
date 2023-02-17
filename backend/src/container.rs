use std::collections::BTreeMap;
use std::net::Ipv4Addr;
use std::path::{Path, PathBuf};
use std::time::Duration;

use color_eyre::eyre::eyre;
use helpers::{NonDetachingJoinHandle, UnixRpcClient};
use models::ImageId;
use serde::{Deserialize, Serialize};
use tokio::time::timeout;
use tracing::instrument;

use crate::context::RpcContext;
use crate::docker::remove_container;
use crate::prelude::*;
use crate::s9pk::manifest::{PackageId, SYSTEM_PACKAGE_ID};
use crate::util::serde::Duration as SerdeDuration;
use crate::util::Version;
use crate::volume::{VolumeId, Volumes};
use crate::HOST_IP;

pub const NET_TLD: &str = "internal";

#[derive(Clone, Debug, Deserialize, Serialize, patch_db::HasModel, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct DockerContainers {
    pub main: DockerContainer,
    // #[serde(default)]
    // pub aux: BTreeMap<String, DockerContainer>,
}

/// This is like the docker procedures of the past designs,
/// but this time all the entrypoints and args are not
/// part of this struct by choice. Used for the times that we are creating our own entry points
#[derive(Clone, Debug, Deserialize, Serialize, patch_db::HasModel, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
pub struct DockerContainer {
    pub image: ImageId,
    #[serde(default)]
    pub mounts: BTreeMap<VolumeId, PathBuf>,
    #[serde(default)]
    pub shm_size_mb: Option<usize>, // TODO: use postfix sizing? like 1k vs 1m vs 1g
    #[serde(default)]
    pub sigterm_timeout: Option<SerdeDuration>,
    #[serde(default)]
    pub system: bool,
}

impl DockerContainer {
    pub fn container_name(pkg_id: &PackageId, name: Option<&str>) -> String {
        if let Some(name) = name {
            format!("{}_{}.{}", pkg_id, name, NET_TLD)
        } else {
            format!("{}.{}", pkg_id, NET_TLD)
        }
    }

    /// We created a new exec runner, where we are going to be passing the commands for it to run.
    /// Idea is that we are going to send it command and get the inputs be filtered back from the manager.
    /// Then we could in theory run commands without the cost of running the docker exec which is known to have
    /// a dely of > 200ms which is not acceptable.
    #[instrument(skip_all)]
    pub async fn long_running_execute(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
    ) -> Result<(LongRunning, UnixRpcClient), Error> {
        let container_name = Self::container_name(pkg_id, None);

        let socket_path =
            Path::new("/tmp/embassy/containers").join(format!("{pkg_id}_{pkg_version}"));
        if tokio::fs::metadata(&socket_path).await.is_ok() {
            tokio::fs::remove_dir_all(&socket_path).await?;
        }
        tokio::fs::create_dir_all(&socket_path).await?;

        let mut cmd = LongRunning::setup_long_running_docker_cmd(
            self,
            ctx,
            &container_name,
            volumes,
            pkg_id,
            pkg_version,
            &socket_path,
        )
        .await?;

        let mut handle = cmd.spawn().with_kind(ErrorKind::Docker)?;

        let client = UnixRpcClient::new(socket_path.join("rpc.sock"));

        let running_output = NonDetachingJoinHandle::from(tokio::spawn(async move {
            if let Err(err) = handle
                .wait()
                .await
                .map_err(|e| eyre!("Runtime error: {e:?}"))
            {
                tracing::error!("{}", err);
                tracing::debug!("{:?}", err);
            }
        }));

        {
            let socket = socket_path.join("rpc.sock");
            if let Err(_err) = timeout(Duration::from_secs(1), async move {
                while tokio::fs::metadata(&socket).await.is_err() {
                    tokio::time::sleep(Duration::from_millis(10)).await;
                }
            })
            .await
            {
                tracing::error!("Timed out waiting for init to create socket");
            }
        }

        Ok((LongRunning { running_output }, client))
    }
}

/// This is created when we wanted a long running docker executor that we could send commands to and get the responses back.
/// We wanted a long running since we want to be able to have the equivelent to the docker execute without the heavy costs of 400 + ms time lag.
/// Also the long running let's us have the ability to start/ end the services quicker.
pub struct LongRunning {
    pub running_output: NonDetachingJoinHandle<()>,
}

impl LongRunning {
    async fn setup_long_running_docker_cmd(
        docker: &DockerContainer,
        ctx: &RpcContext,
        container_name: &str,
        volumes: &Volumes,
        pkg_id: &PackageId,
        pkg_version: &Version,
        socket_path: &Path,
    ) -> Result<tokio::process::Command, Error> {
        const INIT_EXEC: &str = "/start9/bin/embassy_container_init";
        const BIND_LOCATION: &str = "/usr/lib/embassy/container/";
        tracing::trace!("setup_long_running_docker_cmd");

        remove_container(container_name).await?;

        let image_architecture = {
            let mut cmd = tokio::process::Command::new("docker");
            cmd.arg("image")
                .arg("inspect")
                .arg("--format")
                .arg("'{{.Architecture}}'");

            if docker.system {
                cmd.arg(docker.image.for_package(&*SYSTEM_PACKAGE_ID, None));
            } else {
                cmd.arg(docker.image.for_package(pkg_id, Some(pkg_version)));
            }
            let arch = String::from_utf8(cmd.output().await?.stdout)?;
            arch.replace('\'', "").trim().to_string()
        };

        let mut cmd = tokio::process::Command::new("docker");
        cmd.arg("run")
            .arg("--network=start9")
            .arg(format!("--add-host=embassy:{}", Ipv4Addr::from(HOST_IP)))
            .arg("--mount")
            .arg(format!(
                "type=bind,src={BIND_LOCATION},dst=/start9/bin/,readonly"
            ))
            .arg("--mount")
            .arg(format!(
                "type=bind,src={input},dst=/start9/sockets/",
                input = socket_path.display()
            ))
            .arg("--name")
            .arg(&container_name)
            .arg(format!("--hostname={}", &container_name))
            .arg("--entrypoint")
            .arg(format!("{INIT_EXEC}.{image_architecture}"))
            .arg("-i")
            .arg("--rm")
            .kill_on_drop(true);

        for (volume_id, dst) in &docker.mounts {
            let volume = if let Some(v) = volumes.get(volume_id) {
                v
            } else {
                continue;
            };
            let src = volume.path_for(&ctx.datadir, pkg_id, pkg_version, volume_id);
            if let Err(_e) = tokio::fs::metadata(&src).await {
                tokio::fs::create_dir_all(&src).await?;
            }
            cmd.arg("--mount").arg(format!(
                "type=bind,src={},dst={}{}",
                src.display(),
                dst.display(),
                if volume.readonly() { ",readonly" } else { "" }
            ));
        }
        if let Some(shm_size_mb) = docker.shm_size_mb {
            cmd.arg("--shm-size").arg(format!("{}m", shm_size_mb));
        }
        cmd.arg("--log-driver=journald");
        if docker.system {
            cmd.arg(docker.image.for_package(&*SYSTEM_PACKAGE_ID, None));
        } else {
            cmd.arg(docker.image.for_package(pkg_id, Some(pkg_version)));
        }
        cmd.stdout(std::process::Stdio::piped());
        cmd.stderr(std::process::Stdio::inherit());
        cmd.stdin(std::process::Stdio::piped());
        Ok(cmd)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    /// Note, this size doesn't mean the vec will match. The vec will go to the next size, 0 -> 7 = 7 and so forth 7-15 = 15
    /// Just how the vec with capacity works.
    const CAPACITY_IN: usize = 7;
    #[test]
    fn default_capacity_is_set() {
        let ring: RingVec<usize> = RingVec::new(CAPACITY_IN);
        assert_eq!(CAPACITY_IN, ring.value.capacity());
        assert_eq!(0, ring.value.len());
    }
    #[test]
    fn capacity_can_not_be_exceeded() {
        let mut ring = RingVec::new(CAPACITY_IN);
        for i in 1..100usize {
            ring.push(i);
        }
        assert_eq!(CAPACITY_IN, ring.value.capacity());
        assert_eq!(CAPACITY_IN, ring.value.len());
    }
}
