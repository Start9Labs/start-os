use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::ffi::{OsStr, OsString};
use std::net::Ipv4Addr;
use std::path::PathBuf;
use std::time::Duration;

use bollard::container::RemoveContainerOptions;
use futures::future::Either as EitherFuture;
use nix::sys::signal;
use nix::unistd::Pid;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tracing::instrument;

use crate::context::RpcContext;
use crate::id::{Id, ImageId};
use crate::s9pk::manifest::{PackageId, SYSTEM_PACKAGE_ID};
use crate::util::serde::{Duration as SerdeDuration, IoFormat};
use crate::util::Version;
use crate::volume::{VolumeId, Volumes};
use crate::{Error, ResultExt, HOST_IP};

pub const NET_TLD: &str = "embassy";

lazy_static::lazy_static! {
    pub static ref SYSTEM_IMAGES: BTreeSet<ImageId> = {
        let mut set = BTreeSet::new();

        set.insert("compat".parse().unwrap());
        set.insert("utils".parse().unwrap());

        set
    };
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct DockerAction {
    pub image: ImageId,
    #[serde(default)]
    pub system: bool,
    pub entrypoint: String,
    #[serde(default)]
    pub args: Vec<String>,
    #[serde(default)]
    pub mounts: BTreeMap<VolumeId, PathBuf>,
    #[serde(default)]
    pub io_format: Option<IoFormat>,
    #[serde(default)]
    pub inject: bool,
    #[serde(default)]
    pub shm_size_mb: Option<usize>, // TODO: use postfix sizing? like 1k vs 1m vs 1g
    #[serde(default)]
    pub sigterm_timeout: Option<SerdeDuration>,
}
impl DockerAction {
    pub fn validate(
        &self,
        volumes: &Volumes,
        image_ids: &BTreeSet<ImageId>,
        expected_io: bool,
    ) -> Result<(), color_eyre::eyre::Report> {
        for (volume, _) in &self.mounts {
            if !volumes.contains_key(volume) && !matches!(&volume, &VolumeId::Backup) {
                color_eyre::eyre::bail!("unknown volume: {}", volume);
            }
        }
        if self.system {
            if !SYSTEM_IMAGES.contains(&self.image) {
                color_eyre::eyre::bail!("unknown system image: {}", self.image);
            }
        } else {
            if !image_ids.contains(&self.image) {
                color_eyre::eyre::bail!("image for {} not contained in package", self.image);
            }
        }
        if expected_io && self.io_format.is_none() {
            color_eyre::eyre::bail!("expected io-format");
        }
        Ok(())
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
        let mut cmd = tokio::process::Command::new("docker");
        if self.inject && allow_inject {
            cmd.arg("exec");
        } else {
            let container_name = Self::container_name(pkg_id, name);
            cmd.arg("run")
                .arg("--rm")
                .arg("--network=start9")
                .arg(format!("--add-host=embassy:{}", Ipv4Addr::from(HOST_IP)))
                .arg("--name")
                .arg(&container_name)
                .arg(format!("--hostname={}", &container_name))
                .arg("--no-healthcheck");
            match ctx
                .docker
                .remove_container(
                    &container_name,
                    Some(RemoveContainerOptions {
                        v: false,
                        force: true,
                        link: false,
                    }),
                )
                .await
            {
                Ok(()) | Err(bollard::errors::Error::DockerResponseNotFoundError { .. }) => Ok(()),
                Err(e) => Err(e),
            }?;
        }
        cmd.args(
            self.docker_args(ctx, pkg_id, pkg_version, volumes, allow_inject)
                .await,
        );
        let input_buf = if let (Some(input), Some(format)) = (&input, &self.io_format) {
            cmd.stdin(std::process::Stdio::piped());
            Some(format.to_vec(input)?)
        } else {
            None
        };
        cmd.stdout(std::process::Stdio::piped());
        cmd.stderr(std::process::Stdio::piped());
        tracing::trace!(
            "{}",
            format!("{:?}", cmd)
                .split(r#"" ""#)
                .collect::<Vec<&str>>()
                .join(" ")
        );
        let mut handle = cmd.spawn().with_kind(crate::ErrorKind::Docker)?;
        let id = handle.id();
        let timeout_fut = if let Some(timeout) = timeout {
            EitherFuture::Right(async move {
                tokio::time::sleep(timeout).await;

                Ok(())
            })
        } else {
            EitherFuture::Left(futures::future::pending::<Result<_, Error>>())
        };
        if let (Some(input), Some(stdin)) = (&input_buf, &mut handle.stdin) {
            use tokio::io::AsyncWriteExt;
            stdin
                .write_all(input)
                .await
                .with_kind(crate::ErrorKind::Docker)?;
            stdin.flush().await?;
            stdin.shutdown().await?;
            drop(stdin);
        }
        enum Race<T> {
            Done(T),
            TimedOut,
        }
        let res = tokio::select! {
            res = handle.wait_with_output() => Race::Done(res.with_kind(crate::ErrorKind::Docker)?),
            res = timeout_fut => {
                res?;
                Race::TimedOut
            },
        };
        let res = match res {
            Race::Done(x) => x,
            Race::TimedOut => {
                if let Some(id) = id {
                    signal::kill(Pid::from_raw(id as i32), signal::SIGKILL)
                        .with_kind(crate::ErrorKind::Docker)?;
                }
                return Ok(Err((143, "Timed out. Retrying soon...".to_owned())));
            }
        };
        Ok(if res.status.success() || res.status.code() == Some(143) {
            Ok(if let Some(format) = self.io_format {
                match format.from_slice(&res.stdout) {
                    Ok(a) => a,
                    Err(e) => {
                        tracing::warn!(
                            "Failed to deserialize stdout from {}: {}, falling back to UTF-8 string.",
                            format,
                            e
                        );
                        serde_json::from_value(String::from_utf8(res.stdout)?.into())
                            .with_kind(crate::ErrorKind::Deserialization)?
                    }
                }
            } else if res.stdout.is_empty() {
                serde_json::from_value(Value::Null).with_kind(crate::ErrorKind::Deserialization)?
            } else {
                serde_json::from_value(String::from_utf8(res.stdout)?.into())
                    .with_kind(crate::ErrorKind::Deserialization)?
            })
        } else {
            Err((
                res.status.code().unwrap_or_default(),
                String::from_utf8(res.stderr)?,
            ))
        })
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
        let mut cmd = tokio::process::Command::new("docker");
        cmd.arg("run").arg("--rm").arg("--network=none");
        cmd.args(
            self.docker_args(ctx, pkg_id, pkg_version, &volumes.to_readonly(), false)
                .await,
        );
        let input_buf = if let (Some(input), Some(format)) = (&input, &self.io_format) {
            cmd.stdin(std::process::Stdio::piped());
            Some(format.to_vec(input)?)
        } else {
            None
        };
        cmd.stdout(std::process::Stdio::piped());
        cmd.stderr(std::process::Stdio::piped());
        let mut handle = cmd.spawn().with_kind(crate::ErrorKind::Docker)?;
        if let (Some(input), Some(stdin)) = (&input_buf, &mut handle.stdin) {
            use tokio::io::AsyncWriteExt;
            stdin
                .write_all(input)
                .await
                .with_kind(crate::ErrorKind::Docker)?;
        }
        let res = handle
            .wait_with_output()
            .await
            .with_kind(crate::ErrorKind::Docker)?;
        Ok(if res.status.success() || res.status.code() == Some(143) {
            Ok(if let Some(format) = &self.io_format {
                match format.from_slice(&res.stdout) {
                    Ok(a) => a,
                    Err(e) => {
                        tracing::warn!(
                            "Failed to deserialize stdout from {}: {}, falling back to UTF-8 string.",
                            format,
                            e
                        );
                        serde_json::from_value(String::from_utf8(res.stdout)?.into())
                            .with_kind(crate::ErrorKind::Deserialization)?
                    }
                }
            } else if res.stdout.is_empty() {
                serde_json::from_value(Value::Null).with_kind(crate::ErrorKind::Deserialization)?
            } else {
                serde_json::from_value(String::from_utf8(res.stdout)?.into())
                    .with_kind(crate::ErrorKind::Deserialization)?
            })
        } else {
            Err((
                res.status.code().unwrap_or_default(),
                String::from_utf8(res.stderr)?,
            ))
        })
    }

    pub fn container_name(pkg_id: &PackageId, name: Option<&str>) -> String {
        if let Some(name) = name {
            format!("{}_{}.{}", pkg_id, name, NET_TLD)
        } else {
            format!("{}.{}", pkg_id, NET_TLD)
        }
    }

    pub fn uncontainer_name(name: &str) -> Option<(PackageId<&str>, Option<&str>)> {
        let (pre_tld, _) = name.split_once(".")?;
        if pre_tld.contains('_') {
            let (pkg, name) = name.split_once("_")?;
            Some((Id::try_from(pkg).ok()?.into(), Some(name)))
        } else {
            Some((Id::try_from(pre_tld).ok()?.into(), None))
        }
    }

    async fn docker_args(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        volumes: &Volumes,
        allow_inject: bool,
    ) -> Vec<Cow<'_, OsStr>> {
        let mut res = Vec::with_capacity(
            (2 * self.mounts.len()) // --mount <MOUNT_ARG>
                + (2 * self.shm_size_mb.is_some() as usize) // --shm-size <SHM_SIZE>
                + 5 // --interactive --log-driver=journald --entrypoint <ENTRYPOINT> <IMAGE>
                + self.args.len(), // [ARG...]
        );
        for (volume_id, dst) in &self.mounts {
            let volume = if let Some(v) = volumes.get(volume_id) {
                v
            } else {
                continue;
            };
            let src = volume.path_for(ctx, pkg_id, pkg_version, volume_id);
            if let Err(e) = tokio::fs::metadata(&src).await {
                tracing::warn!("{} not mounted to container: {}", src.display(), e);
                continue;
            }
            res.push(OsStr::new("--mount").into());
            res.push(
                OsString::from(format!(
                    "type=bind,src={},dst={}{}",
                    src.display(),
                    dst.display(),
                    if volume.readonly() { ",readonly" } else { "" }
                ))
                .into(),
            );
        }
        if let Some(shm_size_mb) = self.shm_size_mb {
            res.push(OsStr::new("--shm-size").into());
            res.push(OsString::from(format!("{}m", shm_size_mb)).into());
        }
        res.push(OsStr::new("--interactive").into());
        if self.inject && allow_inject {
            res.push(OsString::from(Self::container_name(pkg_id, None)).into());
            res.push(OsStr::new(&self.entrypoint).into());
        } else {
            res.push(OsStr::new("--log-driver=journald").into());
            res.push(OsStr::new("--entrypoint").into());
            res.push(OsStr::new(&self.entrypoint).into());
            if self.system {
                res.push(OsString::from(self.image.for_package(SYSTEM_PACKAGE_ID, None)).into());
            } else {
                res.push(OsString::from(self.image.for_package(pkg_id, Some(pkg_version))).into());
            }
        }
        res.extend(self.args.iter().map(|s| OsStr::new(s).into()));

        res
    }
}
