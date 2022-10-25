use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::ffi::{OsStr, OsString};
use std::net::Ipv4Addr;
use std::path::PathBuf;
use std::time::Duration;

use async_stream::stream;
use bollard::container::RemoveContainerOptions;
use color_eyre::eyre::eyre;
use color_eyre::Report;
use embassy_container_init::{InputJsonRpc, OutputJsonRpc};
use futures::future::Either as EitherFuture;
use futures::{Stream, StreamExt, TryFutureExt, TryStreamExt};
use helpers::NonDetachingJoinHandle;
use nix::sys::signal;
use nix::unistd::Pid;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use serde_json::Value;
use tokio::{
    io::{AsyncBufRead, AsyncBufReadExt, BufReader},
    process::Child,
    sync::mpsc::UnboundedReceiver,
};
use tracing::instrument;

use super::ProcedureName;
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

#[derive(Clone, Debug, Deserialize, Serialize, patch_db::HasModel)]
#[serde(rename_all = "kebab-case")]
pub struct DockerContainers {
    pub main: DockerContainer,
    // #[serde(default)]
    // pub aux: BTreeMap<String, DockerContainer>,
}

/// This is like the docker procedures of the past designs,
/// but this time all the entrypoints and args are not
/// part of this struct by choice. Used for the times that we are creating our own entry points
#[derive(Clone, Debug, Deserialize, Serialize, patch_db::HasModel)]
#[serde(rename_all = "kebab-case")]
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

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct DockerProcedure {
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
    pub sigterm_timeout: Option<SerdeDuration>,
    #[serde(default)]
    pub shm_size_mb: Option<usize>, // TODO: use postfix sizing? like 1k vs 1m vs 1g
}

#[derive(Clone, Debug, Deserialize, Serialize, Default)]
#[serde(rename_all = "kebab-case")]
pub struct DockerInject {
    #[serde(default)]
    pub system: bool,
    pub entrypoint: String,
    #[serde(default)]
    pub args: Vec<String>,
    #[serde(default)]
    pub io_format: Option<IoFormat>,
    #[serde(default)]
    pub sigterm_timeout: Option<SerdeDuration>,
}
impl DockerProcedure {
    pub fn main_docker_procedure(
        container: &DockerContainer,
        injectable: &DockerInject,
    ) -> DockerProcedure {
        DockerProcedure {
            image: container.image.clone(),
            system: injectable.system,
            entrypoint: injectable.entrypoint.clone(),
            args: injectable.args.clone(),
            mounts: container.mounts.clone(),
            io_format: injectable.io_format,
            sigterm_timeout: injectable.sigterm_timeout,
            shm_size_mb: container.shm_size_mb,
        }
    }
    #[cfg(feature = "js_engine")]
    pub fn main_docker_procedure_js(
        container: &DockerContainer,
        _procedure: &super::js_scripts::JsProcedure,
    ) -> DockerProcedure {
        DockerProcedure {
            image: container.image.clone(),
            system: container.system,
            entrypoint: "sleep".to_string(),
            args: Vec::new(),
            mounts: container.mounts.clone(),
            io_format: None,
            sigterm_timeout: container.sigterm_timeout,
            shm_size_mb: container.shm_size_mb,
        }
    }

    pub fn validate(
        &self,
        _eos_version: &Version,
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
        } else if !image_ids.contains(&self.image) {
            color_eyre::eyre::bail!("image for {} not contained in package", self.image);
        }
        if expected_io && self.io_format.is_none() {
            color_eyre::eyre::bail!("expected io-format");
        }
        Ok(())
    }

    #[instrument(skip(ctx, input))]
    pub async fn execute<I: Serialize, O: DeserializeOwned>(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        name: ProcedureName,
        volumes: &Volumes,
        input: Option<I>,
        timeout: Option<Duration>,
    ) -> Result<Result<O, (i32, String)>, Error> {
        let name = name.docker_name();
        let name: Option<&str> = name.as_ref().map(|x| &**x);
        let mut cmd = tokio::process::Command::new("docker");
        tracing::debug!("{:?} is run", name);
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
            Ok(())
            | Err(bollard::errors::Error::DockerResponseServerError {
                status_code: 404, // NOT FOUND
                ..
            }) => Ok(()),
            Err(e) => Err(e),
        }?;
        cmd.args(self.docker_args(ctx, pkg_id, pkg_version, volumes).await?);
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
        if let (Some(input), Some(mut stdin)) = (&input_buf, handle.stdin.take()) {
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

        let io_format = self.io_format;
        let mut output = BufReader::new(
            handle
                .stdout
                .take()
                .ok_or_else(|| eyre!("Can't takeout stdout in execute"))
                .with_kind(crate::ErrorKind::Docker)?,
        );
        let output = NonDetachingJoinHandle::from(tokio::spawn(async move {
            match async {
                if let Some(format) = io_format {
                    return match max_by_lines(&mut output, None).await {
                        MaxByLines::Done(buffer) => {
                            Ok::<Value, Error>(
                                match format.from_slice(buffer.as_bytes()) {
                                    Ok(a) => a,
                                    Err(e) => {
                                        tracing::trace!(
                                        "Failed to deserialize stdout from {}: {}, falling back to UTF-8 string.",
                                        format,
                                        e
                                    );
                                        Value::String(buffer)
                                    }
                                },
                            )
                        },
                        MaxByLines::Error(e) => Err(e),
                        MaxByLines::Overflow(buffer) => Ok(Value::String(buffer))
                    }
                }

                let lines = buf_reader_to_lines(&mut output, 1000).await?;
                if lines.is_empty() {
                    return Ok(Value::Null);
                }

                let joined_output = lines.join("\n");
                Ok(Value::String(joined_output))
            }.await {
                Ok(a) => Ok((a, output)),
                Err(e) => Err((e, output))
            }
        }));
        let err_output = BufReader::new(
            handle
                .stderr
                .take()
                .ok_or_else(|| eyre!("Can't takeout std err"))
                .with_kind(crate::ErrorKind::Docker)?,
        );

        let err_output = NonDetachingJoinHandle::from(tokio::spawn(async move {
            let lines = buf_reader_to_lines(err_output, 1000).await?;
            let joined_output = lines.join("\n");
            Ok::<_, Error>(joined_output)
        }));

        let res = tokio::select! {
            res = handle.wait() => Race::Done(res.with_kind(crate::ErrorKind::Docker)?),
            res = timeout_fut => {
                res?;
                Race::TimedOut
            },
        };
        let exit_status = match res {
            Race::Done(x) => x,
            Race::TimedOut => {
                if let Some(id) = id {
                    signal::kill(Pid::from_raw(id as i32), signal::SIGKILL)
                        .with_kind(crate::ErrorKind::Docker)?;
                }
                return Ok(Err((143, "Timed out. Retrying soon...".to_owned())));
            }
        };
        Ok(
            if exit_status.success() || exit_status.code() == Some(143) {
                Ok(serde_json::from_value(
                    output
                        .await
                        .with_kind(crate::ErrorKind::Unknown)?
                        .map(|(v, _)| v)
                        .map_err(|(e, _)| tracing::warn!("{}", e))
                        .unwrap_or_default(),
                )
                .with_kind(crate::ErrorKind::Deserialization)?)
            } else {
                Err((
                    exit_status.code().unwrap_or_default(),
                    err_output.await.with_kind(crate::ErrorKind::Unknown)??,
                ))
            },
        )
    }

    /// We created a new exec runner, where we are going to be passing the commands for it to run.
    /// Idea is that we are going to send it command and get the inputs be filtered back from the manager.
    /// Then we could in theory run commands without the cost of running the docker exec which is known to have
    /// a dely of > 200ms which is not acceptable.
    #[instrument(skip(ctx, input))]
    pub async fn long_running_execute<S>(
        &self,
        ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        name: ProcedureName,
        volumes: &Volumes,
        input: S,
    ) -> Result<LongRunning, Error>
    where
        S: Stream<Item = InputJsonRpc> + Send + 'static,
    {
        let name = name.docker_name();
        let name: Option<&str> = name.as_deref();
        let container_name = Self::container_name(pkg_id, name);

        let mut cmd = LongRunning::setup_long_running_docker_cmd(
            self,
            ctx,
            &container_name,
            volumes,
            pkg_id,
            pkg_version,
        )
        .await?;

        let mut handle = cmd.spawn().with_kind(crate::ErrorKind::Docker)?;
        let input_handle = LongRunning::spawn_input_handle(&mut handle, input)?
            .map_err(|e| eyre!("Input Handle Error: {e:?}"));

        let (output, output_handle) = LongRunning::spawn_output_handle(&mut handle)?;
        let output_handle = output_handle.map_err(|e| eyre!("Output Handle Error: {e:?}"));
        let err_handle = LongRunning::spawn_error_handle(&mut handle)?
            .map_err(|e| eyre!("Err Handle Error: {e:?}"));

        let running_output = NonDetachingJoinHandle::from(tokio::spawn(async move {
            if let Err(err) = tokio::select!(
                x = handle.wait().map_err(|e| eyre!("Runtime error: {e:?}")) => x.map(|_| ()),
                x = err_handle => x.map(|_| ()),
                x = output_handle => x.map(|_| ()),
                x = input_handle => x.map(|_| ())
            ) {
                tracing::debug!("{:?}", err);
                tracing::error!("Join error");
            }
        }));

        Ok(LongRunning {
            output,
            running_output,
        })
    }

    #[instrument(skip(_ctx, input))]
    pub async fn inject<I: Serialize, O: DeserializeOwned>(
        &self,
        _ctx: &RpcContext,
        pkg_id: &PackageId,
        pkg_version: &Version,
        name: ProcedureName,
        volumes: &Volumes,
        input: Option<I>,
        timeout: Option<Duration>,
    ) -> Result<Result<O, (i32, String)>, Error> {
        let name = name.docker_name();
        let name: Option<&str> = name.as_deref();
        let mut cmd = tokio::process::Command::new("docker");

        tracing::debug!("{:?} is exec", name);
        cmd.arg("exec");

        cmd.args(self.docker_args_inject(pkg_id).await?);
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
        if let (Some(input), Some(mut stdin)) = (&input_buf, handle.stdin.take()) {
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

        let io_format = self.io_format;
        let mut output = BufReader::new(
            handle
                .stdout
                .take()
                .ok_or_else(|| eyre!("Can't takeout stdout in inject"))
                .with_kind(crate::ErrorKind::Docker)?,
        );
        let output = NonDetachingJoinHandle::from(tokio::spawn(async move {
            match async {
                if let Some(format) = io_format {
                    return match max_by_lines(&mut output, None).await {
                        MaxByLines::Done(buffer) => {
                            Ok::<Value, Error>(
                                match format.from_slice(buffer.as_bytes()) {
                                    Ok(a) => a,
                                    Err(e) => {
                                        tracing::trace!(
                                        "Failed to deserialize stdout from {}: {}, falling back to UTF-8 string.",
                                        format,
                                        e
                                    );
                                        Value::String(buffer)
                                    }
                                },
                            )
                        },
                        MaxByLines::Error(e) => Err(e),
                        MaxByLines::Overflow(buffer) => Ok(Value::String(buffer))
                    }
                }

                let lines = buf_reader_to_lines(&mut output, 1000).await?;
                if lines.is_empty() {
                    return Ok(Value::Null);
                }

                let joined_output = lines.join("\n");
                Ok(Value::String(joined_output))
            }.await {
                Ok(a) => Ok((a, output)),
                Err(e) => Err((e, output))
            }
        }));
        let err_output = BufReader::new(
            handle
                .stderr
                .take()
                .ok_or_else(|| eyre!("Can't takeout std err"))
                .with_kind(crate::ErrorKind::Docker)?,
        );

        let err_output = NonDetachingJoinHandle::from(tokio::spawn(async move {
            let lines = buf_reader_to_lines(err_output, 1000).await?;
            let joined_output = lines.join("\n");
            Ok::<_, Error>(joined_output)
        }));

        let res = tokio::select! {
            res = handle.wait() => Race::Done(res.with_kind(crate::ErrorKind::Docker)?),
            res = timeout_fut => {
                res?;
                Race::TimedOut
            },
        };
        let exit_status = match res {
            Race::Done(x) => x,
            Race::TimedOut => {
                if let Some(id) = id {
                    signal::kill(Pid::from_raw(id as i32), signal::SIGKILL)
                        .with_kind(crate::ErrorKind::Docker)?;
                }
                return Ok(Err((143, "Timed out. Retrying soon...".to_owned())));
            }
        };
        Ok(
            if exit_status.success() || exit_status.code() == Some(143) {
                Ok(serde_json::from_value(
                    output
                        .await
                        .with_kind(crate::ErrorKind::Unknown)?
                        .map(|(v, _)| v)
                        .map_err(|(e, _)| tracing::warn!("{}", e))
                        .unwrap_or_default(),
                )
                .with_kind(crate::ErrorKind::Deserialization)?)
            } else {
                Err((
                    exit_status.code().unwrap_or_default(),
                    err_output.await.with_kind(crate::ErrorKind::Unknown)??,
                ))
            },
        )
    }

    #[instrument(skip(ctx, input))]
    pub async fn sandboxed<I: Serialize, O: DeserializeOwned>(
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
            self.docker_args(ctx, pkg_id, pkg_version, &volumes.to_readonly())
                .await?,
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

        let err_output = BufReader::new(
            handle
                .stderr
                .take()
                .ok_or_else(|| eyre!("Can't takeout std err"))
                .with_kind(crate::ErrorKind::Docker)?,
        );
        let err_output = NonDetachingJoinHandle::from(tokio::spawn(async move {
            let lines = buf_reader_to_lines(err_output, 1000).await?;
            let joined_output = lines.join("\n");
            Ok::<_, Error>(joined_output)
        }));

        let io_format = self.io_format;
        let mut output = BufReader::new(
            handle
                .stdout
                .take()
                .ok_or_else(|| eyre!("Can't takeout stdout in sandboxed"))
                .with_kind(crate::ErrorKind::Docker)?,
        );
        let output = NonDetachingJoinHandle::from(tokio::spawn(async move {
            match async {
                if let Some(format) = io_format {
                    return match max_by_lines(&mut output, None).await {
                        MaxByLines::Done(buffer) => {
                            Ok::<Value, Error>(
                                match format.from_slice(buffer.as_bytes()) {
                                    Ok(a) => a,
                                    Err(e) => {
                                        tracing::trace!(
                                        "Failed to deserialize stdout from {}: {}, falling back to UTF-8 string.",
                                        format,
                                        e
                                    );
                                        Value::String(buffer)
                                    }
                                },
                            )
                        },
                        MaxByLines::Error(e) => Err(e),
                        MaxByLines::Overflow(buffer) => Ok(Value::String(buffer))
                    }
                }

                let lines = buf_reader_to_lines(&mut output, 1000).await?;
                if lines.is_empty() {
                    return Ok(Value::Null);
                }

                let joined_output = lines.join("\n");
                Ok(Value::String(joined_output))
            }.await {
                Ok(a) => Ok((a, output)),
                Err(e) => Err((e, output))
            }
        }));

        let exit_status = handle.wait().await.with_kind(crate::ErrorKind::Docker)?;
        Ok(
            if exit_status.success() || exit_status.code() == Some(143) {
                Ok(serde_json::from_value(
                    output
                        .await
                        .with_kind(crate::ErrorKind::Unknown)?
                        .map(|(v, _)| v)
                        .map_err(|(e, _)| tracing::warn!("{}", e))
                        .unwrap_or_default(),
                )
                .with_kind(crate::ErrorKind::Deserialization)?)
            } else {
                Err((
                    exit_status.code().unwrap_or_default(),
                    err_output.await.with_kind(crate::ErrorKind::Unknown)??,
                ))
            },
        )
    }

    pub fn container_name(pkg_id: &PackageId, name: Option<&str>) -> String {
        if let Some(name) = name {
            format!("{}_{}.{}", pkg_id, name, NET_TLD)
        } else {
            format!("{}.{}", pkg_id, NET_TLD)
        }
    }

    pub fn uncontainer_name(name: &str) -> Option<(PackageId<&str>, Option<&str>)> {
        let (pre_tld, _) = name.split_once('.')?;
        if pre_tld.contains('_') {
            let (pkg, name) = name.split_once('_')?;
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
    ) -> Result<Vec<Cow<'_, OsStr>>, Error> {
        let mut res = self.new_docker_args();
        for (volume_id, dst) in &self.mounts {
            let volume = if let Some(v) = volumes.get(volume_id) {
                v
            } else {
                continue;
            };
            let src = volume.path_for(&ctx.datadir, pkg_id, pkg_version, volume_id);
            if let Err(_e) = tokio::fs::metadata(&src).await {
                tokio::fs::create_dir_all(&src).await?;
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
        res.push(OsStr::new("--log-driver=journald").into());
        res.push(OsStr::new("--entrypoint").into());
        res.push(OsStr::new(&self.entrypoint).into());
        if self.system {
            res.push(OsString::from(self.image.for_package(SYSTEM_PACKAGE_ID, None)).into());
        } else {
            res.push(OsString::from(self.image.for_package(pkg_id, Some(pkg_version))).into());
        }

        res.extend(self.args.iter().map(|s| OsStr::new(s).into()));

        Ok(res)
    }

    fn new_docker_args(&self) -> Vec<Cow<OsStr>> {
        Vec::with_capacity(
            (2 * self.mounts.len()) // --mount <MOUNT_ARG>
                + (2 * self.shm_size_mb.is_some() as usize) // --shm-size <SHM_SIZE>
                + 5 // --interactive --log-driver=journald --entrypoint <ENTRYPOINT> <IMAGE>
                + self.args.len(), // [ARG...]
        )
    }
    async fn docker_args_inject(&self, pkg_id: &PackageId) -> Result<Vec<Cow<'_, OsStr>>, Error> {
        let mut res = self.new_docker_args();
        if let Some(shm_size_mb) = self.shm_size_mb {
            res.push(OsStr::new("--shm-size").into());
            res.push(OsString::from(format!("{}m", shm_size_mb)).into());
        }
        res.push(OsStr::new("--interactive").into());

        res.push(OsString::from(Self::container_name(pkg_id, None)).into());
        res.push(OsStr::new(&self.entrypoint).into());

        res.extend(self.args.iter().map(|s| OsStr::new(s).into()));

        Ok(res)
    }
}

struct RingVec<T> {
    value: VecDeque<T>,
    capacity: usize,
}
impl<T> RingVec<T> {
    fn new(capacity: usize) -> Self {
        RingVec {
            value: VecDeque::with_capacity(capacity),
            capacity,
        }
    }
    fn push(&mut self, item: T) -> Option<T> {
        let popped_item = if self.value.len() == self.capacity {
            self.value.pop_front()
        } else {
            None
        };
        self.value.push_back(item);
        popped_item
    }
}

/// This is created when we wanted a long running docker executor that we could send commands to and get the responses back.
/// We wanted a long running since we want to be able to have the equivelent to the docker execute without the heavy costs of 400 + ms time lag.
/// Also the long running let's us have the ability to start/ end the services quicker.
pub struct LongRunning {
    pub output: UnboundedReceiver<OutputJsonRpc>,
    pub running_output: NonDetachingJoinHandle<()>,
}

impl LongRunning {
    async fn setup_long_running_docker_cmd(
        docker: &DockerProcedure,
        ctx: &RpcContext,
        container_name: &str,
        volumes: &Volumes,
        pkg_id: &PackageId,
        pkg_version: &Version,
    ) -> Result<tokio::process::Command, Error> {
        tracing::error!("BLUJ setup_long_running_docker_cmd {container_name}");
        const INIT_EXEC: &str = "/start9/embassy_container_init";
        const BIND_LOCATION: &str = "/usr/lib/embassy/container";
        tracing::trace!("setup_long_running_docker_cmd");

        LongRunning::cleanup_previous_container(ctx, container_name).await?;

        let image_architecture = {
            let mut cmd = tokio::process::Command::new("docker");
            cmd.arg("image")
                .arg("inspect")
                .arg("--format")
                .arg("'{{.Architecture}}'");

            if docker.system {
                cmd.arg(docker.image.for_package(SYSTEM_PACKAGE_ID, None));
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
            .arg(format!("type=bind,src={BIND_LOCATION},dst=/start9"))
            .arg("--name")
            .arg(&container_name)
            .arg(format!("--hostname={}", &container_name))
            .arg("--entrypoint")
            .arg(format!("{INIT_EXEC}.{image_architecture}"))
            .arg("-i")
            .arg("--rm");

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
            cmd.arg(docker.image.for_package(SYSTEM_PACKAGE_ID, None));
        } else {
            cmd.arg(docker.image.for_package(pkg_id, Some(pkg_version)));
        }
        cmd.stdout(std::process::Stdio::piped());
        cmd.stderr(std::process::Stdio::piped());
        cmd.stdin(std::process::Stdio::piped());
        Ok(cmd)
    }

    async fn cleanup_previous_container(
        ctx: &RpcContext,
        container_name: &str,
    ) -> Result<(), Error> {
        match ctx
            .docker
            .remove_container(
                container_name,
                Some(RemoveContainerOptions {
                    v: false,
                    force: true,
                    link: false,
                }),
            )
            .await
        {
            Ok(())
            | Err(bollard::errors::Error::DockerResponseServerError {
                status_code: 404, // NOT FOUND
                ..
            }) => Ok(()),
            Err(e) => Err(e)?,
        }
    }
    fn spawn_input_handle<S>(
        handle: &mut Child,
        input: S,
    ) -> Result<NonDetachingJoinHandle<()>, Error>
    where
        S: Stream<Item = InputJsonRpc> + Send + 'static,
    {
        use tokio::io::AsyncWriteExt;
        let mut stdin = handle
            .stdin
            .take()
            .ok_or_else(|| eyre!("Can't takeout stdin"))
            .with_kind(crate::ErrorKind::Docker)?;
        let handle = NonDetachingJoinHandle::from(tokio::spawn(async move {
            let input = input;
            tokio::pin!(input);
            while let Some(input) = input.next().await {
                let input = match serde_json::to_string(&input) {
                    Ok(a) => a,
                    Err(e) => {
                        tracing::debug!("{:?}", e);
                        tracing::error!("Docker Input Serialization issue");
                        continue;
                    }
                };
                if let Err(e) = stdin.write_all(format!("{input}\n").as_bytes()).await {
                    tracing::debug!("{:?}", e);
                    tracing::error!("Docker Input issue");
                    return;
                }
            }
        }));
        Ok(handle)
    }
    fn spawn_error_handle(handle: &mut Child) -> Result<NonDetachingJoinHandle<()>, Error> {
        let id = handle.id();
        let mut output = tokio::io::BufReader::new(
            handle
                .stderr
                .take()
                .ok_or_else(|| eyre!("Can't takeout stderr"))
                .with_kind(crate::ErrorKind::Docker)?,
        )
        .lines();
        Ok(NonDetachingJoinHandle::from(tokio::spawn(async move {
            while let Ok(Some(line)) = output.next_line().await {
                tracing::debug!("{:?}", id);
                tracing::error!("Error from long running container");
                tracing::error!("{}", line);
            }
        })))
    }

    fn spawn_output_handle(
        handle: &mut Child,
    ) -> Result<(UnboundedReceiver<OutputJsonRpc>, NonDetachingJoinHandle<()>), Error> {
        let mut output = tokio::io::BufReader::new(
            handle
                .stdout
                .take()
                .ok_or_else(|| eyre!("Can't takeout stdout for long running"))
                .with_kind(crate::ErrorKind::Docker)?,
        )
        .lines();
        let (sender, receiver) = tokio::sync::mpsc::unbounded_channel::<OutputJsonRpc>();
        Ok((
            receiver,
            NonDetachingJoinHandle::from(tokio::spawn(async move {
                loop {
                    let next = output.next_line().await;
                    let next = match next {
                        Ok(Some(a)) => a,
                        Ok(None) => {
                            tracing::error!("The docker pipe is closed?");
                            break;
                        }
                        Err(e) => {
                            tracing::debug!("{:?}", e);
                            tracing::error!("Output from docker, killing");
                            break;
                        }
                    };
                    let next = match serde_json::from_str(&next) {
                        Ok(a) => a,
                        Err(_e) => {
                            tracing::trace!("Could not decode output from long running binary");
                            continue;
                        }
                    };
                    if let Err(e) = sender.send(next) {
                        tracing::debug!("{:?}", e);
                        tracing::error!("Could no longer send output");
                        break;
                    }
                }
            })),
        ))
    }
}
async fn buf_reader_to_lines(
    reader: impl AsyncBufRead + Unpin,
    limit: impl Into<Option<usize>>,
) -> Result<Vec<String>, Error> {
    let lines = stream! {
        let mut lines = reader.lines();
        while let Some(line) = lines.next_line().await? {
            yield Ok::<_, Report>(line);
        }
    };
    let output: RingVec<String> = lines
        .try_fold(
            RingVec::new(limit.into().unwrap_or(1000)),
            |mut acc, line| async move {
                acc.push(line);
                Ok(acc)
            },
        )
        .await
        .with_kind(crate::ErrorKind::Unknown)?;
    let output: Vec<String> = output.value.into_iter().collect();
    Ok(output)
}

enum MaxByLines {
    Done(String),
    Overflow(String),
    Error(Error),
}

async fn max_by_lines(
    reader: impl AsyncBufRead + Unpin,
    max_items: impl Into<Option<usize>>,
) -> MaxByLines {
    let mut answer = String::new();

    let mut lines = reader.lines();
    let mut has_over_blown = false;
    let max_items = max_items.into().unwrap_or(10_000_000);

    while let Some(line) = {
        match lines.next_line().await {
            Ok(a) => a,
            Err(e) => return MaxByLines::Error(e.into()),
        }
    } {
        if has_over_blown {
            continue;
        }
        if !answer.is_empty() {
            answer.push('\n');
        }
        answer.push_str(&line);
        if answer.len() >= max_items {
            has_over_blown = true;
            tracing::warn!("Reading the buffer exceeding limits of {}", max_items);
        }
    }
    if has_over_blown {
        return MaxByLines::Overflow(answer);
    }
    MaxByLines::Done(answer)
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
