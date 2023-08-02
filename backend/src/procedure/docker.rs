use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::ffi::{OsStr, OsString};
use std::net::Ipv4Addr;
use std::os::unix::prelude::FileTypeExt;
use std::path::{Path, PathBuf};
use std::time::Duration;
use std::{borrow::Cow, sync::Arc};

use async_stream::stream;
use bollard::container::RemoveContainerOptions;
use color_eyre::eyre::eyre;
use color_eyre::Report;
use futures::future::{BoxFuture, Either as EitherFuture};
use futures::{FutureExt, TryStreamExt};
use helpers::{script_dir, NonDetachingJoinHandle, UnixRpcClient};
use models::{Id, ImageId};
use nix::sys::signal;
use nix::unistd::Pid;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::io::{AsyncBufRead, AsyncBufReadExt, BufReader};
use tokio::time::timeout;
use tracing::instrument;

use super::ProcedureName;
use crate::util::Version;
use crate::volume::{VolumeId, Volumes};
use crate::{context::RpcContext, manager::manager_seed::ManagerSeed};
use crate::{
    manager,
    s9pk::manifest::{PackageId, SYSTEM_PACKAGE_ID},
};
use crate::{
    manager::persistent_container::PersistantPaths,
    util::serde::{Duration as SerdeDuration, IoFormat},
};
use crate::{Error, ResultExt, HOST_IP};

pub const NET_TLD: &str = "embassy";
const S9PK_FILE: &str = include_str!("../../../libs/start_init/startInit.js");

lazy_static::lazy_static! {
    pub static ref SYSTEM_IMAGES: BTreeSet<ImageId> = {
        let mut set = BTreeSet::new();

        set.insert("compat".parse().unwrap());
        set.insert("utils".parse().unwrap());

        set
    };

    pub static ref S9PK_PATH: &'static str = {
        use std::fs;
        use std::io::Write;
        let path = "/tmp/s9pk";
        fs::remove_dir_all(path).unwrap_or_default();
        fs::create_dir_all(path).expect("Should be creating s9pk dir");
        fs::File::create(format!("{}/startInit.js", path))
            .unwrap()
            .write_all(S9PK_FILE.as_bytes())
            .unwrap();
        path
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
    #[serde(default)]
    pub gpu_acceleration: bool,
}

impl DockerContainer {
    /// We created a new exec runner, where we are going to be passing the commands for it to run.
    /// Idea is that we are going to send it command and get the inputs be filtered back from the manager.
    /// Then we could in theory run commands without the cost of running the docker exec which is known to have
    /// a dely of > 200ms which is not acceptable.
    #[instrument(skip_all)]
    pub async fn long_running_execute(
        &self,
        seed: &ManagerSeed,
        paths: Arc<PersistantPaths>,
        rpc_client: Arc<UnixRpcClient>,
    ) -> Result<LongRunning, Error> {
        let container_name = DockerProcedure::container_name(&seed.manifest.id, None);

        let mut cmd =
            LongRunning::setup_long_running_docker_cmd(self, seed, &container_name, paths.clone())
                .await?;

        let mut handle = cmd.spawn().with_kind(crate::ErrorKind::Docker)?;

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
            let socket: PathBuf = paths.socket_path();
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

        Ok(LongRunning {
            running_output,
            rpc_client,
        })
    }
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
    pub inject: bool,
    #[serde(default)]
    pub mounts: BTreeMap<VolumeId, PathBuf>,
    #[serde(default)]
    pub io_format: Option<IoFormat>,
    #[serde(default)]
    pub sigterm_timeout: Option<SerdeDuration>,
    #[serde(default)]
    pub shm_size_mb: Option<usize>, // TODO: use postfix sizing? like 1k vs 1m vs 1g
    #[serde(default)]
    pub gpu_acceleration: bool,
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
            inject: false,
            mounts: container.mounts.clone(),
            io_format: injectable.io_format,
            sigterm_timeout: injectable.sigterm_timeout,
            shm_size_mb: container.shm_size_mb,
            gpu_acceleration: container.gpu_acceleration,
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

    #[instrument(skip_all)]
    pub async fn execute<I: Serialize, O: DeserializeOwned>(
        &self,
        seed: &ManagerSeed,
        name: ProcedureName,
        input: Option<I>,
        timeout: Option<Duration>,
    ) -> Result<Result<O, (i32, String)>, Error> {
        let mut cmd = tokio::process::Command::new("docker");
        let container_name = Self::container_name(&seed.manifest.id, name.docker_name());
        cmd.arg("run")
            .arg("--rm")
            .arg("--network=start9")
            .arg(format!("--add-host=embassy:{}", Ipv4Addr::from(HOST_IP)))
            .arg("--name")
            .arg(&container_name)
            .arg(format!("--hostname={}", &container_name))
            .arg("--no-healthcheck")
            .kill_on_drop(true);
        match seed
            .ctx
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
        cmd.args(self.docker_args(&seed, &seed.manifest.volumes).await?);
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

    #[instrument(skip_all)]
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

    #[instrument(skip_all)]
    pub async fn sandboxed<I: Serialize, O: DeserializeOwned>(
        &self,
        seed: &ManagerSeed,
        input: Option<I>,
        timeout: Option<Duration>,
    ) -> Result<Result<O, (i32, String)>, Error> {
        let mut cmd = tokio::process::Command::new("docker");
        cmd.arg("run").arg("--rm").arg("--network=none");
        cmd.args(
            self.docker_args(seed, &seed.manifest.volumes.to_readonly())
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

    pub fn container_name(pkg_id: &PackageId, name: Option<String>) -> String {
        if let Some(name) = name {
            format!("{}_{}.{}", pkg_id, name, NET_TLD)
        } else {
            format!("{}.{}", pkg_id, NET_TLD)
        }
    }

    pub fn uncontainer_name(name: &str) -> Option<(PackageId, Option<&str>)> {
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
        seed: &ManagerSeed,
        volumes: &Volumes,
    ) -> Result<Vec<Cow<'_, OsStr>>, Error> {
        let mut res = self.new_docker_args();

        for (volume_id, dst) in &self.mounts {
            let volume = if let Some(v) = volumes.get(volume_id) {
                v
            } else {
                continue;
            };
            let src = volume.path_for(
                &seed.ctx.datadir,
                &seed.manifest.id,
                &seed.manifest.version,
                volume_id,
            );
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
        if self.gpu_acceleration {
            fn get_devices<'a>(
                path: &'a Path,
                res: &'a mut Vec<PathBuf>,
            ) -> BoxFuture<'a, Result<(), Error>> {
                async move {
                    let mut read_dir = tokio::fs::read_dir(path).await?;
                    while let Some(entry) = read_dir.next_entry().await? {
                        let fty = entry.metadata().await?.file_type();
                        if fty.is_block_device() || fty.is_char_device() {
                            res.push(entry.path());
                        } else if fty.is_dir() {
                            get_devices(&*entry.path(), res).await?;
                        }
                    }
                    Ok(())
                }
                .boxed()
            }
            let mut devices = Vec::new();
            get_devices(Path::new("/dev/dri"), &mut devices).await?;
            for device in devices {
                res.push(OsStr::new("--device").into());
                res.push(OsString::from(device).into());
            }
        }
        res.push(OsStr::new("--interactive").into());
        res.push(OsStr::new("--log-driver=journald").into());
        res.push(OsStr::new("--entrypoint").into());
        res.push(OsStr::new(&self.entrypoint).into());
        if self.system {
            res.push(OsString::from(self.image.for_package(&*SYSTEM_PACKAGE_ID, None)).into());
        } else {
            res.push(
                OsString::from(
                    self.image
                        .for_package(&seed.manifest.id, Some(&seed.manifest.version)),
                )
                .into(),
            );
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
    pub running_output: NonDetachingJoinHandle<()>,
    pub rpc_client: Arc<UnixRpcClient>,
}

impl LongRunning {
    async fn setup_long_running_docker_cmd(
        docker: &DockerContainer,
        seed: &ManagerSeed,
        container_name: &str,
        paths: Arc<PersistantPaths>,
    ) -> Result<tokio::process::Command, Error> {
        const BIND_LOCATION: &str = "/usr/lib/embassy/container/";
        tracing::trace!("setup_long_running_docker_cmd");

        LongRunning::cleanup_previous_container(&seed.ctx, container_name).await?;
        let package_id = &seed.manifest.id;
        let package_version = &seed.manifest.version;
        let volumes = &seed.manifest.volumes;

        let image_architecture = {
            let mut cmd = tokio::process::Command::new("docker");
            cmd.arg("image")
                .arg("inspect")
                .arg("--format")
                .arg("'{{.Architecture}}'");

            if docker.system {
                cmd.arg(docker.image.for_package(&*SYSTEM_PACKAGE_ID, None));
            } else {
                cmd.arg(docker.image.for_package(package_id, Some(package_version)));
            }
            let arch = String::from_utf8(cmd.output().await?.stdout)?;
            arch.replace('\'', "").trim().to_string()
        };
        // TODO BLUJ Need to make sure we use the location for rpc
        // TODO BLUJ need to use the libs location that was built  and included?
        // TODO BLUJ

        let script_dir = script_dir(&seed.ctx.datadir, package_id, package_version);
        let mut cmd = tokio::process::Command::new("docker");
        cmd.arg("run")
            .arg("--network=start9")
            .arg(format!("--add-host=embassy:{}", Ipv4Addr::from(HOST_IP)))
            .arg("--mount")
            .arg(format!(
                "type=bind,src={src},dst=/start-init,readonly",
                src = S9PK_FILE
            ))
            .arg("--mount")
            .arg(format!(
                "type=bind,src={input},dst=/start9/sockets/",
                input = paths.root().display()
            ))
            .arg(format!(
                "type=bind,src={src},dst=/services,readonly",
                src = script_dir.display()
            ))
            .arg("--name")
            .arg(container_name)
            .arg(format!("--hostname={}", &container_name))
            .arg("-i")
            .arg("--rm")
            .kill_on_drop(true);

        for (volume_id, dst) in &docker.mounts {
            let volume = if let Some(v) = volumes.get(volume_id) {
                v
            } else {
                continue;
            };
            let src = volume.path_for(&seed.ctx.datadir, package_id, package_version, volume_id);
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
            cmd.arg(docker.image.for_package(package_id, Some(package_version)));
        }
        cmd.arg("sh").arg("-c").arg("node /start-init/startInit.js");
        cmd.stdout(std::process::Stdio::piped());
        cmd.stderr(std::process::Stdio::inherit());
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
