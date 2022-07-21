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
use futures::future::Either as EitherFuture;
use futures::TryStreamExt;
use helpers::NonDetachingJoinHandle;
use nix::sys::signal;
use nix::unistd::Pid;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::io::{AsyncBufRead, AsyncBufReadExt, BufReader};
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
    pub inject: bool,
    #[serde(default)]
    pub shm_size_mb: Option<usize>, // TODO: use postfix sizing? like 1k vs 1m vs 1g
    #[serde(default)]
    pub sigterm_timeout: Option<SerdeDuration>,
}
impl DockerProcedure {
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
        if self.inject && !self.mounts.is_empty() {
            color_eyre::eyre::bail!("mounts not allowed in inject actions");
        }
        Ok(())
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
        let name = name.docker_name();
        let name: Option<&str> = name.as_ref().map(|x| &**x);
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
                Ok(())
                | Err(bollard::errors::Error::DockerResponseServerError {
                    status_code: 404, // NOT FOUND
                    ..
                }) => Ok(()),
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
                .ok_or_else(|| eyre!("Can't takeout stout"))
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
                .ok_or_else(|| eyre!("Can't takeout stout"))
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
            let src = volume.path_for(&ctx.datadir, pkg_id, pkg_version, volume_id);
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
