use std::net::Ipv4Addr;
use std::time::Duration;

use models::{Error, ErrorKind, PackageId, ResultExt, Version};
use nix::sys::signal::Signal;
use tokio::process::Command;

use crate::util::Invoke;

#[cfg(not(feature = "podman"))]
pub const CONTAINER_TOOL: &str = "docker";
#[cfg(feature = "podman")]
pub const CONTAINER_TOOL: &str = "podman";

#[cfg(not(feature = "podman"))]
pub const CONTAINER_DATADIR: &str = "/var/lib/docker";
#[cfg(feature = "podman")]
pub const CONTAINER_DATADIR: &str = "/var/lib/containers";

pub struct DockerImageSha(String);

// docker images start9/${package}/*:${version} -q --no-trunc
pub async fn images_for(
    package: &PackageId,
    version: &Version,
) -> Result<Vec<DockerImageSha>, Error> {
    Ok(String::from_utf8(
        Command::new(CONTAINER_TOOL)
            .arg("images")
            .arg(format!("start9/{package}/*:{version}"))
            .arg("--no-trunc")
            .arg("-q")
            .invoke(ErrorKind::Docker)
            .await?,
    )?
    .lines()
    .map(|l| DockerImageSha(l.trim().to_owned()))
    .collect())
}

// docker rmi -f ${sha}
pub async fn remove_image(sha: &DockerImageSha) -> Result<(), Error> {
    match Command::new(CONTAINER_TOOL)
        .arg("rmi")
        .arg("-f")
        .arg(&sha.0)
        .invoke(ErrorKind::Docker)
        .await
        .map(|_| ())
    {
        Err(e)
            if e.source
                .to_string()
                .to_ascii_lowercase()
                .contains("no such image") =>
        {
            Ok(())
        }
        a => a,
    }?;
    Ok(())
}

// docker image prune -f
pub async fn prune_images() -> Result<(), Error> {
    Command::new(CONTAINER_TOOL)
        .arg("image")
        .arg("prune")
        .arg("-f")
        .invoke(ErrorKind::Docker)
        .await?;
    Ok(())
}

// docker container inspect ${name} --format '{{.NetworkSettings.Networks.start9.IPAddress}}'
pub async fn get_container_ip(name: &str) -> Result<Option<Ipv4Addr>, Error> {
    match Command::new(CONTAINER_TOOL)
        .arg("container")
        .arg("inspect")
        .arg(name)
        .arg("--format")
        .arg("{{.NetworkSettings.Networks.start9.IPAddress}}")
        .invoke(ErrorKind::Docker)
        .await
    {
        Err(e)
            if e.source
                .to_string()
                .to_ascii_lowercase()
                .contains("no such container") =>
        {
            Ok(None)
        }
        Err(e) => Err(e),
        Ok(a) => {
            let out = std::str::from_utf8(&a)?.trim();
            if out.is_empty() {
                Ok(None)
            } else {
                Ok(Some({
                    out.parse()
                        .with_ctx(|_| (ErrorKind::ParseNetAddress, out.to_string()))?
                }))
            }
        }
    }
}

// docker stop -t ${timeout} -s ${signal} ${name}
pub async fn stop_container(
    name: &str,
    timeout: Option<Duration>,
    signal: Option<Signal>,
) -> Result<(), Error> {
    let mut cmd = Command::new(CONTAINER_TOOL);
    cmd.arg("stop");
    if let Some(dur) = timeout {
        cmd.arg("-t").arg(dur.as_secs().to_string());
    }
    if let Some(sig) = signal {
        cmd.arg("-s").arg(sig.to_string());
    }
    cmd.arg(name);
    match cmd.invoke(ErrorKind::Docker).await {
        Ok(_) => Ok(()),
        Err(mut e)
            if e.source
                .to_string()
                .to_ascii_lowercase()
                .contains("no such container") =>
        {
            e.kind = ErrorKind::NotFound;
            Err(e)
        }
        Err(e) => Err(e),
    }
}

// docker kill -s ${signal} ${name}
pub async fn kill_container(name: &str, signal: Option<Signal>) -> Result<(), Error> {
    let mut cmd = Command::new(CONTAINER_TOOL);
    cmd.arg("kill");
    if let Some(sig) = signal {
        cmd.arg("-s").arg(sig.to_string());
    }
    cmd.arg(name);
    match cmd.invoke(ErrorKind::Docker).await {
        Ok(_) => Ok(()),
        Err(mut e)
            if e.source
                .to_string()
                .to_ascii_lowercase()
                .contains("no such container") =>
        {
            e.kind = ErrorKind::NotFound;
            Err(e)
        }
        Err(e) => Err(e),
    }
}

// docker pause ${name}
pub async fn pause_container(name: &str) -> Result<(), Error> {
    let mut cmd = Command::new(CONTAINER_TOOL);
    cmd.arg("pause");
    cmd.arg(name);
    match cmd.invoke(ErrorKind::Docker).await {
        Ok(_) => Ok(()),
        Err(mut e)
            if e.source
                .to_string()
                .to_ascii_lowercase()
                .contains("no such container") =>
        {
            e.kind = ErrorKind::NotFound;
            Err(e)
        }
        Err(e) => Err(e),
    }
}

// docker unpause ${name}
pub async fn unpause_container(name: &str) -> Result<(), Error> {
    let mut cmd = Command::new(CONTAINER_TOOL);
    cmd.arg("unpause");
    cmd.arg(name);
    match cmd.invoke(ErrorKind::Docker).await {
        Ok(_) => Ok(()),
        Err(mut e)
            if e.source
                .to_string()
                .to_ascii_lowercase()
                .contains("no such container") =>
        {
            e.kind = ErrorKind::NotFound;
            Err(e)
        }
        Err(e) => Err(e),
    }
}

// docker rm -f ${name}
pub async fn remove_container(name: &str, force: bool) -> Result<(), Error> {
    let mut cmd = Command::new(CONTAINER_TOOL);
    cmd.arg("rm");
    if force {
        cmd.arg("-f");
    }
    cmd.arg(name);
    match cmd.invoke(ErrorKind::Docker).await {
        Ok(_) => Ok(()),
        Err(e)
            if e.source
                .to_string()
                .to_ascii_lowercase()
                .contains("no such container") =>
        {
            Ok(())
        }
        Err(e) => Err(e),
    }
}

// docker network create -d bridge --subnet ${subnet} --opt com.podman.network.bridge.name=${bridge_name}
pub async fn create_bridge_network(
    name: &str,
    subnet: &str,
    bridge_name: &str,
) -> Result<(), Error> {
    let mut cmd = Command::new(CONTAINER_TOOL);
    cmd.arg("network").arg("create");
    cmd.arg("-d").arg("bridge");
    cmd.arg("--subnet").arg(subnet);
    cmd.arg("--opt")
        .arg(format!("com.docker.network.bridge.name={bridge_name}"));
    cmd.arg(name);
    cmd.invoke(ErrorKind::Docker).await?;
    Ok(())
}
