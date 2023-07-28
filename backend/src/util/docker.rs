use std::net::IpAddr;

use models::{PackageId, Version};
use tokio::process::Command;

use crate::util::Invoke;

pub struct DockerImageSha(String);

// docker images start9/${package}/*:${version} -q --no-trunc
pub async fn images_for(
    package: &PackageId,
    version: &Version,
) -> Result<Vec<DockerImageSha>, Error> {
    Ok(String::from_utf8(
        Command::new("docker")
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
    match Command::new("docker")
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
                .starts_with("Error response from daemon: No such image:") =>
        {
            Ok(())
        }
        a => a,
    }?;
    Ok(())
}

// docker image prune -f
pub async fn prune_images() -> Result<(), Error> {
    Command::new("docker")
        .arg("image")
        .arg("prune")
        .arg("-f")
        .invoke(ErrorKind::Docker)
        .await?;
    Ok(())
}

// docker container rm -f ${name}
pub async fn remove_container(name: &str) -> Result<(), Error> {
    match Command::new("docker")
        .arg("container")
        .arg("rm")
        .arg("-f")
        .arg(name)
        .invoke(ErrorKind::Docker)
        .await
        .map(|_| ())
    {
        Err(e)
            if e.source
                .to_string()
                .starts_with("Error response from daemon: No such container:") =>
        {
            Ok(())
        }
        a => a,
    }?;
    Ok(())
}

// docker container inspect ${name} --format '{{.NetworkSettings.Networks.start9.IPAddress}}'
pub async fn get_container_ip(name: &str) -> Result<Option<IpAddr>, Error> {
    match Command::new("docker")
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
                .starts_with("Error response from daemon: No such container:") =>
        {
            Ok(None)
        }
        Err(e) => Err(e),
        Ok(a) => Ok(Some(std::str::from_utf8(&a)?.parse()?)),
    }
}
