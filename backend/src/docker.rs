use models::{PackageId, Version};
use tokio::process::Command;

use crate::prelude::*;
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
    Command::new("docker")
        .arg("rmi")
        .arg("-f")
        .arg(&sha.0)
        .invoke(ErrorKind::Docker)
        .await?;
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
