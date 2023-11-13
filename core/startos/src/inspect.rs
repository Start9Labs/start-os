use std::path::PathBuf;

use rpc_toolkit::command;

use crate::s9pk::manifest::Manifest;
use crate::s9pk::reader::S9pkReader;
use crate::util::display_none;
use crate::util::serde::{display_serializable, IoFormat};
use crate::Error;

#[command(subcommands(hash, manifest, license, icon, instructions, docker_images))]
pub fn inspect() -> Result<(), Error> {
    Ok(())
}

#[command(cli_only)]
pub async fn hash(#[arg] path: PathBuf) -> Result<String, Error> {
    Ok(S9pkReader::open(path, true)
        .await?
        .hash_str()
        .unwrap()
        .to_owned())
}

#[command(cli_only, display(display_serializable))]
pub async fn manifest(
    #[arg] path: PathBuf,
    #[arg(rename = "no-verify", long = "no-verify")] no_verify: bool,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<Manifest, Error> {
    S9pkReader::open(path, !no_verify).await?.manifest().await
}

#[command(cli_only, display(display_none))]
pub async fn license(
    #[arg] path: PathBuf,
    #[arg(rename = "no-verify", long = "no-verify")] no_verify: bool,
) -> Result<(), Error> {
    tokio::io::copy(
        &mut S9pkReader::open(path, !no_verify).await?.license().await?,
        &mut tokio::io::stdout(),
    )
    .await?;
    Ok(())
}

#[command(cli_only, display(display_none))]
pub async fn icon(
    #[arg] path: PathBuf,
    #[arg(rename = "no-verify", long = "no-verify")] no_verify: bool,
) -> Result<(), Error> {
    tokio::io::copy(
        &mut S9pkReader::open(path, !no_verify).await?.icon().await?,
        &mut tokio::io::stdout(),
    )
    .await?;
    Ok(())
}

#[command(cli_only, display(display_none))]
pub async fn instructions(
    #[arg] path: PathBuf,
    #[arg(rename = "no-verify", long = "no-verify")] no_verify: bool,
) -> Result<(), Error> {
    tokio::io::copy(
        &mut S9pkReader::open(path, !no_verify)
            .await?
            .instructions()
            .await?,
        &mut tokio::io::stdout(),
    )
    .await?;
    Ok(())
}

#[command(cli_only, display(display_none), rename = "docker-images")]
pub async fn docker_images(
    #[arg] path: PathBuf,
    #[arg(rename = "no-verify", long = "no-verify")] no_verify: bool,
) -> Result<(), Error> {
    tokio::io::copy(
        &mut S9pkReader::open(path, !no_verify)
            .await?
            .docker_images()
            .await?,
        &mut tokio::io::stdout(),
    )
    .await?;
    Ok(())
}
