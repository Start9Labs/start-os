use std::path::PathBuf;

use clap::Parser;
use rpc_toolkit::{command, from_fn_async, AnyContext, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};

use crate::context::CliContext;
use crate::s9pk::manifest::Manifest;
// use crate::s9pk::reader::S9pkReader;
use crate::util::serde::HandlerExtSerde;
use crate::Error;

pub fn inspect() -> ParentHandler {
    ParentHandler::new()
        .subcommand("hash", from_fn_async(hash))
        .subcommand(
            "manifest",
            from_fn_async(manifest).with_display_serializable(),
        )
        .subcommand("license", from_fn_async(license).no_display())
        .subcommand("icon", from_fn_async(icon).no_display())
        .subcommand("instructions", from_fn_async(instructions).no_display())
        .subcommand("docker-images", from_fn_async(docker_images).no_display())
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
#[ts(export)]
pub struct HashParams {
    path: PathBuf,
}

pub async fn hash(_: CliContext, HashParams { path }: HashParams) -> Result<String, Error> {
    Ok(S9pkReader::open(path, true)
        .await?
        .hash_str()
        .unwrap()
        .to_owned())
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
#[ts(export)]
pub struct ManifestParams {
    path: PathBuf,
    #[arg(long = "no-verify")]
    no_verify: bool,
}

// #[command(cli_only, display(display_serializable))]
pub async fn manifest(
    _: CliContext,
    ManifestParams { .. }: ManifestParams,
) -> Result<Manifest, Error> {
    // S9pkReader::open(path, !no_verify).await?.manifest().await
    todo!()
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
#[ts(export)]
pub struct InspectParams {
    path: PathBuf,
    #[arg(long = "no-verify")]
    no_verify: bool,
}

pub async fn license(
    _: AnyContext,
    InspectParams { path, no_verify }: InspectParams,
) -> Result<(), Error> {
    tokio::io::copy(
        &mut S9pkReader::open(path, !no_verify).await?.license().await?,
        &mut tokio::io::stdout(),
    )
    .await?;
    Ok(())
}

pub async fn icon(
    _: AnyContext,
    InspectParams { path, no_verify }: InspectParams,
) -> Result<(), Error> {
    tokio::io::copy(
        &mut S9pkReader::open(path, !no_verify).await?.icon().await?,
        &mut tokio::io::stdout(),
    )
    .await?;
    Ok(())
}
#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
#[ts(export)]
pub struct InstructionParams {
    path: PathBuf,
    #[arg(long = "no-verify")]
    no_verify: bool,
}

pub async fn instructions(
    _: CliContext,
    InstructionParams { path, no_verify }: InstructionParams,
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
pub async fn docker_images(
    _: AnyContext,
    InspectParams { path, no_verify }: InspectParams,
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
