use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::Parser;
use itertools::Itertools;
use models::ImageId;
use rpc_toolkit::{from_fn_async, Empty, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::fs::File;
use tokio::process::Command;
use ts_rs::TS;

use crate::context::CliContext;
use crate::prelude::*;
use crate::s9pk::manifest::Manifest;
use crate::s9pk::merkle_archive::source::DynFileSource;
use crate::s9pk::merkle_archive::Entry;
use crate::s9pk::v2::compat::CONTAINER_TOOL;
use crate::s9pk::v2::SIG_CONTEXT;
use crate::s9pk::S9pk;
use crate::util::io::TmpDir;
use crate::util::serde::{apply_expr, HandlerExtSerde};
use crate::util::Invoke;

pub const SKIP_ENV: &[&str] = &["TERM", "container", "HOME", "HOSTNAME"];

pub fn s9pk() -> ParentHandler<CliContext> {
    ParentHandler::new()
        .subcommand("edit", edit())
        .subcommand("inspect", inspect())
}

#[derive(Deserialize, Serialize, Parser)]
struct S9pkPath {
    s9pk: PathBuf,
}

fn edit() -> ParentHandler<CliContext, S9pkPath> {
    let only_parent = |a, _| a;
    ParentHandler::new()
        .subcommand(
            "add-image",
            from_fn_async(add_image)
                .with_inherited(only_parent)
                .no_display(),
        )
        .subcommand(
            "manifest",
            from_fn_async(edit_manifest)
                .with_inherited(only_parent)
                .with_display_serializable(),
        )
}

fn inspect() -> ParentHandler<CliContext, S9pkPath> {
    let only_parent = |a, _| a;
    ParentHandler::new()
        .subcommand(
            "file-tree",
            from_fn_async(file_tree)
                .with_inherited(only_parent)
                .with_display_serializable(),
        )
        .subcommand(
            "cat",
            from_fn_async(cat).with_inherited(only_parent).no_display(),
        )
        .subcommand(
            "manifest",
            from_fn_async(inspect_manifest)
                .with_inherited(only_parent)
                .with_display_serializable(),
        )
}

#[derive(Deserialize, Serialize, Parser, TS)]
struct AddImageParams {
    id: ImageId,
    image: String,
    arches: Option<Vec<String>>,
}
async fn add_image(
    ctx: CliContext,
    AddImageParams { id, image, arches }: AddImageParams,
    S9pkPath { s9pk: s9pk_path }: S9pkPath,
) -> Result<(), Error> {
    let mut s9pk = S9pk::from_file(super::load(&ctx, &s9pk_path).await?, false)
        .await?
        .into_dyn();
    let arches: BTreeSet<_> = arches
        .unwrap_or_else(|| vec!["x86_64".to_owned(), "aarch64".to_owned()])
        .into_iter()
        .collect();
    let tmpdir = TmpDir::new().await?;
    for arch in arches {
        let sqfs_path = tmpdir.join(format!("image.{arch}.squashfs"));
        let docker_platform = if arch == "x86_64" {
            "--platform=linux/amd64".to_owned()
        } else if arch == "aarch64" {
            "--platform=linux/arm64".to_owned()
        } else {
            format!("--platform=linux/{arch}")
        };
        let env = String::from_utf8(
            Command::new(CONTAINER_TOOL)
                .arg("run")
                .arg("--rm")
                .arg(&docker_platform)
                .arg("--entrypoint")
                .arg("env")
                .arg(&image)
                .invoke(ErrorKind::Docker)
                .await?,
        )?
        .lines()
        .filter(|l| {
            l.trim()
                .split_once("=")
                .map_or(false, |(v, _)| !SKIP_ENV.contains(&v))
        })
        .join("\n")
            + "\n";
        let workdir = Path::new(
            String::from_utf8(
                Command::new(CONTAINER_TOOL)
                    .arg("run")
                    .arg(&docker_platform)
                    .arg("--rm")
                    .arg("--entrypoint")
                    .arg("pwd")
                    .arg(&image)
                    .invoke(ErrorKind::Docker)
                    .await?,
            )?
            .trim(),
        )
        .to_owned();
        let container_id = String::from_utf8(
            Command::new(CONTAINER_TOOL)
                .arg("create")
                .arg(&docker_platform)
                .arg(&image)
                .invoke(ErrorKind::Docker)
                .await?,
        )?;
        Command::new("bash")
            .arg("-c")
            .arg(format!(
                "{CONTAINER_TOOL} export {container_id} | mksquashfs - {sqfs} -tar",
                container_id = container_id.trim(),
                sqfs = sqfs_path.display()
            ))
            .invoke(ErrorKind::Docker)
            .await?;
        Command::new(CONTAINER_TOOL)
            .arg("rm")
            .arg(container_id.trim())
            .invoke(ErrorKind::Docker)
            .await?;
        let archive = s9pk.as_archive_mut();
        archive.set_signer(ctx.developer_key()?.clone(), SIG_CONTEXT);
        archive.contents_mut().insert_path(
            Path::new("images")
                .join(&arch)
                .join(&id)
                .with_extension("squashfs"),
            Entry::file(DynFileSource::new(sqfs_path)),
        )?;
        archive.contents_mut().insert_path(
            Path::new("images")
                .join(&arch)
                .join(&id)
                .with_extension("env"),
            Entry::file(DynFileSource::new(Arc::from(Vec::from(env)))),
        )?;
        archive.contents_mut().insert_path(
            Path::new("images")
                .join(&arch)
                .join(&id)
                .with_extension("json"),
            Entry::file(DynFileSource::new(Arc::from(
                serde_json::to_vec(&serde_json::json!({
                    "workdir": workdir
                }))
                .with_kind(ErrorKind::Serialization)?,
            ))),
        )?;
    }
    s9pk.as_manifest_mut().images.insert(id);
    let tmp_path = s9pk_path.with_extension("s9pk.tmp");
    let mut tmp_file = File::create(&tmp_path).await?;
    s9pk.serialize(&mut tmp_file, true).await?;
    tmp_file.sync_all().await?;
    tokio::fs::rename(&tmp_path, &s9pk_path).await?;

    Ok(())
}

#[derive(Deserialize, Serialize, Parser, TS)]
struct EditManifestParams {
    expression: String,
}
async fn edit_manifest(
    ctx: CliContext,
    EditManifestParams { expression }: EditManifestParams,
    S9pkPath { s9pk: s9pk_path }: S9pkPath,
) -> Result<Manifest, Error> {
    let mut s9pk = S9pk::from_file(super::load(&ctx, &s9pk_path).await?, false).await?;
    let old = serde_json::to_value(s9pk.as_manifest()).with_kind(ErrorKind::Serialization)?;
    *s9pk.as_manifest_mut() = serde_json::from_value(apply_expr(old.into(), &expression)?.into())
        .with_kind(ErrorKind::Serialization)?;
    let manifest = s9pk.as_manifest().clone();
    let tmp_path = s9pk_path.with_extension("s9pk.tmp");
    let mut tmp_file = File::create(&tmp_path).await?;
    s9pk.as_archive_mut()
        .set_signer(ctx.developer_key()?.clone(), SIG_CONTEXT);
    s9pk.serialize(&mut tmp_file, true).await?;
    tmp_file.sync_all().await?;
    tokio::fs::rename(&tmp_path, &s9pk_path).await?;

    Ok(manifest)
}

async fn file_tree(
    ctx: CliContext,
    _: Empty,
    S9pkPath { s9pk }: S9pkPath,
) -> Result<Vec<PathBuf>, Error> {
    let s9pk = S9pk::from_file(super::load(&ctx, &s9pk).await?, false).await?;
    Ok(s9pk.as_archive().contents().file_paths(""))
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
struct CatParams {
    file_path: PathBuf,
}
async fn cat(
    ctx: CliContext,
    CatParams { file_path }: CatParams,
    S9pkPath { s9pk }: S9pkPath,
) -> Result<(), Error> {
    use crate::s9pk::merkle_archive::source::FileSource;

    let s9pk = S9pk::from_file(super::load(&ctx, &s9pk).await?, false).await?;
    tokio::io::copy(
        &mut s9pk
            .as_archive()
            .contents()
            .get_path(&file_path)
            .or_not_found(&file_path.display())?
            .as_file()
            .or_not_found(&file_path.display())?
            .reader()
            .await?,
        &mut tokio::io::stdout(),
    )
    .await?;
    Ok(())
}

async fn inspect_manifest(
    ctx: CliContext,
    _: Empty,
    S9pkPath { s9pk }: S9pkPath,
) -> Result<Manifest, Error> {
    let s9pk = S9pk::from_file(super::load(&ctx, &s9pk).await?, false).await?;
    Ok(s9pk.as_manifest().clone())
}
