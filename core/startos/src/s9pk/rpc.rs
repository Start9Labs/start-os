use std::path::PathBuf;

use clap::Parser;
use models::ImageId;
use rpc_toolkit::{from_fn_async, Empty, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::fs::File;
use ts_rs::TS;

use crate::context::CliContext;
use crate::prelude::*;
use crate::s9pk::manifest::Manifest;
use crate::s9pk::v2::pack::ImageConfig;
use crate::s9pk::v2::SIG_CONTEXT;
use crate::s9pk::S9pk;
use crate::util::io::TmpDir;
use crate::util::serde::{apply_expr, HandlerExtSerde};

pub const SKIP_ENV: &[&str] = &["TERM", "container", "HOME", "HOSTNAME"];

pub fn s9pk() -> ParentHandler<CliContext> {
    ParentHandler::new()
        .subcommand("pack", from_fn_async(super::v2::pack::pack).no_display())
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
    #[command(flatten)]
    config: ImageConfig,
}
async fn add_image(
    ctx: CliContext,
    AddImageParams { id, config }: AddImageParams,
    S9pkPath { s9pk: s9pk_path }: S9pkPath,
) -> Result<(), Error> {
    let mut s9pk = S9pk::from_file(super::load(&ctx, &s9pk_path).await?)
        .await?
        .into_dyn();
    s9pk.as_manifest_mut().images.insert(id, config);
    let tmpdir = TmpDir::new().await?;
    s9pk.load_images(&tmpdir).await?;
    s9pk.validate_and_filter(None)?;
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
    let mut s9pk = S9pk::from_file(super::load(&ctx, &s9pk_path).await?).await?;
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
    let s9pk = S9pk::from_file(super::load(&ctx, &s9pk).await?).await?;
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

    let s9pk = S9pk::from_file(super::load(&ctx, &s9pk).await?).await?;
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
    let s9pk = S9pk::from_file(super::load(&ctx, &s9pk).await?).await?;
    Ok(s9pk.as_manifest().clone())
}
