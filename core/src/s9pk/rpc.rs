use std::ops::Deref;
use std::path::PathBuf;
use std::sync::Arc;

use clap::Parser;
use rpc_toolkit::{Empty, HandlerExt, ParentHandler, from_fn_async};
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use ts_rs::TS;
use url::Url;

use crate::ImageId;
use crate::context::CliContext;
use crate::prelude::*;
use crate::s9pk::manifest::Manifest;
use crate::s9pk::merkle_archive::source::multi_cursor_file::MultiCursorFile;
use crate::s9pk::v2::SIG_CONTEXT;
use crate::s9pk::v2::pack::ImageConfig;
use crate::util::io::{TmpDir, create_file, open_file};
use crate::util::serde::{HandlerExtSerde, apply_expr};
use crate::util::{Apply, Invoke};

pub const SKIP_ENV: &[&str] = &["TERM", "container", "HOME", "HOSTNAME"];

pub fn s9pk() -> ParentHandler<CliContext> {
    ParentHandler::new()
        .subcommand(
            "pack",
            from_fn_async(super::v2::pack::pack)
                .no_display()
                .with_about("about.package-s9pk-input-files-into-valid-s9pk"),
        )
        .subcommand(
            "list-ingredients",
            from_fn_async(super::v2::pack::list_ingredients)
                .with_custom_display_fn(|_, ingredients| {
                    ingredients
                        .into_iter()
                        .map(Some)
                        .apply(|i| itertools::intersperse(i, None))
                        .for_each(|i| {
                            if let Some(p) = i {
                                print!("{}", p.display())
                            } else {
                                print!(" ")
                            }
                        });
                    println!();
                    Ok(())
                })
                .with_about("about.list-paths-of-package-ingredients"),
        )
        .subcommand(
            "edit",
            edit().with_about("about.commands-add-image-or-edit-manifest"),
        )
        .subcommand(
            "inspect",
            inspect().with_about("about.commands-display-file-paths-contents-manifest"),
        )
        .subcommand(
            "convert",
            from_fn_async(convert)
                .no_display()
                .with_about("about.convert-s9pk-v1-to-v2"),
        )
        .subcommand(
            "publish",
            from_fn_async(publish)
                .no_display()
                .with_about("about.publish-s9pk"),
        )
}

#[derive(Deserialize, Serialize, Parser)]
struct S9pkPath {
    #[arg(help = "help.arg.s9pk-file-path")]
    s9pk: PathBuf,
}

fn edit() -> ParentHandler<CliContext, S9pkPath> {
    let only_parent = |a, _| a;
    ParentHandler::new()
        .subcommand(
            "add-image",
            from_fn_async(add_image)
                .with_inherited(only_parent)
                .no_display()
                .with_about("about.add-image-to-s9pk"),
        )
        .subcommand(
            "manifest",
            from_fn_async(edit_manifest)
                .with_inherited(only_parent)
                .with_display_serializable()
                .with_about("about.edit-s9pk-manifest"),
        )
}

fn inspect() -> ParentHandler<CliContext, S9pkPath> {
    let only_parent = |a, _| a;
    ParentHandler::new()
        .subcommand(
            "file-tree",
            from_fn_async(file_tree)
                .with_inherited(only_parent)
                .with_display_serializable()
                .with_about("about.display-list-of-paths"),
        )
        .subcommand(
            "cat",
            from_fn_async(cat)
                .with_inherited(only_parent)
                .no_display()
                .with_about("about.display-file-contents"),
        )
        .subcommand(
            "manifest",
            from_fn_async(inspect_manifest)
                .with_inherited(only_parent)
                .with_display_serializable()
                .with_about("about.display-s9pk-manifest"),
        )
}

#[derive(Deserialize, Serialize, Parser, TS)]
struct AddImageParams {
    #[arg(help = "help.arg.image-id")]
    id: ImageId,
    #[command(flatten)]
    config: ImageConfig,
}
async fn add_image(
    ctx: CliContext,
    AddImageParams { id, config }: AddImageParams,
    S9pkPath { s9pk: s9pk_path }: S9pkPath,
) -> Result<(), Error> {
    let mut s9pk = super::load(
        MultiCursorFile::from(open_file(&s9pk_path).await?),
        || ctx.developer_key().cloned(),
        None,
    )
    .await?;
    s9pk.as_manifest_mut().images.insert(id, config);
    let tmp_dir = Arc::new(TmpDir::new().await?);
    s9pk.load_images(tmp_dir.clone()).await?;
    s9pk.validate_and_filter(None)?;
    let tmp_path = s9pk_path.with_extension("s9pk.tmp");
    let mut tmp_file = create_file(&tmp_path).await?;
    s9pk.serialize(&mut tmp_file, true).await?;
    drop(s9pk);
    tmp_file.sync_all().await?;
    tokio::fs::rename(&tmp_path, &s9pk_path).await?;

    tmp_dir.gc().await?;

    Ok(())
}

#[derive(Deserialize, Serialize, Parser, TS)]
struct EditManifestParams {
    #[arg(help = "help.arg.db-apply-expr")]
    expression: String,
}
async fn edit_manifest(
    ctx: CliContext,
    EditManifestParams { expression }: EditManifestParams,
    S9pkPath { s9pk: s9pk_path }: S9pkPath,
) -> Result<Manifest, Error> {
    let mut s9pk = super::load(
        MultiCursorFile::from(open_file(&s9pk_path).await?),
        || ctx.developer_key().cloned(),
        None,
    )
    .await?;
    let old = serde_json::to_value(s9pk.as_manifest()).with_kind(ErrorKind::Serialization)?;
    *s9pk.as_manifest_mut() = serde_json::from_value(apply_expr(old.into(), &expression)?.into())
        .with_kind(ErrorKind::Serialization)?;
    let manifest = s9pk.as_manifest().clone();
    let tmp_path = s9pk_path.with_extension("s9pk.tmp");
    let mut tmp_file = create_file(&tmp_path).await?;
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
    S9pkPath { s9pk: s9pk_path }: S9pkPath,
) -> Result<Vec<PathBuf>, Error> {
    let s9pk = super::load(
        MultiCursorFile::from(open_file(&s9pk_path).await?),
        || ctx.developer_key().cloned(),
        None,
    )
    .await?;
    Ok(s9pk.as_archive().contents().file_paths(""))
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
struct CatParams {
    #[arg(help = "help.arg.file-path")]
    file_path: PathBuf,
}
async fn cat(
    ctx: CliContext,
    CatParams { file_path }: CatParams,
    S9pkPath { s9pk: s9pk_path }: S9pkPath,
) -> Result<(), Error> {
    use crate::s9pk::merkle_archive::source::FileSource;

    let s9pk = super::load(
        MultiCursorFile::from(open_file(&s9pk_path).await?),
        || ctx.developer_key().cloned(),
        None,
    )
    .await?;
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
    S9pkPath { s9pk: s9pk_path }: S9pkPath,
) -> Result<Manifest, Error> {
    let s9pk = super::load(
        MultiCursorFile::from(open_file(&s9pk_path).await?),
        || ctx.developer_key().cloned(),
        None,
    )
    .await?;
    Ok(s9pk.as_manifest().clone())
}

async fn convert(ctx: CliContext, S9pkPath { s9pk: s9pk_path }: S9pkPath) -> Result<(), Error> {
    let mut s9pk = super::load(
        MultiCursorFile::from(open_file(&s9pk_path).await?),
        || ctx.developer_key().cloned(),
        None,
    )
    .await?;
    let tmp_path = s9pk_path.with_extension("s9pk.tmp");
    s9pk.serialize(&mut create_file(&tmp_path).await?, true)
        .await?;
    tokio::fs::rename(tmp_path, s9pk_path).await?;
    Ok(())
}

async fn publish(ctx: CliContext, S9pkPath { s9pk: s9pk_path }: S9pkPath) -> Result<(), Error> {
    let filename = s9pk_path.file_name().unwrap().to_string_lossy();
    let s9pk = super::S9pk::open(&s9pk_path, None).await?;
    let manifest = s9pk.as_manifest();
    let path = [
        manifest.id.deref(),
        manifest.version.as_str(),
        filename.deref(),
    ];
    let mut s3url = ctx
        .s9pk_s3base
        .as_ref()
        .ok_or_else(|| Error::new(eyre!("--s9pk-s3base required"), ErrorKind::InvalidRequest))?
        .clone();
    s3url
        .path_segments_mut()
        .map_err(|_| {
            Error::new(
                eyre!("s9pk-s3base is invalid (missing protocol?)"),
                ErrorKind::ParseUrl,
            )
        })?
        .pop_if_empty()
        .extend(path);

    let mut s3dest = format!(
        "s3://{}",
        ctx.s9pk_s3bucket
            .as_deref()
            .or_else(|| s3url
                .host_str()
                .and_then(|h| h.split_once(".").map(|h| h.0)))
            .ok_or_else(|| {
                Error::new(eyre!("--s9pk-s3bucket required"), ErrorKind::InvalidRequest)
            })?,
    )
    .parse::<Url>()?;
    s3dest
        .path_segments_mut()
        .map_err(|_| {
            Error::new(
                eyre!("s9pk-s3base is invalid (missing protocol?)"),
                ErrorKind::ParseUrl,
            )
        })?
        .pop_if_empty()
        .extend(path);
    Command::new("s3cmd")
        .arg("put")
        .arg("-P")
        .arg(s9pk_path)
        .arg(s3dest.as_str())
        .invoke(ErrorKind::Network)
        .await?;
    crate::registry::package::add::cli_add_package_impl(ctx, s9pk, vec![s3url], false).await
}
