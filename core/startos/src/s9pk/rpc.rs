use std::path::{Path, PathBuf};

use clap::Parser;
use models::ImageId;
use rpc_toolkit::{from_fn_async, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::fs::File;
use tokio::process::Command;

use crate::context::CliContext;
use crate::prelude::*;
use crate::s9pk::merkle_archive::source::DynFileSource;
use crate::s9pk::merkle_archive::Entry;
use crate::s9pk::v2::compat::CONTAINER_TOOL;
use crate::s9pk::S9pk;
use crate::util::io::TmpDir;
use crate::util::Invoke;

pub fn s9pk() -> ParentHandler {
    ParentHandler::new().subcommand("edit", edit())
}

#[derive(Deserialize, Serialize, Parser)]
struct S9pkPath {
    s9pk: PathBuf,
}

fn edit() -> ParentHandler<S9pkPath> {
    let only_parent = |a, _| a;
    ParentHandler::<S9pkPath>::new().subcommand(
        "add-image",
        from_fn_async(add_image)
            .with_inherited(only_parent)
            .no_display(),
    )
}

#[derive(Deserialize, Serialize, Parser)]
struct AddImageParams {
    id: ImageId,
    image: String,
}
async fn add_image(
    ctx: CliContext,
    AddImageParams { id, image }: AddImageParams,
    S9pkPath { s9pk: s9pk_path }: S9pkPath,
) -> Result<(), Error> {
    let tmpdir = TmpDir::new().await?;
    let sqfs_path = tmpdir.join("image.squashfs");
    let arch = String::from_utf8(
        Command::new(CONTAINER_TOOL)
            .arg("run")
            .arg("--rm")
            .arg("--entrypoint")
            .arg("uname")
            .arg(&image)
            .arg("-m")
            .invoke(ErrorKind::Docker)
            .await?,
    )?;
    let container_id = String::from_utf8(
        Command::new(CONTAINER_TOOL)
            .arg("create")
            .arg(&image)
            .invoke(ErrorKind::Docker)
            .await?,
    )?;
    Command::new("bash")
        .arg("-c")
        .arg(format!(
            "{CONTAINER_TOOL} export {container_id} | tar2sqfs {sqfs}",
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
    let mut s9pk = S9pk::from_file(super::load(&ctx, &s9pk_path).await?)
        .await?
        .into_dyn();
    let archive = s9pk.as_archive_mut();
    archive.set_signer(ctx.developer_key()?.clone());
    archive.contents_mut().insert_path(
        Path::new("images")
            .join(arch.trim())
            .join(&id)
            .with_extension("squashfs"),
        Entry::file(DynFileSource::new(sqfs_path)),
    )?;
    let tmp_path = s9pk_path.with_extension("s9pk.tmp");
    let mut tmp_file = File::create(&tmp_path).await?;
    s9pk.serialize(&mut tmp_file, true).await?;
    tmp_file.sync_all().await?;
    tokio::fs::rename(&tmp_path, &s9pk_path).await?;

    Ok(())
}
