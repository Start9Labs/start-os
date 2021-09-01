use std::io::Read;
use std::path::PathBuf;

use anyhow::anyhow;
use rpc_toolkit::command;
use rpc_toolkit::yajrc::RpcError;

use crate::context::CliContext;
use crate::s9pk::builder::S9pkPacker;
use crate::s9pk::manifest::Manifest;
use crate::s9pk::reader::S9pkReader;
use crate::util::display_none;
use crate::volume::Volume;
use crate::{Error, ResultExt};

pub mod builder;
pub mod header;
pub mod manifest;
pub mod reader;

pub const SIG_CONTEXT: &'static [u8] = b"s9pk";

#[command(cli_only, display(display_none), blocking)]
pub fn pack(#[context] ctx: CliContext, #[arg] path: Option<PathBuf>) -> Result<(), Error> {
    use std::fs::File;
    use std::io::Read;

    let path = if let Some(path) = path {
        path
    } else {
        std::env::current_dir()?
    };
    let manifest: Manifest = if path.join("manifest.toml").exists() {
        let mut s = String::new();
        File::open(path.join("manifest.toml"))?.read_to_string(&mut s)?;
        serde_toml::from_str(&s).with_kind(crate::ErrorKind::Deserialization)?
    } else if path.join("manifest.yaml").exists() {
        serde_yaml::from_reader(File::open(path.join("manifest.yaml"))?)
            .with_kind(crate::ErrorKind::Deserialization)?
    } else if path.join("manifest.json").exists() {
        serde_json::from_reader(File::open(path.join("manifest.json"))?)
            .with_kind(crate::ErrorKind::Deserialization)?
    } else {
        return Err(Error::new(
            anyhow!("manifest not found"),
            crate::ErrorKind::Pack,
        ));
    };

    let outfile_path = path.join(format!("{}.s9pk", manifest.id));
    let mut outfile = File::create(outfile_path)?;
    S9pkPacker::builder()
        .manifest(&manifest)
        .writer(&mut outfile)
        .license(
            File::open(path.join(manifest.assets.license_path())).with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    manifest.assets.license_path().display().to_string(),
                )
            })?,
        )
        .icon(
            File::open(path.join(manifest.assets.icon_path())).with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    manifest.assets.icon_path().display().to_string(),
                )
            })?,
        )
        .instructions(
            File::open(path.join(manifest.assets.instructions_path())).with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    manifest.assets.instructions_path().display().to_string(),
                )
            })?,
        )
        .docker_images(
            File::open(path.join(manifest.assets.docker_images_path())).with_ctx(|_| {
                (
                    crate::ErrorKind::Filesystem,
                    manifest.assets.docker_images_path().display().to_string(),
                )
            })?,
        )
        .assets({
            let mut assets = tar::Builder::new(Vec::new()); // TODO: Ideally stream this? best not to buffer in memory

            for (asset_volume, _) in manifest
                .volumes
                .iter()
                .filter(|(_, v)| matches!(v, &&Volume::Assets {}))
            {
                assets.append_dir_all(
                    asset_volume,
                    path.join(manifest.assets.assets_path()).join(asset_volume),
                )?;
            }

            std::io::Cursor::new(assets.into_inner()?)
        })
        .build()
        .pack(&ctx.developer_key()?)?;
    outfile.sync_all()?;

    Ok(())
}

#[command(cli_only, display(display_none))]
pub async fn verify(#[arg] path: PathBuf) -> Result<(), Error> {
    let mut s9pk = S9pkReader::open(path).await?;
    s9pk.validate().await?;

    Ok(())
}
