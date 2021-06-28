use std::io::Read;
use std::path::PathBuf;

use anyhow::anyhow;
use rpc_toolkit::command;
use rpc_toolkit::yajrc::RpcError;

use crate::context::{CliContext, EitherContext};
use crate::s9pk::builder::S9pkPacker;
use crate::s9pk::manifest::Manifest;
use crate::s9pk::reader::S9pkReader;
use crate::util::display_none;
use crate::{Error, ResultExt};

pub mod builder;
pub mod header;
pub mod manifest;
pub mod reader;

pub const SIG_CONTEXT: &'static [u8] = b"s9pk";

#[command(cli_only, display(display_none), blocking)]
pub fn pack(#[context] _ctx: EitherContext, #[arg] path: Option<PathBuf>) -> Result<(), Error> {
    use std::fs::File;

    let path = if let Some(path) = path {
        path
    } else {
        std::env::current_dir()?
    };
    let manifest: Manifest = if path.join("manifest.toml").exists() {
        let mut s = String::new();
        File::open(path.join("manifest.toml"))?.read_to_string(&mut s)?;
        serde_toml::from_str(&s).with_kind(crate::ErrorKind::Deserialization)?
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
        .license(File::open(path.join(manifest.assets.license_path()))?)
        .icon(File::open(path.join(manifest.assets.icon_path()))?)
        .instructions(File::open(path.join(manifest.assets.instructions_path()))?)
        .docker_images(File::open(path.join(manifest.assets.docker_images_path()))?)
        .build()
        .pack()?;
    outfile.sync_all()?;

    Ok(())
}

#[command(cli_only, display(display_none))]
pub async fn verify(#[context] _ctx: EitherContext, #[arg] path: PathBuf) -> Result<(), Error> {
    let mut s9pk = S9pkReader::open(path).await?;
    s9pk.validate().await?;

    Ok(())
}
