use std::path::PathBuf;

use color_eyre::eyre::eyre;
use imbl::OrdMap;
use rpc_toolkit::command;
use serde_json::Value;
use tracing::instrument;

use crate::context::SdkContext;
use crate::s9pk::builder::S9pkPacker;
use crate::s9pk::manifest::Manifest;
use crate::s9pk::reader::S9pkReader;
use crate::util::display_none;
use crate::util::serde::IoFormat;
use crate::volume::Volume;
use crate::{Error, ResultExt};

pub mod builder;
pub mod header;
pub mod manifest;
pub mod reader;

pub const SIG_CONTEXT: &'static [u8] = b"s9pk";

#[command(cli_only, display(display_none), blocking)]
#[instrument(skip(ctx))]
pub fn pack(#[context] ctx: SdkContext, #[arg] path: Option<PathBuf>) -> Result<(), Error> {
    use std::fs::File;

    let path = if let Some(path) = path {
        path
    } else {
        std::env::current_dir()?
    };
    let manifest_value: Value = if path.join("manifest.toml").exists() {
        IoFormat::Toml.from_reader(File::open(path.join("manifest.toml"))?)?
    } else if path.join("manifest.yaml").exists() {
        IoFormat::Yaml.from_reader(File::open(path.join("manifest.yaml"))?)?
    } else if path.join("manifest.json").exists() {
        IoFormat::Json.from_reader(File::open(path.join("manifest.json"))?)?
    } else {
        return Err(Error::new(
            eyre!("manifest not found"),
            crate::ErrorKind::Pack,
        ));
    };
    let manifest: Manifest = serde_json::from_value(manifest_value.clone())
        .with_kind(crate::ErrorKind::Deserialization)?;
    let extra_keys =
        enumerate_extra_keys(&serde_json::to_value(&manifest).unwrap(), &manifest_value);
    for k in extra_keys {
        tracing::warn!("Unrecognized Manifest Key: {}", k);
    }

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
        .scripts({
            let mut scripts = tar::Builder::new(Vec::new());
            scripts.append_dir_all(script_volume, path.join(manifest.assets.scripts_path()))?;
            std::io::Cursor::new(scripts.into_inner()?)
        })
        .build()
        .pack(&ctx.developer_key()?)?;
    outfile.sync_all()?;

    Ok(())
}

#[command(rename = "s9pk", cli_only, display(display_none))]
pub async fn verify(#[arg] path: PathBuf) -> Result<(), Error> {
    let mut s9pk = S9pkReader::open(path, true).await?;
    s9pk.validate().await?;

    Ok(())
}

fn enumerate_extra_keys(reference: &Value, candidate: &Value) -> Vec<String> {
    match (reference, candidate) {
        (Value::Object(m_r), Value::Object(m_c)) => {
            let om_r: OrdMap<String, Value> = m_r.clone().into_iter().collect();
            let om_c: OrdMap<String, Value> = m_c.clone().into_iter().collect();
            let common = om_r.clone().intersection(om_c.clone());
            let top_extra = common.clone().symmetric_difference(om_c.clone());
            let mut all_extra = top_extra
                .keys()
                .map(|s| format!(".{}", s))
                .collect::<Vec<String>>();
            for (k, v) in common {
                all_extra.extend(
                    enumerate_extra_keys(&v, om_c.get(&k).unwrap())
                        .into_iter()
                        .map(|s| format!(".{}{}", k, s)),
                )
            }
            all_extra
        }
        (_, Value::Object(m1)) => m1.clone().keys().map(|s| format!(".{}", s)).collect(),
        _ => Vec::new(),
    }
}

#[test]
fn test_enumerate_extra_keys() {
    use serde_json::json;
    let extras = enumerate_extra_keys(
        &json!({
            "test": 1,
            "test2": null,
        }),
        &json!({
            "test": 1,
            "test2": { "test3": null },
            "test4": null
        }),
    );
    println!("{:?}", extras)
}
