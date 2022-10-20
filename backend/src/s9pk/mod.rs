use std::ffi::OsStr;
use std::path::PathBuf;

use color_eyre::eyre::eyre;
use futures::TryStreamExt;
use imbl::OrdMap;
use rpc_toolkit::command;
use serde_json::Value;
use tokio::io::AsyncRead;
use tracing::instrument;

use crate::context::SdkContext;
use crate::s9pk::builder::S9pkPacker;
use crate::s9pk::docker::DockerMultiArch;
use crate::s9pk::manifest::Manifest;
use crate::s9pk::reader::S9pkReader;
use crate::util::display_none;
use crate::util::io::BufferedWriteReader;
use crate::util::serde::IoFormat;
use crate::volume::Volume;
use crate::{Error, ErrorKind, ResultExt};

pub mod builder;
pub mod docker;
pub mod header;
pub mod manifest;
pub mod reader;

pub const SIG_CONTEXT: &'static [u8] = b"s9pk";

#[command(cli_only, display(display_none))]
#[instrument(skip(ctx))]
pub async fn pack(#[context] ctx: SdkContext, #[arg] path: Option<PathBuf>) -> Result<(), Error> {
    use tokio::fs::File;

    let path = if let Some(path) = path {
        path
    } else {
        std::env::current_dir()?
    };
    let manifest_value: Value = if path.join("manifest.toml").exists() {
        IoFormat::Toml
            .from_async_reader(File::open(path.join("manifest.toml")).await?)
            .await?
    } else if path.join("manifest.yaml").exists() {
        IoFormat::Yaml
            .from_async_reader(File::open(path.join("manifest.yaml")).await?)
            .await?
    } else if path.join("manifest.json").exists() {
        IoFormat::Json
            .from_async_reader(File::open(path.join("manifest.json")).await?)
            .await?
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
    let mut outfile = File::create(outfile_path).await?;
    S9pkPacker::builder()
        .manifest(&manifest)
        .writer(&mut outfile)
        .license(
            File::open(path.join(manifest.assets.license_path()))
                .await
                .with_ctx(|_| {
                    (
                        crate::ErrorKind::Filesystem,
                        manifest.assets.license_path().display().to_string(),
                    )
                })?,
        )
        .icon(
            File::open(path.join(manifest.assets.icon_path()))
                .await
                .with_ctx(|_| {
                    (
                        crate::ErrorKind::Filesystem,
                        manifest.assets.icon_path().display().to_string(),
                    )
                })?,
        )
        .instructions(
            File::open(path.join(manifest.assets.instructions_path()))
                .await
                .with_ctx(|_| {
                    (
                        crate::ErrorKind::Filesystem,
                        manifest.assets.instructions_path().display().to_string(),
                    )
                })?,
        )
        .docker_images({
            let docker_images_path = path.join(manifest.assets.docker_images_path());
            let res: Box<dyn AsyncRead + Unpin + Send + Sync> = if tokio::fs::metadata(&docker_images_path).await?.is_dir() {
                let tars: Vec<_> = tokio_stream::wrappers::ReadDirStream::new(tokio::fs::read_dir(&docker_images_path).await?).try_collect().await?;
                let mut arch_info = DockerMultiArch::default();
                for tar in &tars {
                    if tar.path().extension() == Some(OsStr::new("tar")) {
                        arch_info.available.insert(tar.path().file_stem().unwrap_or_default().to_str().unwrap_or_default().to_owned());
                    }
                }
                if arch_info.available.contains("aarch64") {
                    arch_info.default = "aarch64".to_owned();
                } else {
                    arch_info.default = arch_info.available.iter().next().cloned().unwrap_or_default();
                }
                let arch_info_cbor = IoFormat::Cbor.to_vec(&arch_info)?;
                Box::new(BufferedWriteReader::new(|w| async move {
                    let mut docker_images = tokio_tar::Builder::new(w);
                    let mut multiarch_header = tokio_tar::Header::new_gnu();
                    multiarch_header.set_path("multiarch.cbor")?;
                    multiarch_header.set_size(arch_info_cbor.len() as u64);
                    multiarch_header.set_cksum();
                    docker_images.append(&multiarch_header, std::io::Cursor::new(arch_info_cbor)).await?;
                    for tar in tars
                    {
                        docker_images
                            .append_path_with_name(
                                tar.path(),
                                tar.file_name(),
                            )
                            .await?;
                    }
                    Ok::<_, std::io::Error>(())
                }, 1024 * 1024))
            } else {
                Box::new(File::open(docker_images_path)
                    .await
                    .with_ctx(|_| {
                        (
                            crate::ErrorKind::Filesystem,
                            manifest.assets.docker_images_path().display().to_string(),
                        )
                    })?)
            };
            res
        })
        .assets({
            let asset_volumes = manifest
            .volumes
            .iter()
            .filter(|(_, v)| matches!(v, &&Volume::Assets {})).map(|(id, _)| id.clone()).collect::<Vec<_>>();
            let assets_path = manifest.assets.assets_path().to_owned();
            let path = path.clone();

            BufferedWriteReader::new(|w| async move {
                let mut assets = tokio_tar::Builder::new(w);
                for asset_volume in asset_volumes
                {
                    assets
                        .append_dir_all(
                            &asset_volume,
                            path.join(&assets_path).join(&asset_volume),
                        )
                        .await?;
                }
                Ok::<_, std::io::Error>(())
            }, 1024 * 1024)
        })
        .scripts({
            let script_path = path.join(manifest.assets.scripts_path()).join("embassy.js");
            let needs_script = manifest.package_procedures().any(|a| a.is_script());
            let has_script = script_path.exists();
            match (needs_script, has_script) {
                (true, true) => Some(File::open(script_path).await?),
                (true, false) => {
                    return Err(Error::new(eyre!("Script is declared in manifest, but no such script exists at ./scripts/embassy.js"), ErrorKind::Pack).into())
                }
                (false, true) => {
                    tracing::warn!("Manifest does not declare any actions that use scripts, but a script exists at ./scripts/embassy.js");
                    None
                }
                (false, false) => None
            }
        })
        .build()
        .pack(&ctx.developer_key()?)
        .await?;
    outfile.sync_all().await?;

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
