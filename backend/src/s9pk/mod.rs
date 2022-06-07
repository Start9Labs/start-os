use std::path::PathBuf;

use color_eyre::eyre::eyre;
use imbl::OrdMap;
use rpc_toolkit::command;
use serde_json::Value;
use tokio::io::{AsyncRead, AsyncReadExt};
use tracing::instrument;

use crate::context::SdkContext;
use crate::s9pk::builder::S9pkPacker;
use crate::s9pk::manifest::Manifest;
use crate::s9pk::reader::S9pkReader;
use crate::util::display_none;
use crate::util::serde::IoFormat;
use crate::volume::Volume;
use crate::{Error, ErrorKind, ResultExt};

pub mod builder;
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
        .docker_images(
            File::open(path.join(manifest.assets.docker_images_path()))
                .await
                .with_ctx(|_| {
                    (
                        crate::ErrorKind::Filesystem,
                        manifest.assets.docker_images_path().display().to_string(),
                    )
                })?,
        )
        .assets({
            let mut assets = tokio_tar::Builder::new(Vec::new()); // TODO: Ideally stream this? best not to buffer in memory

            for (asset_volume, _) in manifest
                .volumes
                .iter()
                .filter(|(_, v)| matches!(v, &&Volume::Assets {}))
            {
                assets
                    .append_dir_all(
                        asset_volume,
                        path.join(manifest.assets.assets_path()).join(asset_volume),
                    )
                    .await?;
            }

            std::io::Cursor::new(assets.into_inner().await?)
        })
        .scripts({
            let script_base_path = path.join(manifest.assets.scripts_path());
            let needs_script = manifest.package_procedures().any(|a| a.is_script());
            let script_path_js = script_base_path.join("embassy.js");
            let script_path_ts = script_base_path.join("embassy.ts");
            match (needs_script, script_path_js.exists(), script_path_ts.exists()) {
                (false, false, false) => None,
                (false, true, _) => {
                    tracing::warn!("Manifest does not declare any actions that use scripts, but a script exists at ./scripts/embassy.js");
                    None
                }
                (false, false, true) => {
                    tracing::warn!("Manifest does not declare any actions that use scripts, but a script exists at ./scripts/embassy.ts");
                    None
                }
                (true, false, false) => {
                    return Err(Error::new(eyre!("Script is declared in manifest, but no such script exists at ./scripts/embassy.js"), ErrorKind::Pack))
                }
                (true, true, true) => {
                    tracing::warn!("Both ./scripts/embassy.js and ./scripts/embassy.ts exist. Using embassy.js, skipping Typescript bundling...");
                    Some(Box::new(File::open(script_path_js).await?))
                }
                (true, true, false) => Some(Box::new(File::open(script_path_js).await?)),
                (true, false, true) => {
                    #[cfg(feature = "embassy-script")]
                    {
                        use ts_bundler::loader::{BOOTSTRAP, ModsLoader};
                        use ts_bundler::options::{ EMIT_OPTIONS};
                        use ts_bundler::deno_emit;
                        // typecheck against bootstrap
                        println!("path = {:?}", path);
                        println!("script_base_path = {:?}", script_base_path);
                        let mut loader = ModsLoader::from(script_base_path);
                        let code_string = deno_emit::bundle(BOOTSTRAP.parse().unwrap(), &mut loader, None, deno_emit::BundleOptions {
                            bundle_type: deno_emit::BundleType::Module,
                            emit_options: EMIT_OPTIONS.clone(),
                            emit_ignore_directives: true,
                        }).await.map_err(|e| Error::new(eyre!("Typescript bundling failed: {:?}", e), ErrorKind::Javascript))?.code;
                        Some(Box::new(std::io::Cursor::new(code_string)))
                    }
                    #[cfg(not(feature = "embassy-script"))]
                    {
                        panic!("Embassy Script Feature is NOT ENABLED")
                    }
                }
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
