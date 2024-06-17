use std::collections::BTreeMap;
use std::path::Path;
use std::sync::Arc;

use itertools::Itertools;
use models::ImageId;
use tokio::fs::File;
use tokio::io::{AsyncRead, AsyncSeek, AsyncWriteExt};
use tokio::process::Command;

use crate::dependencies::{DepInfo, Dependencies};
use crate::prelude::*;
use crate::s9pk::manifest::Manifest;
use crate::s9pk::merkle_archive::directory_contents::DirectoryContents;
use crate::s9pk::merkle_archive::source::multi_cursor_file::MultiCursorFile;
use crate::s9pk::merkle_archive::source::Section;
use crate::s9pk::merkle_archive::{Entry, MerkleArchive};
use crate::s9pk::rpc::SKIP_ENV;
use crate::s9pk::v1::manifest::{Manifest as ManifestV1, PackageProcedure};
use crate::s9pk::v1::reader::S9pkReader;
use crate::s9pk::v2::pack::{PackSource, CONTAINER_TOOL};
use crate::s9pk::v2::{S9pk, SIG_CONTEXT};
use crate::util::io::TmpDir;
use crate::util::Invoke;

pub const MAGIC_AND_VERSION: &[u8] = &[0x3b, 0x3b, 0x01];

impl S9pk<Section<MultiCursorFile>> {
    #[instrument(skip_all)]
    pub async fn from_v1<R: AsyncRead + AsyncSeek + Unpin + Send + Sync>(
        mut reader: S9pkReader<R>,
        destination: impl AsRef<Path>,
        signer: ed25519_dalek::SigningKey,
    ) -> Result<Self, Error> {
        let scratch_dir = TmpDir::new().await?;

        let mut archive = DirectoryContents::<PackSource>::new();

        // manifest.json
        let manifest_raw = reader.manifest().await?;
        let manifest = from_value::<ManifestV1>(manifest_raw.clone())?;
        let mut new_manifest = Manifest::from(manifest.clone());

        let images: BTreeMap<ImageId, bool> = manifest
            .package_procedures()
            .filter_map(|p| {
                if let PackageProcedure::Docker(p) = p {
                    Some((p.image.clone(), p.system))
                } else {
                    None
                }
            })
            .collect();

        // LICENSE.md
        let license: Arc<[u8]> = reader.license().await?.to_vec().await?.into();
        archive.insert_path(
            "LICENSE.md",
            Entry::file(PackSource::Buffered(license.into())),
        )?;

        // instructions.md
        let instructions: Arc<[u8]> = reader.instructions().await?.to_vec().await?.into();
        archive.insert_path(
            "instructions.md",
            Entry::file(PackSource::Buffered(instructions.into())),
        )?;

        // icon.md
        let icon: Arc<[u8]> = reader.icon().await?.to_vec().await?.into();
        archive.insert_path(
            format!("icon.{}", manifest.assets.icon_type()),
            Entry::file(PackSource::Buffered(icon.into())),
        )?;

        // images
        for arch in reader.docker_arches().await? {
            let images_dir = scratch_dir.join("images").join(&arch);
            let docker_platform = if arch == "x86_64" {
                "--platform=linux/amd64".to_owned()
            } else if arch == "aarch64" {
                "--platform=linux/arm64".to_owned()
            } else {
                format!("--platform=linux/{arch}")
            };
            tokio::fs::create_dir_all(&images_dir).await?;
            Command::new(CONTAINER_TOOL)
                .arg("load")
                .input(Some(&mut reader.docker_images(&arch).await?))
                .invoke(ErrorKind::Docker)
                .await?;
            for (image, system) in &images {
                let mut image_config = new_manifest.images.remove(image).unwrap_or_default();
                image_config.arch.insert(arch.as_str().into());
                new_manifest.images.insert(image.clone(), image_config);
                let sqfs_path = images_dir.join(image).with_extension("squashfs");
                let image_name = if *system {
                    format!("start9/{}:latest", image)
                } else {
                    format!("start9/{}/{}:{}", manifest.id, image, manifest.version)
                };
                let id = String::from_utf8(
                    Command::new(CONTAINER_TOOL)
                        .arg("create")
                        .arg(&docker_platform)
                        .arg(&image_name)
                        .invoke(ErrorKind::Docker)
                        .await?,
                )?;
                let env = String::from_utf8(
                    Command::new(CONTAINER_TOOL)
                        .arg("run")
                        .arg("--rm")
                        .arg(&docker_platform)
                        .arg("--entrypoint")
                        .arg("env")
                        .arg(&image_name)
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
                            .arg("--rm")
                            .arg(&docker_platform)
                            .arg("--entrypoint")
                            .arg("pwd")
                            .arg(&image_name)
                            .invoke(ErrorKind::Docker)
                            .await?,
                    )?
                    .trim(),
                )
                .to_owned();
                Command::new("bash")
                    .arg("-c")
                    .arg(format!(
                        "{CONTAINER_TOOL} export {id} | mksquashfs - {sqfs} -tar",
                        id = id.trim(),
                        sqfs = sqfs_path.display()
                    ))
                    .invoke(ErrorKind::Docker)
                    .await?;
                Command::new(CONTAINER_TOOL)
                    .arg("rm")
                    .arg(id.trim())
                    .invoke(ErrorKind::Docker)
                    .await?;
                archive.insert_path(
                    Path::new("images")
                        .join(&arch)
                        .join(&image)
                        .with_extension("squashfs"),
                    Entry::file(PackSource::File(sqfs_path)),
                )?;
                archive.insert_path(
                    Path::new("images")
                        .join(&arch)
                        .join(&image)
                        .with_extension("env"),
                    Entry::file(PackSource::Buffered(Vec::from(env).into())),
                )?;
                archive.insert_path(
                    Path::new("images")
                        .join(&arch)
                        .join(&image)
                        .with_extension("json"),
                    Entry::file(PackSource::Buffered(
                        serde_json::to_vec(&serde_json::json!({
                            "workdir": workdir
                        }))
                        .with_kind(ErrorKind::Serialization)?
                        .into(),
                    )),
                )?;
                Command::new(CONTAINER_TOOL)
                    .arg("rmi")
                    .arg(&image_name)
                    .invoke(ErrorKind::Docker)
                    .await?;
            }
        }

        // assets
        let asset_dir = scratch_dir.join("assets");
        tokio::fs::create_dir_all(&asset_dir).await?;
        tokio_tar::Archive::new(reader.assets().await?)
            .unpack(&asset_dir)
            .await?;
        for (asset_id, _) in manifest
            .volumes
            .iter()
            .filter(|(_, v)| v.get("type").and_then(|v| v.as_str()) == Some("assets"))
        {
            let assets_path = asset_dir.join(&asset_id);
            let sqfs_path = assets_path.with_extension("squashfs");
            Command::new("mksquashfs")
                .arg(&assets_path)
                .arg(&sqfs_path)
                .invoke(ErrorKind::Filesystem)
                .await?;
            archive.insert_path(
                Path::new("assets")
                    .join(&asset_id)
                    .with_extension("squashfs"),
                Entry::file(PackSource::File(sqfs_path)),
            )?;
        }

        // javascript
        let js_dir = scratch_dir.join("javascript");
        let sqfs_path = js_dir.with_extension("squashfs");
        tokio::fs::create_dir_all(&js_dir).await?;
        if let Some(mut scripts) = reader.scripts().await? {
            let mut js_file = File::create(js_dir.join("embassy.js")).await?;
            tokio::io::copy(&mut scripts, &mut js_file).await?;
            js_file.sync_all().await?;
        }
        {
            let mut js_file = File::create(js_dir.join("embassyManifest.json")).await?;
            js_file
                .write_all(&serde_json::to_vec(&manifest_raw).with_kind(ErrorKind::Serialization)?)
                .await?;
            js_file.sync_all().await?;
        }
        Command::new("mksquashfs")
            .arg(&js_dir)
            .arg(&sqfs_path)
            .invoke(ErrorKind::Filesystem)
            .await?;
        archive.insert_path(
            Path::new("javascript.squashfs"),
            Entry::file(PackSource::File(sqfs_path)),
        )?;

        archive.insert_path(
            "manifest.json",
            Entry::file(PackSource::Buffered(
                serde_json::to_vec::<Manifest>(&new_manifest)
                    .with_kind(ErrorKind::Serialization)?
                    .into(),
            )),
        )?;

        let mut s9pk = S9pk::new(MerkleArchive::new(archive, signer, SIG_CONTEXT), None).await?;
        let mut dest_file = File::create(destination.as_ref()).await?;
        s9pk.serialize(&mut dest_file, false).await?;
        dest_file.sync_all().await?;

        scratch_dir.delete().await?;

        Ok(S9pk::deserialize(
            &MultiCursorFile::from(File::open(destination.as_ref()).await?),
            None,
        )
        .await?)
    }
}

impl From<ManifestV1> for Manifest {
    fn from(value: ManifestV1) -> Self {
        let default_url = value.upstream_repo.clone();
        Self {
            id: value.id,
            title: value.title,
            version: value.version,
            release_notes: value.release_notes,
            license: value.license.into(),
            wrapper_repo: value.wrapper_repo,
            upstream_repo: value.upstream_repo,
            support_site: value.support_site.unwrap_or_else(|| default_url.clone()),
            marketing_site: value.marketing_site.unwrap_or_else(|| default_url.clone()),
            donation_url: value.donation_url,
            description: value.description,
            images: BTreeMap::new(),
            assets: value
                .volumes
                .iter()
                .filter(|(_, v)| v.get("type").and_then(|v| v.as_str()) == Some("assets"))
                .map(|(id, _)| id.clone())
                .collect(),
            volumes: value
                .volumes
                .iter()
                .filter(|(_, v)| v.get("type").and_then(|v| v.as_str()) == Some("data"))
                .map(|(id, _)| id.clone())
                .collect(),
            alerts: value.alerts,
            dependencies: Dependencies(
                value
                    .dependencies
                    .into_iter()
                    .map(|(id, value)| {
                        (
                            id,
                            DepInfo {
                                description: value.description,
                                optional: !value.requirement.required(),
                            },
                        )
                    })
                    .collect(),
            ),
            hardware_requirements: value.hardware_requirements,
            git_hash: value.git_hash,
            os_version: value.eos_version,
            has_config: value.config.is_some(),
        }
    }
}
