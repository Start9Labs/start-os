use std::collections::BTreeMap;
use std::path::Path;
use std::sync::Arc;

use exver::ExtendedVersion;
use models::ImageId;
use tokio::io::{AsyncRead, AsyncSeek, AsyncWriteExt};
use tokio::process::Command;

use crate::dependencies::{DepInfo, Dependencies};
use crate::prelude::*;
use crate::s9pk::manifest::Manifest;
use crate::s9pk::merkle_archive::directory_contents::DirectoryContents;
use crate::s9pk::merkle_archive::source::TmpSource;
use crate::s9pk::merkle_archive::{Entry, MerkleArchive};
use crate::s9pk::v1::manifest::{Manifest as ManifestV1, PackageProcedure};
use crate::s9pk::v1::reader::S9pkReader;
use crate::s9pk::v2::pack::{ImageSource, PackSource, CONTAINER_TOOL};
use crate::s9pk::v2::{S9pk, SIG_CONTEXT};
use crate::util::io::{create_file, TmpDir};
use crate::util::Invoke;

pub const MAGIC_AND_VERSION: &[u8] = &[0x3b, 0x3b, 0x01];

impl S9pk<TmpSource<PackSource>> {
    #[instrument(skip_all)]
    pub async fn from_v1<R: AsyncRead + AsyncSeek + Unpin + Send + Sync>(
        mut reader: S9pkReader<R>,
        tmp_dir: Arc<TmpDir>,
        signer: ed25519_dalek::SigningKey,
    ) -> Result<Self, Error> {
        let mut archive = DirectoryContents::<TmpSource<PackSource>>::new();

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
            Entry::file(TmpSource::new(
                tmp_dir.clone(),
                PackSource::Buffered(license.into()),
            )),
        )?;

        // instructions.md
        let instructions: Arc<[u8]> = reader.instructions().await?.to_vec().await?.into();
        archive.insert_path(
            "instructions.md",
            Entry::file(TmpSource::new(
                tmp_dir.clone(),
                PackSource::Buffered(instructions.into()),
            )),
        )?;

        // icon.md
        let icon: Arc<[u8]> = reader.icon().await?.to_vec().await?.into();
        archive.insert_path(
            format!("icon.{}", manifest.assets.icon_type()),
            Entry::file(TmpSource::new(
                tmp_dir.clone(),
                PackSource::Buffered(icon.into()),
            )),
        )?;

        // images
        for arch in reader.docker_arches().await? {
            let images_dir = tmp_dir.join("images").join(&arch);
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
                let image_name = if *system {
                    format!("start9/{}:latest", image)
                } else {
                    format!("start9/{}/{}:{}", manifest.id, image, manifest.version)
                };
                ImageSource::DockerTag(image_name.clone())
                    .load(
                        tmp_dir.clone(),
                        &new_manifest.id,
                        &new_manifest.version,
                        image,
                        &arch,
                        &mut archive,
                    )
                    .await?;
                Command::new(CONTAINER_TOOL)
                    .arg("rmi")
                    .arg("-f")
                    .arg(&image_name)
                    .invoke(ErrorKind::Docker)
                    .await?;
            }
        }

        // assets
        let asset_dir = tmp_dir.join("assets");
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
                Entry::file(TmpSource::new(tmp_dir.clone(), PackSource::File(sqfs_path))),
            )?;
        }

        // javascript
        let js_dir = tmp_dir.join("javascript");
        let sqfs_path = js_dir.with_extension("squashfs");
        tokio::fs::create_dir_all(&js_dir).await?;
        if let Some(mut scripts) = reader.scripts().await? {
            let mut js_file = create_file(js_dir.join("embassy.js")).await?;
            tokio::io::copy(&mut scripts, &mut js_file).await?;
            js_file.sync_all().await?;
        }
        {
            let mut js_file = create_file(js_dir.join("embassyManifest.json")).await?;
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
            Entry::file(TmpSource::new(tmp_dir.clone(), PackSource::File(sqfs_path))),
        )?;

        archive.insert_path(
            "manifest.json",
            Entry::file(TmpSource::new(
                tmp_dir.clone(),
                PackSource::Buffered(
                    serde_json::to_vec::<Manifest>(&new_manifest)
                        .with_kind(ErrorKind::Serialization)?
                        .into(),
                ),
            )),
        )?;

        let mut res = S9pk::new(MerkleArchive::new(archive, signer, SIG_CONTEXT), None).await?;
        res.as_archive_mut().update_hashes(true).await?;
        Ok(res)
    }
}

impl From<ManifestV1> for Manifest {
    fn from(value: ManifestV1) -> Self {
        let default_url = value.upstream_repo.clone();
        Self {
            id: value.id,
            title: value.title,
            version: ExtendedVersion::from(value.version).into(),
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
