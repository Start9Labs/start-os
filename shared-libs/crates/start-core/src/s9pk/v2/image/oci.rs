use std::collections::{BTreeMap, BTreeSet};

use base64::Engine;
use oci_spec::distribution::Reference;
use oci_spec::image::{
    Descriptor, DigestAlgorithm, ImageIndex, ImageManifest, MediaType, OciLayout, ToDockerV2S2,
};
use sha2::{Digest, Sha256, Sha384, Sha512};

use super::*;
use crate::s9pk::merkle_archive::source::TmpSource;
use crate::util::HashWriter;
use crate::util::io::{TmpDir, create_file};

#[cfg(test)]
pub(super) mod tests;

#[derive(Deserialize)]
#[serde(untagged)]
enum Manifest {
    Image(ImageManifest),
    Index(ImageIndex),
}

fn media_matches(actual: &MediaType, expected: MediaType) -> bool {
    actual == &expected
        || expected
            .to_docker_v2s2()
            .is_ok_and(|media| actual.as_ref() == media)
}

fn blob_path(descriptor: &Descriptor) -> PathBuf {
    Path::new("blobs")
        .join(descriptor.digest().algorithm().as_ref())
        .join(descriptor.digest().digest())
}

async fn checked_copy<H: Digest + Unpin>(
    source: DynFileSource,
    descriptor: &Descriptor,
) -> Result<DynFileSource, Error> {
    let limit = descriptor
        .size()
        .checked_add(1)
        .ok_or_else(|| invalid("OCI blob size overflow"))?;
    let temp = std::sync::Arc::new(TmpDir::new().await?);
    let path = temp.join("blob");
    let mut writer = HashWriter::new(H::new(), create_file(&path).await?);
    let size = tokio::io::copy(&mut source.reader().await?.take(limit), &mut writer).await?;
    if size != descriptor.size() {
        return Err(invalid("OCI blob does not match its descriptor size").into());
    }
    if hex::encode(writer.finish().0.finalize()) != descriptor.digest().digest() {
        return Err(invalid("OCI blob does not match its descriptor digest").into());
    }
    Ok(TmpSource::new(temp, path).into())
}

async fn checked_blob<S: FileSource + Clone>(
    archive: &Squashfs<S>,
    descriptor: &Descriptor,
) -> Result<DynFileSource, Error> {
    let source = if let Some(data) = descriptor.data() {
        DynFileSource::new(std::sync::Arc::<[u8]>::from(
            base64::prelude::BASE64_STANDARD
                .decode(data)
                .map_err(|_| invalid("invalid OCI embedded blob"))?,
        ))
    } else {
        DynFileSource::new(archive_file(archive, &blob_path(descriptor))?.clone())
    };
    match descriptor.digest().algorithm() {
        DigestAlgorithm::Sha256 => checked_copy::<Sha256>(source, descriptor).await,
        DigestAlgorithm::Sha384 => checked_copy::<Sha384>(source, descriptor).await,
        DigestAlgorithm::Sha512 => checked_copy::<Sha512>(source, descriptor).await,
        _ => Err(invalid("unsupported OCI digest algorithm").into()),
    }
}

fn parse_manifest(bytes: &[u8], expected: &MediaType) -> Result<Manifest, Error> {
    let manifest: Manifest = serde_json::from_slice(bytes).with_kind(ErrorKind::Deserialization)?;
    let (schema, media_type, kind) = match &manifest {
        Manifest::Image(image) => (
            image.schema_version(),
            image.media_type(),
            MediaType::ImageManifest,
        ),
        Manifest::Index(index) => (
            index.schema_version(),
            index.media_type(),
            MediaType::ImageIndex,
        ),
    };
    if schema != 2
        || !media_matches(expected, kind.clone())
        || media_type
            .as_ref()
            .is_some_and(|media| !media_matches(media, kind))
    {
        return Err(invalid("unsupported OCI manifest schema or media type").into());
    }
    Ok(manifest)
}

fn reference_matches(entry: &Descriptor, reference: &str) -> bool {
    let annotation = |key| entry.annotations().as_ref().and_then(|a| a.get(key));
    entry.digest().as_ref() == reference
        || annotation("org.opencontainers.image.ref.name").is_some_and(|name| name == reference)
        || annotation("io.containerd.image.name").is_some_and(|name| {
            match (name.parse::<Reference>(), reference.parse::<Reference>()) {
                (Ok(name), Ok(reference)) => name == reference,
                _ => false,
            }
        })
}

fn layer_media_type(media_type: &MediaType) -> bool {
    matches!(
        media_type,
        MediaType::ImageLayer
            | MediaType::ImageLayerGzip
            | MediaType::ImageLayerZstd
            | MediaType::ImageLayerNonDistributable
            | MediaType::ImageLayerNonDistributableGzip
            | MediaType::ImageLayerNonDistributableZstd
    ) || media_matches(media_type, MediaType::ImageLayerGzip)
        || matches!(
            media_type.as_ref(),
            "application/vnd.docker.image.rootfs.diff.tar"
                | "application/vnd.docker.image.rootfs.foreign.diff.tar.gzip"
        )
}

pub(super) async fn from_layout<S: FileSource + Clone>(
    archive: &Squashfs<S>,
    platform: &str,
    reference: Option<&str>,
) -> Result<(Squashfs<DynFileSource>, DockerImageConfig), Error> {
    let layout: OciLayout = serde_json::from_slice(
        &archive_file(archive, Path::new("oci-layout"))?
            .to_vec(None)
            .await?,
    )
    .with_kind(ErrorKind::Deserialization)?;
    if layout.image_layout_version() != "1.0.0" {
        return Err(invalid("unsupported OCI image layout version").into());
    }
    let root = parse_manifest(
        &archive_file(archive, Path::new("index.json"))?
            .to_vec(None)
            .await?,
        &MediaType::ImageIndex,
    )?;
    let Manifest::Index(mut index) = root else {
        unreachable!()
    };
    if let Some(reference) = reference {
        index.set_manifests(
            index
                .manifests()
                .iter()
                .filter(|entry| reference_matches(entry, reference))
                .cloned()
                .collect(),
        );
    }
    let mut pending = vec![Manifest::Index(index)];
    let mut visited = BTreeSet::new();
    let mut images = BTreeMap::new();
    while let Some(next) = pending.pop() {
        match next {
            Manifest::Index(index) => {
                for entry in index.manifests() {
                    if entry.artifact_type().is_some()
                        || !(media_matches(entry.media_type(), MediaType::ImageManifest)
                            || media_matches(entry.media_type(), MediaType::ImageIndex))
                    {
                        continue;
                    }
                    if let Some(p) = entry.platform() {
                        if !(ImagePlatform {
                            os: p.os().to_string(),
                            architecture: p.architecture().to_string(),
                        })
                        .matches(platform)
                        {
                            continue;
                        }
                    }
                    if !visited.insert(entry.digest().to_string()) {
                        continue;
                    }
                    let source = checked_blob(archive, entry).await?;
                    pending.push(parse_manifest(
                        &source.to_vec(None).await?,
                        entry.media_type(),
                    )?);
                }
            }
            Manifest::Image(image) => {
                if image.artifact_type().is_some()
                    || !media_matches(image.config().media_type(), MediaType::ImageConfig)
                {
                    continue;
                }
                let source = checked_blob(archive, image.config()).await?;
                let config: ArchiveConfig = serde_json::from_slice(&source.to_vec(None).await?)
                    .with_kind(ErrorKind::Deserialization)?;
                if config.platform.matches(platform) {
                    images
                        .entry(image.config().digest().to_string())
                        .or_insert((config, image.layers().clone()));
                }
            }
        }
    }
    if images.is_empty() {
        return Err(Error::new(MissingImagePlatform, ErrorKind::ParseS9pk));
    }
    if images.len() != 1 {
        return Err(invalid("OCI input selects several images for the requested platform").into());
    }
    let (config, descriptors) = images.into_values().next().unwrap();
    let mut layers = Vec::new();
    for descriptor in descriptors {
        if !layer_media_type(descriptor.media_type()) {
            return Err(invalid("unsupported OCI layer media type").into());
        }
        layers.push(checked_blob(archive, &descriptor).await?);
    }
    from_layers(config, layers, platform).await
}
