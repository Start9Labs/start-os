use std::path::Path;

use crate::prelude::*;
use crate::s9pk::manifest::Manifest;
use crate::s9pk::merkle_archive::sink::Sink;
use crate::s9pk::merkle_archive::source::multi_cursor_file::MultiCursorFile;
use crate::s9pk::merkle_archive::source::{ArchiveSource, FileSource, Section};
use crate::s9pk::merkle_archive::MerkleArchive;

const MAGIC_AND_VERSION: &[u8] = &[0x3b, 0x3b, 0x02];

pub mod compat;
pub mod manifest;

/**
    /
    ├── manifest.json
    ├── LICENSE.md
    ├── instructions.md
    ├── icon.<ext>
    ├── images
    │   └── <arch>
    │       └── <id>.squashfs (xN)
    ├── assets
    │   └── <id>.squashfs (xN)
    └── javascript.squashfs
*/

pub struct S9pk<S = Section<MultiCursorFile>> {
    manifest: Manifest,
    archive: MerkleArchive<S>,
}
impl<S> S9pk<S> {
    pub fn as_manifest(&self) -> &Manifest {
        &self.manifest
    }
    pub fn as_archive(&self) -> &MerkleArchive<S> {
        &self.archive
    }
}

impl<S: FileSource> S9pk<S> {
    pub async fn new(archive: MerkleArchive<S>) -> Result<Self, Error> {
        let manifest = extract_manifest(&archive).await?;
        Ok(Self { manifest, archive })
    }

    pub async fn serialize<W: Sink>(&mut self, w: &mut W, verify: bool) -> Result<(), Error> {
        use tokio::io::AsyncWriteExt;

        w.write_all(MAGIC_AND_VERSION).await?;
        self.archive.serialize(w, verify).await?;

        Ok(())
    }
}

impl<S: ArchiveSource> S9pk<Section<S>> {
    pub async fn deserialize(source: &S) -> Result<Self, Error> {
        use tokio::io::AsyncReadExt;

        let mut header = source
            .fetch(
                0,
                MAGIC_AND_VERSION.len() as u64 + MerkleArchive::<Section<S>>::header_size(),
            )
            .await?;

        let mut magic_version = [0u8; 3];
        header.read_exact(&mut magic_version).await?;
        ensure_code!(
            &magic_version == MAGIC_AND_VERSION,
            ErrorKind::ParseS9pk,
            "Invalid Magic or Unexpected Version"
        );

        let archive = MerkleArchive::deserialize(source, &mut header).await?;
        let manifest = extract_manifest(&archive).await?;

        Ok(Self { archive, manifest })
    }
}
impl S9pk {
    pub async fn open(path: impl AsRef<Path>) -> Result<Self, Error> {
        Self::deserialize(&MultiCursorFile::from(tokio::fs::File::open(path).await?)).await
    }
}

async fn extract_manifest<S: FileSource>(archive: &MerkleArchive<S>) -> Result<Manifest, Error> {
    let manifest = serde_json::from_slice(
        &archive
            .contents()
            .get_path("manifest.json")
            .or_not_found("manifest.json")?
            .read_file_to_vec()
            .await?,
    )
    .with_kind(ErrorKind::Deserialization)?;
    Ok(manifest)
}
