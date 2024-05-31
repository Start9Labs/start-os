use std::ffi::OsStr;
use std::path::Path;
use std::sync::Arc;

use imbl_value::InternedString;
use models::{mime, DataUrl, PackageId};
use tokio::fs::File;

use crate::prelude::*;
use crate::registry::signer::commitment::merkle_archive::MerkleArchiveCommitment;
use crate::s9pk::manifest::Manifest;
use crate::s9pk::merkle_archive::file_contents::FileContents;
use crate::s9pk::merkle_archive::sink::Sink;
use crate::s9pk::merkle_archive::source::multi_cursor_file::MultiCursorFile;
use crate::s9pk::merkle_archive::source::{ArchiveSource, DynFileSource, FileSource, Section};
use crate::s9pk::merkle_archive::{Entry, MerkleArchive};
use crate::ARCH;

const MAGIC_AND_VERSION: &[u8] = &[0x3b, 0x3b, 0x02];

pub const SIG_CONTEXT: &str = "s9pk";

pub mod compat;
pub mod manifest;

/**
    /
    ├── manifest.json
    ├── icon.<ext>
    ├── LICENSE.md
    ├── instructions.md
    ├── javascript.squashfs
    ├── assets
    │   └── <id>.squashfs (xN)
    └── images
        └── <arch>
            ├── <id>.env (xN)
            └── <id>.squashfs (xN)
*/

fn priority(s: &str) -> Option<usize> {
    match s {
        "manifest.json" => Some(0),
        a if Path::new(a).file_stem() == Some(OsStr::new("icon")) => Some(1),
        "LICENSE.md" => Some(2),
        "instructions.md" => Some(3),
        "javascript.squashfs" => Some(4),
        "assets" => Some(5),
        "images" => Some(6),
        _ => None,
    }
}

fn filter(p: &Path) -> bool {
    match p.iter().count() {
        1 if p.file_name() == Some(OsStr::new("manifest.json")) => true,
        1 if p.file_stem() == Some(OsStr::new("icon")) => true,
        1 if p.file_name() == Some(OsStr::new("LICENSE.md")) => true,
        1 if p.file_name() == Some(OsStr::new("instructions.md")) => true,
        1 if p.file_name() == Some(OsStr::new("javascript.squashfs")) => true,
        1 if p.file_name() == Some(OsStr::new("assets")) => true,
        1 if p.file_name() == Some(OsStr::new("images")) => true,
        2 if p.parent() == Some(Path::new("assets")) => {
            p.extension().map_or(false, |ext| ext == "squashfs")
        }
        2 if p.parent() == Some(Path::new("images")) => p.file_name() == Some(OsStr::new(&*ARCH)),
        3 if p.parent() == Some(&*Path::new("images").join(&*ARCH)) => p
            .extension()
            .map_or(false, |ext| ext == "squashfs" || ext == "env"),
        _ => false,
    }
}

#[derive(Clone)]
pub struct S9pk<S = Section<MultiCursorFile>> {
    pub manifest: Manifest,
    manifest_dirty: bool,
    archive: MerkleArchive<S>,
    size: Option<u64>,
}
impl<S> S9pk<S> {
    pub fn as_manifest(&self) -> &Manifest {
        &self.manifest
    }
    pub fn as_manifest_mut(&mut self) -> &mut Manifest {
        self.manifest_dirty = true;
        &mut self.manifest
    }
    pub fn as_archive(&self) -> &MerkleArchive<S> {
        &self.archive
    }
    pub fn as_archive_mut(&mut self) -> &mut MerkleArchive<S> {
        &mut self.archive
    }
    pub fn size(&self) -> Option<u64> {
        self.size
    }
}

impl<S: FileSource + Clone> S9pk<S> {
    pub async fn new(archive: MerkleArchive<S>, size: Option<u64>) -> Result<Self, Error> {
        let manifest = extract_manifest(&archive).await?;
        Ok(Self {
            manifest,
            manifest_dirty: false,
            archive,
            size,
        })
    }

    pub async fn icon(&self) -> Result<(InternedString, FileContents<S>), Error> {
        let mut best_icon = None;
        for (path, icon) in self
            .archive
            .contents()
            .with_stem("icon")
            .filter(|(p, _)| {
                Path::new(&*p)
                    .extension()
                    .and_then(|e| e.to_str())
                    .and_then(mime)
                    .map_or(false, |e| e.starts_with("image/"))
            })
            .filter_map(|(k, v)| v.into_file().map(|f| (k, f)))
        {
            let size = icon.size().await?;
            best_icon = match best_icon {
                Some((s, a)) if s >= size => Some((s, a)),
                _ => Some((size, (path, icon))),
            };
        }
        best_icon
            .map(|(_, a)| a)
            .ok_or_else(|| Error::new(eyre!("no icon found in archive"), ErrorKind::ParseS9pk))
    }

    pub async fn icon_data_url(&self) -> Result<DataUrl<'static>, Error> {
        let (name, contents) = self.icon().await?;
        let mime = Path::new(&*name)
            .extension()
            .and_then(|e| e.to_str())
            .and_then(mime)
            .unwrap_or("image/png");
        DataUrl::from_reader(mime, contents.reader().await?, Some(contents.size().await?)).await
    }

    pub async fn serialize<W: Sink>(&mut self, w: &mut W, verify: bool) -> Result<(), Error> {
        use tokio::io::AsyncWriteExt;

        w.write_all(MAGIC_AND_VERSION).await?;
        if !self.manifest_dirty {
            self.archive.serialize(w, verify).await?;
        } else {
            let mut dyn_s9pk = self.clone().into_dyn();
            dyn_s9pk.as_archive_mut().contents_mut().insert_path(
                "manifest.json",
                Entry::file(DynFileSource::new(Arc::<[u8]>::from(
                    serde_json::to_vec(&self.manifest).with_kind(ErrorKind::Serialization)?,
                ))),
            )?;
            dyn_s9pk.archive.serialize(w, verify).await?;
        }

        Ok(())
    }

    pub fn into_dyn(self) -> S9pk<DynFileSource> {
        S9pk {
            manifest: self.manifest,
            manifest_dirty: self.manifest_dirty,
            archive: self.archive.into_dyn(),
            size: self.size,
        }
    }
}

impl<S: ArchiveSource + Clone> S9pk<Section<S>> {
    #[instrument(skip_all)]
    pub async fn deserialize(
        source: &S,
        commitment: Option<&MerkleArchiveCommitment>,
        apply_filter: bool,
    ) -> Result<Self, Error> {
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

        let mut archive =
            MerkleArchive::deserialize(source, SIG_CONTEXT, &mut header, commitment).await?;

        if apply_filter {
            archive.filter(filter)?;
        }

        archive.sort_by(|a, b| match (priority(a), priority(b)) {
            (Some(a), Some(b)) => a.cmp(&b),
            (Some(_), None) => std::cmp::Ordering::Less,
            (None, Some(_)) => std::cmp::Ordering::Greater,
            (None, None) => std::cmp::Ordering::Equal,
        });

        Self::new(archive, source.size().await).await
    }
}
impl S9pk {
    pub async fn from_file(file: File, apply_filter: bool) -> Result<Self, Error> {
        Self::deserialize(&MultiCursorFile::from(file), None, apply_filter).await
    }
    pub async fn open(
        path: impl AsRef<Path>,
        id: Option<&PackageId>,
        apply_filter: bool,
    ) -> Result<Self, Error> {
        let res = Self::from_file(tokio::fs::File::open(path).await?, apply_filter).await?;
        if let Some(id) = id {
            ensure_code!(
                &res.as_manifest().id == id,
                ErrorKind::ValidateS9pk,
                "manifest.id does not match expected"
            );
        }
        Ok(res)
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
