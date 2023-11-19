use crate::prelude::*;
use crate::s9pk::merkle_archive::sink::Sink;
use crate::s9pk::merkle_archive::source::{ArchiveSource, FileSource, Section};
use crate::s9pk::merkle_archive::MerkleArchive;

const MAGIC_AND_VERSION: &[u8] = &[0x3b, 0x3b, 0x02];

pub mod compat;

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

pub struct S9pk<S>(MerkleArchive<S>);
impl<S: FileSource> S9pk<S> {
    pub async fn serialize<W: Sink>(&mut self, w: &mut W, verify: bool) -> Result<(), Error> {
        use tokio::io::AsyncWriteExt;

        w.write_all(MAGIC_AND_VERSION).await?;
        self.0.serialize(w, verify).await?;

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

        Ok(Self(MerkleArchive::deserialize(source, &mut header).await?))
    }
}
