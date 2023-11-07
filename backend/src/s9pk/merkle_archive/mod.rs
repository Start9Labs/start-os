use ed25519_dalek::{Signature, VerifyingKey};
use tokio::io::AsyncRead;

use crate::prelude::*;
use crate::s9pk::merkle_archive::directory_contents::DirectoryContents;
use crate::s9pk::merkle_archive::file_contents::FileContents;
use crate::s9pk::merkle_archive::hash::Hash;
use crate::s9pk::merkle_archive::sink::Sink;
use crate::s9pk::merkle_archive::source::{ArchiveSource, FileSource, Section};

pub mod directory_contents;
pub mod file_contents;
pub mod hash;
pub mod sink;
pub mod source;
pub mod varint;

pub struct MerkleArchive<S> {
    pubkey: VerifyingKey,
    signature: Signature,
    contents: DirectoryContents<S>,
}
impl<S> MerkleArchive<S> {
    pub const fn header_size() -> u64 {
        32 // pubkey
                 + 64 // signature
                 + DirectoryContents::<Section<S>>::header_size()
    }
}
impl<S: ArchiveSource> MerkleArchive<Section<S>> {
    pub async fn deserialize(
        source: &S,
        header: &mut (impl AsyncRead + Unpin + Send),
    ) -> Result<Self, Error> {
        use tokio::io::AsyncReadExt;

        let mut pubkey = [0u8; 32];
        header.read_exact(&mut pubkey).await?;
        let pubkey = VerifyingKey::from_bytes(&pubkey)?;

        let mut signature = [0u8; 64];
        header.read_exact(&mut signature).await?;
        let signature = Signature::from_bytes(&signature);

        let contents = DirectoryContents::deserialize(source, header).await?;

        pubkey.verify_strict(contents.sighash().await?.as_bytes(), &signature)?;

        Ok(Self {
            pubkey,
            signature,
            contents,
        })
    }
}
impl<S: FileSource> MerkleArchive<S> {
    pub async fn serialize<W: Sink>(&self, w: &mut W) -> Result<(), Error> {
        use tokio::io::AsyncWriteExt;

        w.write_all(self.pubkey.as_bytes()).await?;
        w.write_all(&self.signature.to_bytes()).await?;
        let mut next_pos = w.current_position().await?;
        self.contents.serialize_toc(&mut next_pos, w).await?;
        self.contents
            .serialize_entries(&mut next_pos, w, S::TRUSTED)
            .await?;
        Ok(())
    }
}

pub struct Entry<S> {
    hash: Hash,
    contents: EntryContents<S>,
}
impl<S> Entry<S> {
    pub fn header_size(&self) -> u64 {
        32 // hash
        + self.contents.header_size()
    }
    pub fn to_missing(&self) -> Self {
        Self {
            hash: self.hash,
            contents: EntryContents::Missing,
        }
    }
}
impl<S: ArchiveSource> Entry<Section<S>> {
    pub async fn deserialize(
        source: &S,
        header: &mut (impl AsyncRead + Unpin + Send),
    ) -> Result<Self, Error> {
        use tokio::io::AsyncReadExt;

        let mut hash = [0u8; 32];
        header.read_exact(&mut hash).await?;
        let hash = Hash::from_bytes(hash);

        let contents = EntryContents::deserialize(source, header).await?;

        Ok(Self { hash, contents })
    }
}
impl<S: FileSource> Entry<S> {
    pub async fn serialize_header<W: Sink>(
        &self,
        next_pos: &mut u64,
        w: &mut W,
    ) -> Result<(), Error> {
        use tokio::io::AsyncWriteExt;

        w.write_all(self.hash.as_bytes()).await?;
        self.contents.serialize_header(next_pos, w).await?;

        Ok(())
    }
}

pub enum EntryContents<S> {
    Missing,
    File(FileContents<S>),
    Directory(DirectoryContents<S>),
}
impl<S> EntryContents<S> {
    fn type_id(&self) -> u8 {
        match self {
            Self::Missing => 0,
            Self::File(_) => 1,
            Self::Directory(_) => 2,
        }
    }
    pub fn header_size(&self) -> u64 {
        1 // type
        + match self {
            Self::Missing => 0,
            Self::File(_) => FileContents::<S>::header_size(),
            Self::Directory(_) => DirectoryContents::<S>::header_size(),
        }
    }
}
impl<S: ArchiveSource> EntryContents<Section<S>> {
    pub async fn deserialize(
        source: &S,
        header: &mut (impl AsyncRead + Unpin + Send),
    ) -> Result<Self, Error> {
        use tokio::io::AsyncReadExt;

        let mut type_id = [0u8];
        header.read_exact(&mut type_id).await?;
        match type_id[0] {
            0 => Ok(Self::Missing),
            1 => Ok(Self::File(FileContents::deserialize(source, header).await?)),
            2 => Ok(Self::Directory(
                DirectoryContents::deserialize(source, header).await?,
            )),
            id => Err(Error::new(
                eyre!("Unknown type id {id} found in MerkleArchive"),
                ErrorKind::ParseS9pk,
            )),
        }
    }
}
impl<S: FileSource> EntryContents<S> {
    pub async fn serialize_header<W: Sink>(
        &self,
        next_pos: &mut u64,
        w: &mut W,
    ) -> Result<(), Error> {
        use tokio::io::AsyncWriteExt;

        w.write_all(&[self.type_id()]).await?;
        match self {
            Self::Missing => (),
            Self::File(f) => f.serialize_header(next_pos, w).await?, // position: u64 BE
            Self::Directory(d) => d.serialize_header(next_pos, w).await?, // position: u64 BE
        }

        Ok(())
    }
}
