use std::path::Path;
use std::sync::Arc;

use ed25519::signature::Keypair;
use ed25519_dalek::{Signature, SigningKey, VerifyingKey};
use tokio::io::AsyncRead;

use crate::prelude::*;
use crate::s9pk::merkle_archive::directory_contents::DirectoryContents;
use crate::s9pk::merkle_archive::file_contents::FileContents;
use crate::s9pk::merkle_archive::hash::Hash;
use crate::s9pk::merkle_archive::sink::Sink;
use crate::s9pk::merkle_archive::source::{ArchiveSource, DynFileSource, FileSource, Section};
use crate::s9pk::merkle_archive::write_queue::WriteQueue;

pub mod directory_contents;
pub mod file_contents;
pub mod hash;
pub mod sink;
pub mod source;
#[cfg(test)]
mod test;
pub mod varint;
pub mod write_queue;

#[derive(Debug)]
enum Signer {
    Signed(VerifyingKey, Signature),
    Signer(SigningKey),
}

#[derive(Debug)]
pub struct MerkleArchive<S> {
    signer: Signer,
    contents: DirectoryContents<S>,
}
impl<S> MerkleArchive<S> {
    pub fn new(contents: DirectoryContents<S>, signer: SigningKey) -> Self {
        Self {
            signer: Signer::Signer(signer),
            contents,
        }
    }
    pub fn signer(&self) -> VerifyingKey {
        match &self.signer {
            Signer::Signed(k, _) => *k,
            Signer::Signer(k) => k.verifying_key(),
        }
    }
    pub const fn header_size() -> u64 {
        32 // pubkey
                 + 64 // signature
                 + 32 // sighash
                 + DirectoryContents::<Section<S>>::header_size()
    }
    pub fn contents(&self) -> &DirectoryContents<S> {
        &self.contents
    }
    pub fn contents_mut(&mut self) -> &mut DirectoryContents<S> {
        &mut self.contents
    }
    pub fn set_signer(&mut self, key: SigningKey) {
        self.signer = Signer::Signer(key);
    }
    pub fn sort_by(
        &mut self,
        sort_by: impl Fn(&str, &str) -> std::cmp::Ordering + Send + Sync + 'static,
    ) {
        self.contents.sort_by(sort_by)
    }
}
impl<S: ArchiveSource> MerkleArchive<Section<S>> {
    #[instrument(skip_all)]
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

        let mut sighash = [0u8; 32];
        header.read_exact(&mut sighash).await?;
        let sighash = Hash::from_bytes(sighash);

        let contents = DirectoryContents::deserialize(source, header, sighash).await?;

        pubkey.verify_strict(contents.sighash().await?.as_bytes(), &signature)?;

        Ok(Self {
            signer: Signer::Signed(pubkey, signature),
            contents,
        })
    }
}
impl<S: FileSource> MerkleArchive<S> {
    pub async fn update_hashes(&mut self, only_missing: bool) -> Result<(), Error> {
        self.contents.update_hashes(only_missing).await
    }
    pub fn filter(&mut self, filter: impl Fn(&Path) -> bool) -> Result<(), Error> {
        self.contents.filter(filter)
    }
    #[instrument(skip_all)]
    pub async fn serialize<W: Sink>(&self, w: &mut W, verify: bool) -> Result<(), Error> {
        use tokio::io::AsyncWriteExt;

        let sighash = self.contents.sighash().await?;

        let (pubkey, signature) = match &self.signer {
            Signer::Signed(pubkey, signature) => (*pubkey, *signature),
            Signer::Signer(s) => (s.into(), ed25519_dalek::Signer::sign(s, sighash.as_bytes())),
        };

        w.write_all(pubkey.as_bytes()).await?;
        w.write_all(&signature.to_bytes()).await?;
        w.write_all(sighash.as_bytes()).await?;
        let mut next_pos = w.current_position().await?;
        next_pos += DirectoryContents::<S>::header_size();
        self.contents.serialize_header(next_pos, w).await?;
        next_pos += self.contents.toc_size();
        let mut queue = WriteQueue::new(next_pos);
        self.contents.serialize_toc(&mut queue, w).await?;
        queue.serialize(w, verify).await?;
        Ok(())
    }
    pub fn into_dyn(self) -> MerkleArchive<DynFileSource> {
        MerkleArchive {
            signer: self.signer,
            contents: self.contents.into_dyn(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Entry<S> {
    hash: Option<Hash>,
    contents: EntryContents<S>,
}
impl<S> Entry<S> {
    pub fn new(contents: EntryContents<S>) -> Self {
        Self {
            hash: None,
            contents,
        }
    }
    pub fn file(source: S) -> Self {
        Self::new(EntryContents::File(FileContents::new(source)))
    }
    pub fn hash(&self) -> Option<Hash> {
        self.hash
    }
    pub fn as_contents(&self) -> &EntryContents<S> {
        &self.contents
    }
    pub fn as_file(&self) -> Option<&FileContents<S>> {
        match self.as_contents() {
            EntryContents::File(f) => Some(f),
            _ => None,
        }
    }
    pub fn as_directory(&self) -> Option<&DirectoryContents<S>> {
        match self.as_contents() {
            EntryContents::Directory(d) => Some(d),
            _ => None,
        }
    }
    pub fn as_contents_mut(&mut self) -> &mut EntryContents<S> {
        self.hash = None;
        &mut self.contents
    }
    pub fn into_contents(self) -> EntryContents<S> {
        self.contents
    }
    pub fn into_file(self) -> Option<FileContents<S>> {
        match self.into_contents() {
            EntryContents::File(f) => Some(f),
            _ => None,
        }
    }
    pub fn into_directory(self) -> Option<DirectoryContents<S>> {
        match self.into_contents() {
            EntryContents::Directory(d) => Some(d),
            _ => None,
        }
    }
    pub fn header_size(&self) -> u64 {
        32 // hash
        + self.contents.header_size()
    }
}
impl<S: Clone> Entry<S> {}
impl<S: ArchiveSource> Entry<Section<S>> {
    #[instrument(skip_all)]
    pub async fn deserialize(
        source: &S,
        header: &mut (impl AsyncRead + Unpin + Send),
    ) -> Result<Self, Error> {
        use tokio::io::AsyncReadExt;

        let mut hash = [0u8; 32];
        header.read_exact(&mut hash).await?;
        let hash = Hash::from_bytes(hash);

        let contents = EntryContents::deserialize(source, header, hash).await?;

        Ok(Self {
            hash: Some(hash),
            contents,
        })
    }
}
impl<S: FileSource> Entry<S> {
    pub fn filter(&mut self, filter: impl Fn(&Path) -> bool) -> Result<(), Error> {
        if let EntryContents::Directory(d) = &mut self.contents {
            d.filter(filter)?;
        }
        Ok(())
    }
    pub async fn read_file_to_vec(&self) -> Result<Vec<u8>, Error> {
        match self.as_contents() {
            EntryContents::File(f) => Ok(f.to_vec(self.hash).await?),
            EntryContents::Directory(_) => Err(Error::new(
                eyre!("expected file, found directory"),
                ErrorKind::ParseS9pk,
            )),
            EntryContents::Missing => {
                Err(Error::new(eyre!("entry is missing"), ErrorKind::ParseS9pk))
            }
        }
    }
    pub async fn to_missing(&self) -> Result<Self, Error> {
        let hash = if let Some(hash) = self.hash {
            hash
        } else {
            self.contents.hash().await?
        };
        Ok(Self {
            hash: Some(hash),
            contents: EntryContents::Missing,
        })
    }
    pub async fn update_hash(&mut self, only_missing: bool) -> Result<(), Error> {
        if let EntryContents::Directory(d) = &mut self.contents {
            d.update_hashes(only_missing).await?;
        }
        self.hash = Some(self.contents.hash().await?);
        Ok(())
    }
    #[instrument(skip_all)]
    pub async fn serialize_header<W: Sink>(
        &self,
        position: u64,
        w: &mut W,
    ) -> Result<Option<u64>, Error> {
        use tokio::io::AsyncWriteExt;

        let hash = if let Some(hash) = self.hash {
            hash
        } else {
            self.contents.hash().await?
        };
        w.write_all(hash.as_bytes()).await?;
        self.contents.serialize_header(position, w).await
    }
    pub fn into_dyn(self) -> Entry<DynFileSource> {
        Entry {
            hash: self.hash,
            contents: self.contents.into_dyn(),
        }
    }
}

#[derive(Debug, Clone)]
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
    pub fn is_dir(&self) -> bool {
        matches!(self, &EntryContents::Directory(_))
    }
}
impl<S: ArchiveSource> EntryContents<Section<S>> {
    #[instrument(skip_all)]
    pub async fn deserialize(
        source: &S,
        header: &mut (impl AsyncRead + Unpin + Send),
        hash: Hash,
    ) -> Result<Self, Error> {
        use tokio::io::AsyncReadExt;

        let mut type_id = [0u8];
        header.read_exact(&mut type_id).await?;
        match type_id[0] {
            0 => Ok(Self::Missing),
            1 => Ok(Self::File(FileContents::deserialize(source, header).await?)),
            2 => Ok(Self::Directory(
                DirectoryContents::deserialize(source, header, hash).await?,
            )),
            id => Err(Error::new(
                eyre!("Unknown type id {id} found in MerkleArchive"),
                ErrorKind::ParseS9pk,
            )),
        }
    }
}
impl<S: FileSource> EntryContents<S> {
    pub async fn hash(&self) -> Result<Hash, Error> {
        match self {
            Self::Missing => Err(Error::new(
                eyre!("Cannot compute hash of missing file"),
                ErrorKind::Pack,
            )),
            Self::File(f) => f.hash().await,
            Self::Directory(d) => d.sighash().await,
        }
    }
    #[instrument(skip_all)]
    pub async fn serialize_header<W: Sink>(
        &self,
        position: u64,
        w: &mut W,
    ) -> Result<Option<u64>, Error> {
        use tokio::io::AsyncWriteExt;

        w.write_all(&[self.type_id()]).await?;
        Ok(match self {
            Self::Missing => None,
            Self::File(f) => Some(f.serialize_header(position, w).await?),
            Self::Directory(d) => Some(d.serialize_header(position, w).await?),
        })
    }
    pub fn into_dyn(self) -> EntryContents<DynFileSource> {
        match self {
            Self::Missing => EntryContents::Missing,
            Self::File(f) => EntryContents::File(f.into_dyn()),
            Self::Directory(d) => EntryContents::Directory(d.into_dyn()),
        }
    }
}
