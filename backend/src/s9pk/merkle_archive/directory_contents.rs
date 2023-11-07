use std::collections::BTreeMap;

use futures::future::BoxFuture;
use futures::FutureExt;
use imbl_value::InternedString;
use tokio::io::AsyncRead;

use crate::prelude::*;
use crate::s9pk::merkle_archive::hash::{Hash, HashWriter};
use crate::s9pk::merkle_archive::sink::{Sink, TrackingWriter};
use crate::s9pk::merkle_archive::source::{ArchiveSource, FileSource, Section};
use crate::s9pk::merkle_archive::{varint, Entry, EntryContents};

pub struct DirectoryContents<S>(BTreeMap<InternedString, Entry<S>>);
impl<S> DirectoryContents<S> {
    pub const fn header_size() -> u64 {
        8 // position: u64 BE
        + 8 // size: u64 BE
    }
    pub async fn serialize_header<W: Sink>(
        &self,
        next_pos: &mut u64,
        w: &mut W,
    ) -> Result<(), Error> {
        use tokio::io::AsyncWriteExt;

        let size = self.toc_size();

        let position = *next_pos;
        *next_pos += size;

        w.write_all(&position.to_be_bytes()).await?;
        w.write_all(&size.to_be_bytes()).await?;

        Ok(())
    }

    pub fn toc_size(&self) -> u64 {
        self.0.iter().fold(
            varint::serialized_varint_size(self.0.len() as u64),
            |acc, (name, entry)| {
                acc + varint::serialized_varstring_size(&**name) + entry.header_size()
            },
        )
    }
}
impl<S: ArchiveSource> DirectoryContents<Section<S>> {
    pub fn deserialize<'a>(
        source: &'a S,
        header: &'a mut (impl AsyncRead + Unpin + Send),
    ) -> BoxFuture<'a, Result<Self, Error>> {
        async move {
            use tokio::io::AsyncReadExt;

            let mut position = [0u8; 8];
            header.read_exact(&mut position).await?;
            let position = u64::from_be_bytes(position);

            let mut size = [0u8; 8];
            header.read_exact(&mut size).await?;
            let size = u64::from_be_bytes(size);

            let mut toc_reader = source.fetch(position, size).await?;

            let len = varint::deserialize_varint(&mut toc_reader).await?;
            let mut entries = BTreeMap::new();
            for _ in 0..len {
                entries.insert(
                    varint::deserialize_varstring(&mut toc_reader).await?.into(),
                    Entry::deserialize(source, &mut toc_reader).await?,
                );
            }

            Ok(Self(entries))
        }
        .boxed()
    }
}
impl<S: FileSource> DirectoryContents<S> {
    pub async fn sighash(&self) -> Result<Hash, Error> {
        let mut hasher = TrackingWriter::new(0, HashWriter::new());
        Self(
            self.0
                .iter()
                .map(|(name, entry)| (name.clone(), entry.to_missing()))
                .collect(),
        )
        .serialize_toc(&mut 0, &mut hasher)
        .await?;
        Ok(hasher.into_inner().finalize())
    }

    pub async fn serialize_toc<W: Sink>(&self, next_pos: &mut u64, w: &mut W) -> Result<(), Error> {
        varint::serialize_varint(self.0.len() as u64, w).await?;
        for (name, entry) in self.0.iter() {
            varint::serialize_varstring(&**name, w).await?;
            entry.serialize_header(next_pos, w).await?;
        }

        Ok(())
    }
    pub fn serialize_entries<'a, W: Sink>(
        &'a self,
        next_pos: &'a mut u64,
        w: &'a mut W,
        verify: bool,
    ) -> BoxFuture<'a, Result<(), Error>> {
        async move {
            for entry in self.0.values() {
                match &entry.contents {
                    EntryContents::Missing => (),
                    EntryContents::File(f) => {
                        f.serialize_body(w, if verify { Some(entry.hash) } else { None })
                            .await?;
                    }
                    EntryContents::Directory(d) => {
                        d.serialize_toc(next_pos, w).await?;
                        if verify {
                            ensure_code!(
                                d.sighash().await? == entry.hash,
                                ErrorKind::InvalidSignature,
                                "hash sum does not match"
                            );
                        }
                    }
                }
            }
            for entry in self.0.values() {
                if let EntryContents::Directory(d) = &entry.contents {
                    d.serialize_entries(next_pos, w, verify).await?;
                }
            }

            Ok(())
        }
        .boxed()
    }
}
