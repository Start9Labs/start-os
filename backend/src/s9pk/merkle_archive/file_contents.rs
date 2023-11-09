use tokio::io::AsyncRead;

use crate::prelude::*;
use crate::s9pk::merkle_archive::hash::{Hash, HashWriter, VerifyingWriter};
use crate::s9pk::merkle_archive::sink::{Sink, TrackingWriter};
use crate::s9pk::merkle_archive::source::{ArchiveSource, FileSource, Section};

#[derive(Debug)]
pub struct FileContents<S>(S);
impl<S> FileContents<S> {
    pub fn new(source: S) -> Self {
        Self(source)
    }
    pub const fn header_size() -> u64 {
        8 // position: u64 BE
        + 8 // size: u64 BE
    }
}
impl<S: ArchiveSource> FileContents<Section<S>> {
    #[instrument(skip_all)]
    pub async fn deserialize(
        source: &S,
        header: &mut (impl AsyncRead + Unpin + Send),
    ) -> Result<Self, Error> {
        use tokio::io::AsyncReadExt;

        let mut position = [0u8; 8];
        header.read_exact(&mut position).await?;
        let position = u64::from_be_bytes(position);

        let mut size = [0u8; 8];
        header.read_exact(&mut size).await?;
        let size = u64::from_be_bytes(size);

        Ok(Self(source.section(position, size)))
    }
}
impl<S: FileSource> FileContents<S> {
    pub async fn hash(&self) -> Result<Hash, Error> {
        let mut hasher = TrackingWriter::new(0, HashWriter::new());
        self.serialize_body(&mut hasher, None).await?;
        Ok(hasher.into_inner().finalize())
    }
    pub async fn size(&self) -> Result<u64, Error> {
        self.0.size().await
    }
    pub async fn to_vec(&self) -> Result<Vec<u8>, Error> {
        let mut res = Vec::with_capacity(self.size().await? as usize);
        self.0.copy_to(&mut res).await?;
        Ok(res)
    }
    #[instrument(skip_all)]
    pub async fn serialize_header<W: Sink>(&self, position: u64, w: &mut W) -> Result<u64, Error> {
        use tokio::io::AsyncWriteExt;

        let size = self.0.size().await?;

        w.write_all(&position.to_be_bytes()).await?;
        w.write_all(&size.to_be_bytes()).await?;

        Ok(position)
    }
    #[instrument(skip_all)]
    pub async fn serialize_body<W: Sink>(
        &self,
        w: &mut W,
        verify: Option<Hash>,
    ) -> Result<(), Error> {
        let start = if verify.is_some() {
            Some(w.current_position().await?)
        } else {
            None
        };
        let mut w = VerifyingWriter::new(w, verify);
        self.0.copy_to(&mut w).await?;
        let w = w.verify()?;
        if let Some(start) = start {
            ensure_code!(
                w.current_position().await? - start == self.0.size().await?,
                ErrorKind::Pack,
                "FileSource::copy_to wrote a number of bytes that does not match FileSource::size"
            );
        }
        Ok(())
    }
}
