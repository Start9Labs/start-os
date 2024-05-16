use blake3::Hash;
use serde::{Deserialize, Serialize};
use tokio::io::AsyncWrite;
use ts_rs::TS;

use crate::prelude::*;
use crate::registry::signer::commitment::{Commitment, Digestable};
use crate::s9pk::merkle_archive::hash::VerifyingWriter;
use crate::s9pk::merkle_archive::source::ArchiveSource;
use crate::util::io::{ParallelBlake3Writer, TrackingIO};
use crate::util::serde::Base64;
use crate::CAP_10_MiB;

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct Blake3Commitment {
    pub hash: Base64<[u8; 32]>,
    pub size: u64,
}
impl Digestable for Blake3Commitment {
    fn update<D: sha2::Digest>(&self, digest: &mut D) {
        digest.update(&*self.hash);
        digest.update(&u64::to_be_bytes(self.size));
    }
}
impl<Resource: ArchiveSource> Commitment<Resource> for Blake3Commitment {
    async fn create(resource: &Resource) -> Result<Self, Error> {
        let mut hasher = TrackingIO::new(0, ParallelBlake3Writer::new(CAP_10_MiB));
        resource.copy_all_to(&mut hasher).await?;
        Ok(Self {
            size: hasher.position(),
            hash: Base64(*hasher.into_inner().finalize().await?.as_bytes()),
        })
    }
    async fn copy_to<W: AsyncWrite + Unpin + Send>(
        &self,
        resource: &Resource,
        writer: W,
    ) -> Result<(), Error> {
        let mut hasher =
            VerifyingWriter::new(writer, Some((Hash::from_bytes(*self.hash), self.size)));
        resource.copy_to(0, self.size, &mut hasher).await?;
        hasher.verify().await?;
        Ok(())
    }
}
