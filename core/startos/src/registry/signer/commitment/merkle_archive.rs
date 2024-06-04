use digest::Update;
use serde::{Deserialize, Serialize};
use tokio::io::AsyncWrite;
use ts_rs::TS;

use crate::prelude::*;
use crate::registry::signer::commitment::{Commitment, Digestable};
use crate::s9pk::merkle_archive::source::FileSource;
use crate::s9pk::merkle_archive::MerkleArchive;
use crate::s9pk::S9pk;
use crate::util::io::TrackingIO;
use crate::util::serde::Base64;

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct MerkleArchiveCommitment {
    pub root_sighash: Base64<[u8; 32]>,
    #[ts(type = "number")]
    pub root_maxsize: u64,
}
impl Digestable for MerkleArchiveCommitment {
    fn update<D: Update>(&self, digest: &mut D) {
        digest.update(&*self.root_sighash);
        digest.update(&u64::to_be_bytes(self.root_maxsize));
    }
}
impl<'a, S: FileSource + Clone> Commitment<&'a MerkleArchive<S>> for MerkleArchiveCommitment {
    async fn create(resource: &'a MerkleArchive<S>) -> Result<Self, Error> {
        resource.commitment().await
    }
    async fn check(&self, resource: &'a MerkleArchive<S>) -> Result<(), Error> {
        let MerkleArchiveCommitment {
            root_sighash,
            root_maxsize,
        } = resource.commitment().await?;
        if root_sighash != self.root_sighash {
            return Err(Error::new(
                eyre!("merkle root mismatch"),
                ErrorKind::InvalidSignature,
            ));
        }
        if root_maxsize > self.root_maxsize {
            return Err(Error::new(
                eyre!("merkle root directory max size too large"),
                ErrorKind::InvalidSignature,
            ));
        }
        Ok(())
    }
    async fn copy_to<W: AsyncWrite + Unpin + Send>(
        &self,
        resource: &'a MerkleArchive<S>,
        writer: W,
    ) -> Result<(), Error> {
        self.check(resource).await?;
        resource
            .serialize(&mut TrackingIO::new(0, writer), true)
            .await
    }
}

impl<'a, S: FileSource + Clone> Commitment<&'a S9pk<S>> for MerkleArchiveCommitment {
    async fn create(resource: &'a S9pk<S>) -> Result<Self, Error> {
        resource.as_archive().commitment().await
    }
    async fn check(&self, resource: &'a S9pk<S>) -> Result<(), Error> {
        let MerkleArchiveCommitment {
            root_sighash,
            root_maxsize,
        } = resource.as_archive().commitment().await?;
        if root_sighash != self.root_sighash {
            return Err(Error::new(
                eyre!("merkle root mismatch"),
                ErrorKind::InvalidSignature,
            ));
        }
        if root_maxsize > self.root_maxsize {
            return Err(Error::new(
                eyre!("merkle root directory max size too large"),
                ErrorKind::InvalidSignature,
            ));
        }
        Ok(())
    }
    async fn copy_to<W: AsyncWrite + Unpin + Send>(
        &self,
        resource: &'a S9pk<S>,
        writer: W,
    ) -> Result<(), Error> {
        self.check(resource).await?;
        resource
            .clone()
            .serialize(&mut TrackingIO::new(0, writer), true)
            .await
    }
}
