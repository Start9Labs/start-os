use digest::Update;
use futures::Future;
use tokio::io::AsyncWrite;

use crate::prelude::*;

pub mod blake3;
pub mod merkle_archive;
pub mod request;

pub trait Digestable {
    fn update<D: Update>(&self, digest: &mut D);
}

pub trait Commitment<Resource>: Sized + Digestable {
    fn create(resource: Resource) -> impl Future<Output = Result<Self, Error>> + Send;
    fn copy_to<W: AsyncWrite + Unpin + Send>(
        &self,
        resource: Resource,
        writer: W,
    ) -> impl Future<Output = Result<(), Error>> + Send;
    fn check(&self, resource: Resource) -> impl Future<Output = Result<(), Error>> + Send {
        self.copy_to(resource, tokio::io::sink())
    }
}
