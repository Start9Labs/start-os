use std::path::Path;

use digest::generic_array::GenericArray;
use digest::{Digest, OutputSizeUser};
use sha2::Sha256;

use super::FileSystem;
use crate::prelude::*;

pub struct Label<S: AsRef<str>> {
    label: S,
}
impl<S: AsRef<str>> Label<S> {
    pub fn new(label: S) -> Self {
        Label { label }
    }
}
impl<S: AsRef<str> + Send + Sync> FileSystem for Label<S> {
    fn extra_args(&self) -> impl IntoIterator<Item = impl AsRef<std::ffi::OsStr>> {
        ["-L", self.label.as_ref()]
    }
    fn source(&self) -> Option<impl AsRef<Path>> {
        None::<&Path>
    }
    async fn source_hash(
        &self,
    ) -> Result<GenericArray<u8, <Sha256 as OutputSizeUser>::OutputSize>, Error> {
        let mut sha = Sha256::new();
        sha.update("Label");
        sha.update(self.label.as_ref().as_bytes());
        Ok(sha.finalize())
    }
}
