use ed25519_dalek::{Signature, SigningKey, VerifyingKey};
use sha2::Sha512;

use crate::prelude::*;
use crate::registry::signer::sign::SignatureScheme;

pub struct Ed25519;
impl SignatureScheme for Ed25519 {
    type SigningKey = SigningKey;
    type VerifyingKey = VerifyingKey;
    type Signature = Signature;
    type Digest = Sha512;
    fn new_digest(&self) -> Self::Digest {
        <Self::Digest as digest::Digest>::new()
    }
    fn sign(
        &self,
        key: &Self::SigningKey,
        digest: Self::Digest,
        context: &str,
    ) -> Result<Self::Signature, Error> {
        Ok(key.sign_prehashed(digest, Some(context.as_bytes()))?)
    }
    fn verify(
        &self,
        key: &Self::VerifyingKey,
        digest: Self::Digest,
        context: &str,
        signature: &Self::Signature,
    ) -> Result<(), Error> {
        key.verify_prehashed_strict(digest, Some(context.as_bytes()), signature)?;
        Ok(())
    }
}
