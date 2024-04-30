use std::collections::HashMap;

use imbl_value::InternedString;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha512};
use ts_rs::TS;
use url::Url;

use crate::prelude::*;
use crate::s9pk::merkle_archive::source::multi_cursor_file::MultiCursorFile;
use crate::s9pk::merkle_archive::source::ArchiveSource;
use crate::util::serde::{Base64, Pem};

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct SignerInfo {
    pub name: String,
    pub contact: Vec<ContactInfo>,
    pub keys: Vec<SignerKey>,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
#[serde(tag = "alg", content = "pubkey")]
pub enum SignerKey {
    Ed25519(Pem<ed25519_dalek::VerifyingKey>),
}
impl SignerKey {
    pub fn verifier(&self) -> Verifier {
        match self {
            Self::Ed25519(k) => Verifier::Ed25519(*k, Sha512::new()),
        }
    }
    pub fn verify_message(
        &self,
        message: &[u8],
        signature: &[u8],
        context: &str,
    ) -> Result<(), Error> {
        let mut v = self.verifier();
        v.update(message);
        v.verify(signature, context)
    }
}

pub enum Verifier {
    Ed25519(Pem<ed25519_dalek::VerifyingKey>, Sha512),
}
impl Verifier {
    pub fn update(&mut self, data: &[u8]) {
        match self {
            Self::Ed25519(_, h) => h.update(data),
        }
    }
    pub fn verify(self, signature: &[u8], context: &str) -> Result<(), Error> {
        match self {
            Self::Ed25519(k, h) => k.verify_prehashed_strict(
                h,
                Some(context.as_bytes()),
                &ed25519_dalek::Signature::from_slice(signature)?,
            )?,
        }
        Ok(())
    }
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
// TODO: better types
pub enum ContactInfo {
    Email(String),
    Matrix(String),
    Website(#[ts(type = "string")] Url),
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct SignatureInfo {
    pub blake3_ed255i9: Option<Blake3Ed2551SignatureInfo>,
}
impl SignatureInfo {
    pub fn new() -> Self {
        Self {
            blake3_ed255i9: None,
        }
    }
    pub fn add_sig(&mut self, signature: &Signature, context: &str) -> Result<(), Error> {
        match signature {
            Signature::Blake3Ed25519(s) => {
                if self
                    .blake3_ed255i9
                    .as_ref()
                    .map_or(true, |info| info.hash == s.hash)
                {
                    let new = if let Some(mut info) = self.blake3_ed255i9.take() {
                        info.signatures.insert(s.pubkey, s.signature);
                        info
                    } else {
                        s.info(context)
                    };
                    self.blake3_ed255i9 = Some(new);
                    Ok(())
                } else {
                    Err(Error::new(
                        eyre!("hash sum mismatch"),
                        ErrorKind::InvalidSignature,
                    ))
                }
            }
        }
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct Blake3Ed2551SignatureInfo {
    #[ts(type = "string")]
    pub context: InternedString,
    pub hash: Base64<[u8; 32]>,
    pub size: u64,
    pub signatures: HashMap<Pem<ed25519_dalek::VerifyingKey>, Base64<[u8; 64]>>,
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum Signature {
    Blake3Ed25519(Blake3Ed25519Signature),
}
impl Signature {
    pub fn validate(&self, context: &str) -> Result<(), Error> {
        match self {
            Self::Blake3Ed25519(a) => a.validate(context),
        }
    }
    pub fn signer(&self) -> SignerKey {
        match self {
            Self::Blake3Ed25519(s) => SignerKey::Ed25519(s.pubkey.clone()),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct Blake3Ed25519Signature {
    pub hash: Base64<[u8; 32]>,
    pub size: u64,
    pub pubkey: Pem<ed25519_dalek::VerifyingKey>,
    // ed25519-sig(sha512(blake3(file) + len_u64_be(file)))
    pub signature: Base64<[u8; 64]>,
}
impl Blake3Ed25519Signature {
    pub async fn sign_file(
        key: &ed25519_dalek::SigningKey,
        file: &MultiCursorFile,
        context: &str,
    ) -> Result<Self, Error> {
        let size = file
            .size()
            .await
            .ok_or_else(|| Error::new(eyre!("failed to get file size"), ErrorKind::Filesystem))?;
        let hash = file.blake3_mmap().await?;
        let signature = key.sign_prehashed(
            Sha512::new_with_prefix(hash.as_bytes()).chain_update(u64::to_be_bytes(size)),
            Some(context.as_bytes()),
        )?;
        Ok(Self {
            hash: Base64(*hash.as_bytes()),
            size,
            pubkey: Pem::new(key.verifying_key()),
            signature: Base64(signature.to_bytes()),
        })
    }

    pub fn validate(&self, context: &str) -> Result<(), Error> {
        let sig = ed25519_dalek::Signature::from_bytes(&*self.signature);
        self.pubkey.verify_prehashed_strict(
            Sha512::new_with_prefix(*self.hash).chain_update(u64::to_be_bytes(self.size)),
            Some(context.as_bytes()),
            &sig,
        )?;
        Ok(())
    }

    pub async fn check_file(&self, file: &MultiCursorFile) -> Result<(), Error> {
        let size = file
            .size()
            .await
            .ok_or_else(|| Error::new(eyre!("failed to get file size"), ErrorKind::Filesystem))?;
        if self.size != size {
            return Err(Error::new(
                eyre!("incorrect file size: expected {} got {}", self.size, size),
                ErrorKind::InvalidSignature,
            ));
        }
        let hash = file.blake3_mmap().await?;
        if &*self.hash != hash.as_bytes() {
            return Err(Error::new(
                eyre!("hash sum mismatch"),
                ErrorKind::InvalidSignature,
            ));
        }
        Ok(())
    }

    pub fn info(&self, context: &str) -> Blake3Ed2551SignatureInfo {
        Blake3Ed2551SignatureInfo {
            context: InternedString::intern(context),
            hash: self.hash,
            size: self.size,
            signatures: [(self.pubkey, self.signature)].into_iter().collect(),
        }
    }
}
