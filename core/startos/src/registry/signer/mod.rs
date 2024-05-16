use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::str::FromStr;

use clap::builder::ValueParserFactory;
use imbl_value::InternedString;
use reqwest::Client;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha512};
use tokio::io::AsyncWrite;
use ts_rs::TS;
use url::Url;

use crate::prelude::*;
use crate::registry::signer::sign::ed25519::Ed25519;
use crate::registry::signer::sign::{AnyVerifyingKey, SignatureScheme};
use crate::s9pk::merkle_archive::source::http::HttpSource;
use crate::s9pk::merkle_archive::source::multi_cursor_file::MultiCursorFile;
use crate::s9pk::merkle_archive::source::{ArchiveSource, FileSource};
use crate::util::clap::FromStrParser;
use crate::util::serde::{Base64, Pem};

pub mod commitment;
pub mod sign;

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct SignerInfo {
    pub name: String,
    pub contact: Vec<ContactInfo>,
    pub keys: HashSet<AnyVerifyingKey>,
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

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
// TODO: better types
pub enum ContactInfo {
    Email(String),
    Matrix(String),
    Website(#[ts(type = "string")] Url),
}
impl std::fmt::Display for ContactInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Email(e) => write!(f, "mailto:{e}"),
            Self::Matrix(m) => write!(f, "https://matrix.to/#/{m}"),
            Self::Website(w) => write!(f, "{w}"),
        }
    }
}
impl FromStr for ContactInfo {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(if let Some(s) = s.strip_prefix("mailto:") {
            Self::Email(s.to_owned())
        } else if let Some(s) = s.strip_prefix("https://matrix.to/#/") {
            Self::Matrix(s.to_owned())
        } else {
            Self::Website(s.parse()?)
        })
    }
}
impl ValueParserFactory for ContactInfo {
    type Parser = FromStrParser<Self>;
    fn value_parser() -> Self::Parser {
        Self::Parser::new()
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "kebab-case")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct SignatureInfo {
    #[ts(type = "string")]
    pub context: InternedString,
    pub blake3_ed25519: Option<Blake3Ed25519SignatureInfo>,
    // pub merkle_archive_ed25519: Option<MerkleArchiveEd25519SignatureInfo>,
}
impl SignatureInfo {
    pub fn new(context: &str) -> Self {
        Self {
            context: context.into(),
            blake3_ed25519: None,
        }
    }
    pub fn validate(&self, accept: AcceptSigners) -> Result<FileValidator, Error> {
        FileValidator::from_signatures(self.signatures(), accept, &self.context)
    }
    pub fn all_signers(&self) -> AcceptSigners {
        AcceptSigners::All(
            self.signatures()
                .map(|s| AcceptSigners::Signer(s.signer()))
                .collect(),
        )
        .flatten()
    }
    pub fn signatures(&self) -> impl Iterator<Item = Signature> + '_ {
        self.blake3_ed25519.iter().flat_map(|info| {
            info.signatures
                .iter()
                .map(|(k, s)| (k.clone(), *s))
                .map(|(pubkey, signature)| {
                    Signature::Blake3Ed25519(Blake3Ed25519Signature {
                        hash: info.hash,
                        size: info.size,
                        pubkey,
                        signature,
                    })
                })
        })
    }
    pub fn add_sig(&mut self, signature: &Signature) -> Result<(), Error> {
        signature.validate(&self.context)?;
        match signature {
            Signature::Blake3Ed25519(s) => {
                if self
                    .blake3_ed25519
                    .as_ref()
                    .map_or(true, |info| info.hash == s.hash)
                {
                    let new = if let Some(mut info) = self.blake3_ed25519.take() {
                        info.signatures.insert(s.pubkey, s.signature);
                        info
                    } else {
                        s.info()
                    };
                    self.blake3_ed25519 = Some(new);
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

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum AcceptSigners {
    #[serde(skip)]
    Accepted(Signature),
    Signer(AnyVerifyingKey),
    Any(Vec<AcceptSigners>),
    All(Vec<AcceptSigners>),
}
impl AcceptSigners {
    const fn null() -> Self {
        Self::Any(Vec::new())
    }
    pub fn flatten(self) -> Self {
        match self {
            Self::Any(mut s) | Self::All(mut s) if s.len() == 1 => s.swap_remove(0).flatten(),
            s => s,
        }
    }
    pub fn accepted(&self) -> bool {
        match self {
            Self::Accepted(_) => true,
            Self::All(s) => s.iter().all(|s| s.accepted()),
            _ => false,
        }
    }
    pub fn try_accept(
        self,
        context: &str,
    ) -> Box<dyn Iterator<Item = Result<Signature, Error>> + Send + Sync + '_> {
        match self {
            Self::Accepted(s) => Box::new(std::iter::once(s).map(|s| {
                s.validate(context)?;
                Ok(s)
            })),
            Self::All(s) => Box::new(s.into_iter().flat_map(|s| s.try_accept(context))),
            _ => Box::new(std::iter::once(Err(Error::new(
                eyre!("signer(s) not accepted"),
                ErrorKind::InvalidSignature,
            )))),
        }
    }
    pub fn process_signature(&mut self, sig: &Signature) {
        let new = match std::mem::replace(self, Self::null()) {
            Self::Accepted(s) => Self::Accepted(s),
            Self::Signer(s) => {
                if s == sig.signer() {
                    Self::Accepted(sig.clone())
                } else {
                    Self::Signer(s)
                }
            }
            Self::All(mut s) => {
                s.iter_mut().for_each(|s| s.process_signature(sig));

                Self::All(s)
            }
            Self::Any(mut s) => {
                if let Some(s) = s
                    .iter_mut()
                    .map(|s| {
                        s.process_signature(sig);
                        s
                    })
                    .filter(|s| s.accepted())
                    .next()
                {
                    std::mem::replace(s, Self::null())
                } else {
                    Self::Any(s)
                }
            }
        };
        *self = new;
    }
}

#[must_use]
pub struct FileValidator {
    blake3: Option<blake3::Hash>,
    size: Option<u64>,
}
impl FileValidator {
    fn add_blake3(&mut self, hash: [u8; 32], size: u64) -> Result<(), Error> {
        if let Some(h) = self.blake3 {
            ensure_code!(h == hash, ErrorKind::InvalidSignature, "hash sum mismatch");
        }
        self.blake3 = Some(blake3::Hash::from_bytes(hash));
        if let Some(s) = self.size {
            ensure_code!(s == size, ErrorKind::InvalidSignature, "file size mismatch");
        }
        self.size = Some(size);
        Ok(())
    }
    pub fn blake3(&self) -> Result<blake3::Hash, Error> {
        if let Some(hash) = self.blake3 {
            Ok(hash)
        } else {
            Err(Error::new(
                eyre!("no BLAKE3 signatures found"),
                ErrorKind::InvalidSignature,
            ))
        }
    }
    pub fn size(&self) -> Result<u64, Error> {
        if let Some(size) = self.size {
            Ok(size)
        } else {
            Err(Error::new(
                eyre!("no signatures found"),
                ErrorKind::InvalidSignature,
            ))
        }
    }
    pub fn from_signatures(
        signatures: impl IntoIterator<Item = Signature>,
        mut accept: AcceptSigners,
        context: &str,
    ) -> Result<Self, Error> {
        let mut res = Self {
            blake3: None,
            size: None,
        };
        for signature in signatures {
            accept.process_signature(&signature);
        }
        for signature in accept.try_accept(context) {
            match signature? {
                Signature::Blake3Ed25519(s) => res.add_blake3(*s.hash, s.size)?,
            }
        }

        Ok(res)
    }
    pub async fn download(
        &self,
        url: Url,
        client: Client,
        dst: &mut (impl AsyncWrite + Unpin + Send + ?Sized),
    ) -> Result<(), Error> {
        let src = HttpSource::new(client, url).await?;
        let (Some(hash), Some(size)) = (self.blake3, self.size) else {
            return Err(Error::new(
                eyre!("no BLAKE3 signatures found"),
                ErrorKind::InvalidSignature,
            ));
        };
        src.section(0, size)
            .copy_verify(dst, Some((hash, size)))
            .await?;

        Ok(())
    }
    pub async fn validate_file(&self, file: &MultiCursorFile) -> Result<(), Error> {
        ensure_code!(
            file.size().await == Some(self.size()?),
            ErrorKind::InvalidSignature,
            "file size mismatch"
        );
        ensure_code!(
            file.blake3_mmap().await? == self.blake3()?,
            ErrorKind::InvalidSignature,
            "hash sum mismatch"
        );
        Ok(())
    }
}

#[derive(Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct Blake3Ed25519SignatureInfo {
    pub hash: Base64<[u8; 32]>,
    pub size: u64,
    pub signatures: HashMap<Pem<ed25519_dalek::VerifyingKey>, Base64<[u8; 64]>>,
}
impl Blake3Ed25519SignatureInfo {
    pub fn validate(&self, context: &str) -> Result<Vec<Pem<ed25519_dalek::VerifyingKey>>, Error> {
        self.signatures
            .iter()
            .map(|(k, s)| {
                let sig = Blake3Ed25519Signature {
                    hash: self.hash,
                    size: self.size,
                    pubkey: k.clone(),
                    signature: *s,
                };
                sig.validate(context)?;
                Ok(sig.pubkey)
            })
            .collect()
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum Signature {
    Blake3Ed25519(Blake3Ed25519Signature),
    MerkleArchiveEd25519(MerkleArchiveEd25519Signature),
}
impl Signature {
    pub fn validate(&self, context: &str) -> Result<(), Error> {
        match self {
            Self::Blake3Ed25519(a) => a.validate(context),
        }
    }
    pub fn signer(&self) -> AnyVerifyingKey {
        match self {
            Self::Blake3Ed25519(s) => AnyVerifyingKey::Ed25519(s.pubkey.clone()),
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

    pub fn info(&self) -> Blake3Ed25519SignatureInfo {
        Blake3Ed25519SignatureInfo {
            hash: self.hash,
            size: self.size,
            signatures: [(self.pubkey, self.signature)].into_iter().collect(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct MerkleArchiveEd25519Signature {
    pub root_sighash: Base64<[u8; 32]>,
    pub root_maxsize: u64,
    pub pubkey: Pem<ed25519_dalek::VerifyingKey>,
    // ed25519-sig(sha512(root_sighash + u64_be(root_maxsize)))
    pub signature: Base64<[u8; 64]>,
}
