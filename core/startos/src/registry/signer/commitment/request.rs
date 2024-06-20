use std::collections::BTreeMap;
use std::time::{SystemTime, UNIX_EPOCH};

use axum::body::Body;
use axum::extract::Request;
use digest::Update;
use futures::TryStreamExt;
use serde::{Deserialize, Serialize};
use tokio::io::AsyncWrite;
use tokio_util::io::StreamReader;
use ts_rs::TS;
use url::Url;

use crate::prelude::*;
use crate::registry::signer::commitment::{Commitment, Digestable};
use crate::s9pk::merkle_archive::hash::VerifyingWriter;
use crate::util::serde::Base64;

#[derive(Clone, Debug, Deserialize, Serialize, HasModel, PartialEq, Eq, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct RequestCommitment {
    #[ts(type = "number")]
    pub timestamp: i64,
    #[ts(type = "number")]
    pub nonce: u64,
    #[ts(type = "number")]
    pub size: u64,
    pub blake3: Base64<[u8; 32]>,
}
impl RequestCommitment {
    pub fn append_query(&self, url: &mut Url) {
        url.query_pairs_mut()
            .append_pair("timestamp", &self.timestamp.to_string())
            .append_pair("nonce", &self.nonce.to_string())
            .append_pair("size", &self.size.to_string())
            .append_pair("blake3", &self.blake3.to_string());
    }
    pub fn from_query(url: &Url) -> Result<Self, Error> {
        let query: BTreeMap<_, _> = url.query_pairs().collect();
        Ok(Self {
            timestamp: query.get("timestamp").or_not_found("timestamp")?.parse()?,
            nonce: query.get("nonce").or_not_found("nonce")?.parse()?,
            size: query.get("size").or_not_found("size")?.parse()?,
            blake3: query.get("blake3").or_not_found("blake3")?.parse()?,
        })
    }
}
impl Digestable for RequestCommitment {
    fn update<D: Update>(&self, digest: &mut D) {
        digest.update(&i64::to_be_bytes(self.timestamp));
        digest.update(&u64::to_be_bytes(self.nonce));
        digest.update(&u64::to_be_bytes(self.size));
        digest.update(&*self.blake3);
    }
}
impl<'a> Commitment<&'a mut Request> for RequestCommitment {
    async fn create(resource: &'a mut Request) -> Result<Self, Error> {
        use http_body_util::BodyExt;

        let body = std::mem::replace(resource.body_mut(), Body::empty())
            .collect()
            .await
            .with_kind(ErrorKind::Network)?
            .to_bytes();
        let res = Self {
            timestamp: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_secs() as i64)
                .unwrap_or_else(|e| e.duration().as_secs() as i64 * -1),
            nonce: rand::random(),
            size: body.len() as u64,
            blake3: Base64(*blake3::hash(&*body).as_bytes()),
        };
        *resource.body_mut() = Body::from(body);
        Ok(res)
    }
    async fn copy_to<W: AsyncWrite + Unpin + Send>(
        &self,
        resource: &'a mut Request,
        writer: W,
    ) -> Result<(), Error> {
        use tokio::io::AsyncReadExt;

        let mut body = StreamReader::new(
            std::mem::replace(resource.body_mut(), Body::empty())
                .into_data_stream()
                .map_err(std::io::Error::other),
        )
        .take(self.size);

        let mut writer = VerifyingWriter::new(
            writer,
            Some((blake3::Hash::from_bytes(*self.blake3), self.size)),
        );
        tokio::io::copy(&mut body, &mut writer).await?;
        writer.verify().await?;

        Ok(())
    }
}
