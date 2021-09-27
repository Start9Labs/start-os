use anyhow::anyhow;
use clap::ArgMatches;
use digest::Digest;
use futures::Stream;
use rpc_toolkit::command;
use serde_json::Value;
use sha2::Sha256;
use tokio::io::AsyncWriteExt;
use tokio::pin;
use tokio_stream::StreamExt;

use crate::context::RpcContext;
use crate::update::latest_information::LatestInformation;
use crate::{Error, ErrorKind, ResultExt};

const URL: &str = "https://beta-registry-0-3.start9labs.com/eos/latest";
const HEADER_KEY: &str = "CHECKSUM";
mod latest_information;

pub fn display_properties(_: (), _: &ArgMatches<'_>) {
    println!("Test");
}
#[command(display(display_properties))]
pub async fn update_system(#[context] ctx: RpcContext) -> Result<(), Error> {
    if let None = maybe_do_update(ctx).await? {
        return Ok(());
    }
    todo!()
}

pub async fn maybe_do_update(mut ctx: RpcContext) -> Result<Option<()>, Error> {
    let mut db = ctx.db.handle();
    let latest_version = reqwest::get(URL)
        .await
        .with_kind(ErrorKind::Network)?
        .json::<LatestInformation>()
        .await
        .with_kind(ErrorKind::Network)?
        .version;
    let current_version = crate::db::DatabaseModel::new()
        .server_info()
        .version()
        .get_mut(&mut db)
        .await?;
    if &latest_version > &current_version {
        let file_name = "/tmp/test";
        download_file(file_name).await?;
        swap(&mut ctx).await?;
        Ok(Some(()))
    } else {
        Ok(None)
    }
}

pub async fn download_file(file_name: &str) -> Result<(), Error> {
    let download_request = reqwest::get(URL).await.with_kind(ErrorKind::Network)?;
    let hash_from_header: String = download_request
        .headers()
        .get(HEADER_KEY)
        .ok_or_else(|| Error::new(anyhow!("No {} in headers", HEADER_KEY), ErrorKind::Network))?
        .to_str()
        .with_kind(ErrorKind::InvalidRequest)?
        .to_owned();
    let stream_download = download_request.bytes_stream();
    let file_sum = write_stream_to_file(stream_download, file_name).await?;
    check_download(&hash_from_header, file_sum).await?;
    Ok(())
}

async fn write_stream_to_file(
    stream_download: impl Stream<Item = Result<rpc_toolkit::hyper::body::Bytes, reqwest::Error>>,
    file: &str,
) -> Result<Vec<u8>, Error> {
    let mut file = tokio::fs::File::create(file)
        .await
        .with_kind(ErrorKind::Filesystem)?;
    let mut hasher = Sha256::new();
    pin!(stream_download);
    while let Some(Ok(item)) = stream_download.next().await {
        file.write(&item).await.with_kind(ErrorKind::Filesystem)?;
        hasher.update(item);
    }
    Ok(hasher.finalize().to_vec())
}

pub async fn check_download(hash_from_header: &str, file_digest: Vec<u8>) -> Result<(), Error> {
    if hex::decode(hash_from_header).with_kind(ErrorKind::Network)? != file_digest {
        return Err(Error::new(
            anyhow!("Hash sum does not match source"),
            ErrorKind::Network,
        ));
    }
    Ok(())
}

pub async fn swap(ctx: &mut RpcContext) -> Result<Value, Error> {
    // disk/util add setLabel
    todo!("Do swap");
    todo!("Let system know that we need a reboot or something")
}
