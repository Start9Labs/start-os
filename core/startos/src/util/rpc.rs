use clap::Parser;
use rpc_toolkit::{from_fn_async, Context, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::fs::File;
use url::Url;

use crate::context::CliContext;
use crate::prelude::*;
use crate::s9pk::merkle_archive::source::http::HttpSource;
use crate::s9pk::merkle_archive::source::multi_cursor_file::MultiCursorFile;
use crate::s9pk::merkle_archive::source::{ArchiveSource, DynFileSource, FileSource};
use crate::util::io::ParallelBlake3Writer;
use crate::util::serde::Base16;

pub fn util<C: Context>() -> ParentHandler<C> {
    ParentHandler::new().subcommand("b3sum", from_fn_async(b3sum))
}

#[derive(Debug, Deserialize, Serialize, Parser)]
pub struct B3sumParams {
    #[arg(long = "no-mmap", action = clap::ArgAction::SetFalse)]
    allow_mmap: bool,
    file: String,
}

pub async fn b3sum(
    ctx: CliContext,
    B3sumParams { file, allow_mmap }: B3sumParams,
) -> Result<Base16<[u8; 32]>, Error> {
    let source = if let Ok(url) = file.parse::<Url>() {
        if url.scheme() == "file" {
            let file = MultiCursorFile::from(File::open(url.path()).await?);
            if allow_mmap {
                return file.blake3_mmap().await.map(|h| *h.as_bytes()).map(Base16);
            }
            DynFileSource::new(file.section(
                0,
                file.size().await.ok_or_else(|| {
                    Error::new(eyre!("failed to get file size"), ErrorKind::Filesystem)
                })?,
            ))
        } else if url.scheme() == "http" || url.scheme() == "https" {
            let file = HttpSource::new(ctx.client.clone(), url).await?;
            DynFileSource::new(file.section(
                0,
                file.size().await.ok_or_else(|| {
                    Error::new(eyre!("failed to get file size"), ErrorKind::Filesystem)
                })?,
            ))
        } else {
            return Err(Error::new(
                eyre!("unknown scheme: {}", url.scheme()),
                ErrorKind::InvalidRequest,
            ));
        }
    } else {
        let file = MultiCursorFile::from(File::open(file).await?);
        if allow_mmap {
            return file.blake3_mmap().await.map(|h| *h.as_bytes()).map(Base16);
        }
        DynFileSource::new(file.section(
            0,
            file.size().await.ok_or_else(|| {
                Error::new(eyre!("failed to get file size"), ErrorKind::Filesystem)
            })?,
        ))
    };
    let mut hasher = ParallelBlake3Writer::new(crate::s9pk::merkle_archive::hash::BUFFER_CAPACITY);
    source.copy(&mut hasher).await?;
    hasher.finalize().await.map(|h| *h.as_bytes()).map(Base16)
}
