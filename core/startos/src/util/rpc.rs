use std::path::Path;

use clap::Parser;
use rpc_toolkit::{from_fn_async, Context, ParentHandler};
use serde::{Deserialize, Serialize};
use url::Url;

use crate::context::CliContext;
use crate::prelude::*;
use crate::s9pk::merkle_archive::source::http::HttpSource;
use crate::s9pk::merkle_archive::source::multi_cursor_file::MultiCursorFile;
use crate::s9pk::merkle_archive::source::ArchiveSource;
use crate::util::io::{open_file, ParallelBlake3Writer};
use crate::util::serde::Base16;
use crate::util::{Apply, PathOrUrl};
use crate::CAP_10_MiB;

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
    async fn b3sum_source<S: ArchiveSource>(source: S) -> Result<Base16<[u8; 32]>, Error> {
        let mut hasher = ParallelBlake3Writer::new(CAP_10_MiB);
        source.copy_all_to(&mut hasher).await?;
        hasher.finalize().await.map(|h| *h.as_bytes()).map(Base16)
    }
    async fn b3sum_file(
        path: impl AsRef<Path>,
        allow_mmap: bool,
    ) -> Result<Base16<[u8; 32]>, Error> {
        let file = MultiCursorFile::from(open_file(path).await?);
        if allow_mmap {
            return file.blake3_mmap().await.map(|h| *h.as_bytes()).map(Base16);
        }
        b3sum_source(file).await
    }
    match file.parse::<PathOrUrl>()? {
        PathOrUrl::Path(path) => b3sum_file(path, allow_mmap).await,
        PathOrUrl::Url(url) => {
            if url.scheme() == "http" || url.scheme() == "https" {
                HttpSource::new(ctx.client.clone(), url)
                    .await?
                    .apply(b3sum_source)
                    .await
            } else {
                Err(Error::new(
                    eyre!("unknown scheme: {}", url.scheme()),
                    ErrorKind::InvalidRequest,
                ))
            }
        }
    }
}
