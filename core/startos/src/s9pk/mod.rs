pub mod merkle_archive;
pub mod rpc;
pub mod v1;
pub mod v2;

use std::io::SeekFrom;
use std::path::Path;

use tokio::fs::File;
use tokio::io::{AsyncReadExt, AsyncSeekExt};
pub use v2::{manifest, S9pk};

use crate::context::CliContext;
use crate::prelude::*;
use crate::s9pk::v1::reader::S9pkReader;
use crate::s9pk::v2::compat::MAGIC_AND_VERSION;

pub async fn load(ctx: &CliContext, path: impl AsRef<Path>) -> Result<File, Error> {
    // TODO: return s9pk
    const MAGIC_LEN: usize = MAGIC_AND_VERSION.len();
    let mut magic = [0_u8; MAGIC_LEN];
    let mut file = tokio::fs::File::open(&path).await?;
    file.read_exact(&mut magic).await?;
    file.seek(SeekFrom::Start(0)).await?;
    if magic == v2::compat::MAGIC_AND_VERSION {
        tracing::info!("Converting package to v2 s9pk");
        let new_path = path.as_ref().with_extension("compat.s9pk");
        S9pk::from_v1(
            S9pkReader::from_reader(file, true).await?,
            &new_path,
            ctx.developer_key()?.clone(),
        )
        .await?;
        tokio::fs::rename(&new_path, &path).await?;
        file = tokio::fs::File::open(&path).await?;
        tracing::info!("Converted s9pk successfully");
    }
    Ok(file)
}
