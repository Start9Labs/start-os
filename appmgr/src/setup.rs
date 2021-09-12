use std::path::PathBuf;

use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use torut::onion::TorSecretKeyV3;

use crate::context::SetupContext;
use crate::disk::disk;
use crate::disk::main::DEFAULT_PASSWORD;
use crate::{Error, ResultExt};

#[command(subcommands(status, disk))]
pub fn setup() -> Result<(), Error> {
    Ok(())
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct StatusRes {
    is_recovering: bool,
    tor_address: Option<String>,
}

#[command(rpc_only)]
pub fn status() -> Result<StatusRes, Error> {
    // TODO
    Ok(StatusRes {
        is_recovering: false,
        tor_address: None,
    })
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct SetupResult {
    tor_address: String,
}

#[command(rpc_only)]
pub async fn execute(
    #[context] ctx: SetupContext,
    #[arg(rename = "embassy-logicalname")] embassy_logicalname: PathBuf,
    #[arg(rename = "embassy-password")] embassy_password: String,
) -> Result<SetupResult, Error> {
    let guid =
        crate::disk::main::create(&ctx.zfs_pool_name, [embassy_logicalname], DEFAULT_PASSWORD)
            .await?;
    tokio::fs::write("/embassy-os/disk.guid", &guid)
        .await
        .with_ctx(|_| (crate::ErrorKind::Filesystem, "/embassy-os/disk.guid"))?;
    crate::disk::main::load(&guid, &ctx.zfs_pool_name, &ctx.datadir, DEFAULT_PASSWORD).await?;
    let password = argon2::hash_encoded(
        embassy_password.as_bytes(),
        &rand::random::<[u8; 16]>()[..],
        &argon2::Config::default(),
    )
    .with_kind(crate::ErrorKind::PasswordHashGeneration)?;
    let tor_key = TorSecretKeyV3::generate();
    let key_vec = tor_key.as_bytes().to_vec();
    let sqlite_pool = ctx.secret_store().await?;
    sqlx::query!(
        "INSERT OR REPLACE INTO account (id, password, tor_key) VALUES (?, ?, ?)",
        0,
        password,
        key_vec,
    )
    .execute(&mut sqlite_pool.acquire().await?)
    .await?;

    Ok(SetupResult {
        tor_address: tor_key.public().get_onion_address().to_string(),
    })
}
