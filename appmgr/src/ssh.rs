use std::path::Path;

use rpc_toolkit::command;
use sqlx::{query, Pool, Sqlite};
use tokio::sync;

use crate::util::display_none;
use crate::{context::EitherContext, Error};

#[derive(serde::Deserialize, serde::Serialize)]
pub struct PubKey(
    #[serde(serialize_with = "crate::util::serialize_display")]
    #[serde(deserialize_with = "crate::util::deserialize_from_str")]
    openssh_keys::PublicKey,
);
impl std::str::FromStr for PubKey {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse().map(|pk| PubKey(pk)).map_err(|e| Error {
            source: e.into(),
            kind: crate::ErrorKind::ParseSshKey,
            revision: None,
        })
    }
}

#[command(subcommands(add, remove, list,))]
pub fn ssh(#[context] ctx: EitherContext) -> Result<EitherContext, Error> {
    Ok(ctx)
}

#[command(display(display_none))]
pub async fn add(#[context] ctx: EitherContext, #[arg] key: PubKey) -> Result<String, Error> {
    let pool = &ctx.as_rpc().unwrap().secret_store;
    // check fingerprint for duplicates
    let fp = key.0.fingerprint_md5();
    if sqlx::query!("SELECT * FROM ssh_keys WHERE fingerprint = ?", fp)
        .fetch_optional(pool)
        .await?
        .is_none()
    {
        // if no duplicates, insert into DB
        let raw_key = format!("{}", key.0);
        sqlx::query!(
            "INSERT INTO ssh_keys (fingerprint, openssh_pubkey, created_at) VALUES (?, ?, datetime('now'))"
        , fp, raw_key).execute(pool).await?;
        // insert into live key file, for now we actually do a wholesale replacement of the keys file, for maximum
        // consistency
        sync_keys_from_db(pool, todo!()).await?;
    }
    // return fingerprint
    Ok(fp)
}
#[command(display(display_none))]
pub async fn remove(
    #[context] ctx: EitherContext,
    #[arg] fingerprint: String,
) -> Result<(), Error> {
    let pool = &ctx.as_rpc().unwrap().secret_store;
    // check if fingerprint is in DB
    // if in DB, remove it from DB
    let n = sqlx::query!("DELETE FROM ssh_keys WHERE fingerprint = ?", fingerprint)
        .execute(pool)
        .await?
        .rows_affected();
    // if not in DB, Err404
    if n == 0 {
        Err(Error {
            source: anyhow::anyhow!("SSH Key Not Found"),
            kind: crate::error::ErrorKind::NotFound,
            revision: None,
        })
    } else {
        // AND overlay key file
        sync_keys_from_db(pool, todo!()).await?;
        Ok(())
    }
}

#[command(display(display_none))]
pub fn list(#[context] ctx: EitherContext) -> Result<Vec<String>, Error> {
    // list keys in DB and return them
    todo!()
}

pub async fn sync_keys_from_db(pool: &Pool<Sqlite>, dest: &Path) -> Result<(), Error> {
    let keys = sqlx::query!("SELECT openssh_pubkey FROM ssh_keys")
        .fetch_all(pool)
        .await?;
    let contents: String = keys
        .into_iter()
        .map(|k| format!("{}\n", k.openssh_pubkey))
        .collect();
    std::fs::write(dest, contents).map_err(|e| e.into())
}
