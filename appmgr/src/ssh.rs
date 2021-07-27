use std::path::Path;

use rpc_toolkit::command;
use sqlx::{query, Pool, Sqlite};
use tokio::sync;

use crate::util::display_none;
use crate::{context::EitherContext, Error};

static SSH_AUTHORIZED_KEYS_FILE: &str = "/root/.ssh/authorized_keys";

#[derive(serde::Deserialize, serde::Serialize)]
pub struct PubKey(
    #[serde(serialize_with = "crate::util::serialize_display")]
    #[serde(deserialize_with = "crate::util::deserialize_from_str")]
    openssh_keys::PublicKey,
);

#[derive(serde::Serialize, serde::Deserialize)]
pub struct SshKeyResponse {
    pub alg: String,
    pub hash: String,
    pub hostname: String,
    #[serde(rename = "camelCase")]
    pub created_at: String,
}
impl std::fmt::Display for SshKeyResponse {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{} {} {} {}",
            self.created_at, self.alg, self.hash, self.hostname
        )
    }
}
fn display_all_ssh_keys(all: &Vec<SshKeyResponse>, _f: &clap::ArgMatches<'_>) -> () {
    for s in all {
        println!("{}", s)
    }
}

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
        sync_keys_from_db(pool, Path::new(SSH_AUTHORIZED_KEYS_FILE)).await?;
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
        sync_keys_from_db(pool, Path::new(SSH_AUTHORIZED_KEYS_FILE)).await?;
        Ok(())
    }
}

#[command(display(display_all_ssh_keys))]
pub async fn list(#[context] ctx: EitherContext) -> Result<Vec<SshKeyResponse>, Error> {
    let pool = &ctx.as_rpc().unwrap().secret_store;
    // list keys in DB and return them
    let entries = sqlx::query!("SELECT fingerprint, openssh_pubkey, created_at FROM ssh_keys")
        .fetch_all(pool)
        .await?;
    Ok(entries
        .into_iter()
        .map(|r| {
            let k = PubKey(r.openssh_pubkey.parse().unwrap()).0;
            let alg = k.keytype().to_owned();
            let hash = k.fingerprint();
            let hostname = k.comment.unwrap_or("".to_owned());
            let created_at = r.created_at;
            SshKeyResponse {
                alg,
                hash,
                hostname,
                created_at,
            }
        })
        .collect())
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
