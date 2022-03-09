use std::path::Path;

use chrono::Utc;
use clap::ArgMatches;
use color_eyre::eyre::eyre;
use rpc_toolkit::command;
use sqlx::{Pool, Sqlite};
use tracing::instrument;

use crate::context::RpcContext;
use crate::util::display_none;
use crate::util::serde::{display_serializable, IoFormat};
use crate::{Error, ErrorKind};

static SSH_AUTHORIZED_KEYS_FILE: &str = "/home/start9/.ssh/authorized_keys";

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct PubKey(
    #[serde(serialize_with = "crate::util::serde::serialize_display")]
    #[serde(deserialize_with = "crate::util::serde::deserialize_from_str")]
    openssh_keys::PublicKey,
);

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct SshKeyResponse {
    pub alg: String,
    pub fingerprint: String,
    pub hostname: String,
    pub created_at: String,
}
impl std::fmt::Display for SshKeyResponse {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{} {} {} {}",
            self.created_at, self.alg, self.fingerprint, self.hostname
        )
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

#[command(subcommands(add, delete, list,))]
pub fn ssh() -> Result<(), Error> {
    Ok(())
}

#[command(display(display_none))]
#[instrument(skip(ctx))]
pub async fn add(#[context] ctx: RpcContext, #[arg] key: PubKey) -> Result<SshKeyResponse, Error> {
    let pool = &ctx.secret_store;
    // check fingerprint for duplicates
    let fp = key.0.fingerprint_md5();
    match sqlx::query!("SELECT * FROM ssh_keys WHERE fingerprint = ?", fp)
        .fetch_optional(pool)
        .await?
    {
        None => {
            // if no duplicates, insert into DB
            let raw_key = format!("{}", key.0);
            let created_at = Utc::now().to_rfc3339();
            sqlx::query!(
                "INSERT INTO ssh_keys (fingerprint, openssh_pubkey, created_at) VALUES (?, ?, ?)",
                fp,
                raw_key,
                created_at
            )
            .execute(pool)
            .await?;
            // insert into live key file, for now we actually do a wholesale replacement of the keys file, for maximum
            // consistency
            sync_keys_from_db(pool, Path::new(SSH_AUTHORIZED_KEYS_FILE)).await?;
            Ok(SshKeyResponse {
                alg: key.0.keytype().to_owned(),
                fingerprint: fp,
                hostname: key.0.comment.unwrap_or(String::new()).to_owned(),
                created_at,
            })
        }
        Some(_) => Err(Error::new(eyre!("Duplicate ssh key"), ErrorKind::Duplicate)),
    }
}
#[command(display(display_none))]
#[instrument(skip(ctx))]
pub async fn delete(#[context] ctx: RpcContext, #[arg] fingerprint: String) -> Result<(), Error> {
    let pool = &ctx.secret_store;
    // check if fingerprint is in DB
    // if in DB, remove it from DB
    let n = sqlx::query!("DELETE FROM ssh_keys WHERE fingerprint = ?", fingerprint)
        .execute(pool)
        .await?
        .rows_affected();
    // if not in DB, Err404
    if n == 0 {
        Err(Error {
            source: color_eyre::eyre::eyre!("SSH Key Not Found"),
            kind: crate::error::ErrorKind::NotFound,
            revision: None,
        })
    } else {
        // AND overlay key file
        sync_keys_from_db(pool, Path::new(SSH_AUTHORIZED_KEYS_FILE)).await?;
        Ok(())
    }
}

fn display_all_ssh_keys(all: Vec<SshKeyResponse>, matches: &ArgMatches<'_>) {
    use prettytable::*;

    if matches.is_present("format") {
        return display_serializable(all, matches);
    }

    let mut table = Table::new();
    table.add_row(row![bc =>
        "CREATED AT",
        "ALGORITHM",
        "FINGERPRINT",
        "HOSTNAME",
    ]);
    for key in all {
        let row = row![
            &format!("{}", key.created_at),
            &key.alg,
            &key.fingerprint,
            &key.hostname,
        ];
        table.add_row(row);
    }
    table.print_tty(false);
}

#[command(display(display_all_ssh_keys))]
#[instrument(skip(ctx))]
pub async fn list(
    #[context] ctx: RpcContext,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<Vec<SshKeyResponse>, Error> {
    let pool = &ctx.secret_store;
    // list keys in DB and return them
    let entries = sqlx::query!("SELECT fingerprint, openssh_pubkey, created_at FROM ssh_keys")
        .fetch_all(pool)
        .await?;
    Ok(entries
        .into_iter()
        .map(|r| {
            let k = PubKey(r.openssh_pubkey.parse().unwrap()).0;
            let alg = k.keytype().to_owned();
            let fingerprint = k.fingerprint_md5();
            let hostname = k.comment.unwrap_or("".to_owned());
            let created_at = r.created_at;
            SshKeyResponse {
                alg,
                fingerprint,
                hostname,
                created_at,
            }
        })
        .collect())
}

#[instrument(skip(pool, dest))]
pub async fn sync_keys_from_db<P: AsRef<Path>>(pool: &Pool<Sqlite>, dest: P) -> Result<(), Error> {
    let dest = dest.as_ref();
    let keys = sqlx::query!("SELECT openssh_pubkey FROM ssh_keys")
        .fetch_all(pool)
        .await?;
    let contents: String = keys
        .into_iter()
        .map(|k| format!("{}\n", k.openssh_pubkey))
        .collect();
    let ssh_dir = dest.parent().ok_or_else(|| {
        Error::new(
            eyre!("SSH Key File cannot be \"/\""),
            crate::ErrorKind::Filesystem,
        )
    })?;
    if tokio::fs::metadata(ssh_dir).await.is_err() {
        tokio::fs::create_dir_all(ssh_dir).await?;
    }
    std::fs::write(dest, contents).map_err(|e| e.into())
}
