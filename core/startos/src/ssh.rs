use std::path::Path;

use chrono::Utc;
use clap::Parser;
use color_eyre::eyre::eyre;
use rpc_toolkit::{command, from_fn_async, Empty, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use sqlx::{Pool, Postgres};
use tracing::instrument;

use crate::context::{CliContext, RpcContext};
use crate::util::serde::{display_serializable, WithIoFormat};
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

// #[command(subcommands(add, delete, list,))]
pub fn ssh() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "add",
            from_fn_async(add)
                .no_display()
                .with_remote_cli::<CliContext>(),
        )
        .subcommand(
            "delete",
            from_fn_async(delete)
                .no_display()
                .with_remote_cli::<CliContext>(),
        )
        .subcommand(
            "list",
            from_fn_async(list)
                .with_custom_display_fn(|handle, result| {
                    Ok(display_all_ssh_keys(handle.params, result))
                })
                .with_remote_cli::<CliContext>(),
        )
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct AddParams {
    key: PubKey,
}

#[instrument(skip_all)]
pub async fn add(ctx: RpcContext, AddParams { key }: AddParams) -> Result<SshKeyResponse, Error> {
    let pool = &ctx.secret_store;
    // check fingerprint for duplicates
    let fp = key.0.fingerprint_md5();
    match sqlx::query!("SELECT * FROM ssh_keys WHERE fingerprint = $1", fp)
        .fetch_optional(pool)
        .await?
    {
        None => {
            // if no duplicates, insert into DB
            let raw_key = format!("{}", key.0);
            let created_at = Utc::now().to_rfc3339();
            sqlx::query!(
                "INSERT INTO ssh_keys (fingerprint, openssh_pubkey, created_at) VALUES ($1, $2, $3)",
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

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct DeleteParams {
    fingerprint: String,
}

#[instrument(skip_all)]
pub async fn delete(
    ctx: RpcContext,
    DeleteParams { fingerprint }: DeleteParams,
) -> Result<(), Error> {
    let pool = &ctx.secret_store;
    // check if fingerprint is in DB
    // if in DB, remove it from DB
    let n = sqlx::query!("DELETE FROM ssh_keys WHERE fingerprint = $1", fingerprint)
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

fn display_all_ssh_keys(params: WithIoFormat<Empty>, result: Vec<SshKeyResponse>) {
    use prettytable::*;

    if let Some(format) = params.format {
        return display_serializable(format, params);
    }

    let mut table = Table::new();
    table.add_row(row![bc =>
        "CREATED AT",
        "ALGORITHM",
        "FINGERPRINT",
        "HOSTNAME",
    ]);
    for key in result {
        let row = row![
            &format!("{}", key.created_at),
            &key.alg,
            &key.fingerprint,
            &key.hostname,
        ];
        table.add_row(row);
    }
    table.print_tty(false).unwrap();
}

#[instrument(skip_all)]
pub async fn list(ctx: RpcContext, _: Empty) -> Result<Vec<SshKeyResponse>, Error> {
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

#[instrument(skip_all)]
pub async fn sync_keys_from_db<P: AsRef<Path>>(
    pool: &Pool<Postgres>,
    dest: P,
) -> Result<(), Error> {
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
