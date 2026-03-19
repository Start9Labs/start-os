use std::io::Write;
use std::path::PathBuf;
use std::process::Stdio;

use axum::body::Body;
use axum::http::{Response, header};
use clap::Parser;
use imbl_value::imbl::OrdMap;
use imbl_value::json;
use itertools::Itertools;
use rpc_toolkit::{from_fn_async, CallRemote, Context, Empty, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tempfile::NamedTempFile;
use tokio::process::Command;

use crate::continuations::{self, Guid, RpcContinuation};
use crate::error::Error;
use crate::{CliContext, ServerContext};

#[derive(Debug, Serialize, Deserialize)]
pub struct BackupCreateRes {
    pub guid: Guid,
    pub filename: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct BackupRestoreRes {
    pub upload: Guid,
}

/// RPC handler: buffer backup, register download continuation, return guid + filename.
async fn create(ctx: ServerContext) -> Result<BackupCreateRes, Error> {
    // Get hostname for the filename
    let hostname = match Command::new("uci")
        .args(["get", "system.@system[0].hostname"])
        .output()
        .await
    {
        Ok(output) if output.status.success() => {
            String::from_utf8_lossy(&output.stdout).trim().to_string()
        }
        _ => "startwrt".to_string(),
    };

    let hostname: String = hostname
        .chars()
        .filter(|c| c.is_ascii_alphanumeric() || *c == '-' || *c == '_' || *c == '.')
        .collect();
    let hostname = if hostname.is_empty() {
        "startwrt".to_string()
    } else {
        hostname
    };

    let date = chrono::Utc::now().format("%Y-%m-%d");
    let filename = format!("backup-{hostname}-{date}.tar.gz");

    let output = Command::new("sysupgrade")
        .args(["--create-backup", "-"])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .kill_on_drop(true)
        .output()
        .await
        .map_err(|e| {
            crate::activity::log("backup", "downloaded", false, "Failed to create config backup", Some(&e.to_string()));
            Error::other(format!("Failed to spawn sysupgrade: {e}"))
        })?;

    if !output.status.success() {
        let msg = format!(
            "sysupgrade --create-backup failed (exit {})",
            output.status.code().unwrap_or(-1),
        );
        tracing::error!("{}: {}", msg, String::from_utf8_lossy(&output.stderr));
        crate::activity::log("backup", "downloaded", false, "Failed to create config backup", Some(&msg));
        return Err(Error::other(msg));
    }

    crate::activity::log("backup", "downloaded", true, "Downloaded config backup", None);

    let body = output.stdout;
    let fname = filename.clone();
    let guid = Guid::new();
    ctx.continuations.add(
        guid.clone(),
        RpcContinuation::rest(
            move |_req| async move {
                Ok(Response::builder()
                    .header(header::CONTENT_TYPE, "application/gzip")
                    .header(
                        header::CONTENT_DISPOSITION,
                        format!("attachment; filename=\"{fname}\""),
                    )
                    .body(Body::from(body))
                    .unwrap())
            },
            continuations::DEFAULT_TTL,
        ),
    );

    Ok(BackupCreateRes { guid, filename })
}

/// CLI handler: call backup.create via RPC, download the file, write to ~/Downloads.
async fn cli_download(
    HandlerArgs {
        context: ctx,
        parent_method,
        method,
        ..
    }: HandlerArgs<CliContext>,
) -> Result<(), Error> {
    let res: BackupCreateRes = imbl_value::from_value(
        ctx.call_remote(
            &parent_method.into_iter().chain(method).join("."),
            OrdMap::new(),
            json!({}),
            Empty {},
        )
        .await?,
    )
    .map_err(|e| Error::other(format!("Failed to parse response: {e}")))?;

    let (bytes, _) = ctx.rest_download(res.guid.as_ref()).await?;

    let download_dir = dirs::download_dir()
        .unwrap_or_else(|| PathBuf::from("."));
    let path = download_dir.join(&res.filename);
    std::fs::write(&path, &bytes).map_err(|e| {
        crate::Error::other(format!("Failed to write {}: {e}", path.display()))
    })?;

    println!("{}", path.display());
    Ok(())
}

/// RPC handler: register upload continuation for restore, return guid.
async fn restore(ctx: ServerContext) -> Result<BackupRestoreRes, Error> {
    let guid = Guid::new();
    ctx.continuations.add(
        guid.clone(),
        RpcContinuation::rest(
            move |req| async move {
                let body_bytes = axum::body::to_bytes(req.into_body(), 10 * 1024 * 1024)
                    .await
                    .map_err(|e| Error::other(format!("Failed to read upload: {e}")))?;

                // Atomic temp file write
                let tmp_path = "/tmp/backup-restore.tar.gz";
                let data = body_bytes.to_vec();
                tokio::task::spawn_blocking(move || -> Result<(), Error> {
                    let mut tmp = NamedTempFile::new_in("/tmp")
                        .map_err(|e| Error::other(format!("Failed to create temp file: {e}")))?;
                    tmp.write_all(&data)
                        .map_err(|e| Error::other(format!("Failed to write temp file: {e}")))?;
                    tmp.as_file()
                        .sync_all()
                        .map_err(|e| Error::other(format!("Failed to sync temp file: {e}")))?;
                    tmp.persist(tmp_path)
                        .map_err(|e| Error::other(format!("Failed to persist temp file: {e}")))?;
                    Ok(())
                })
                .await
                .map_err(|e| Error::other(format!("Failed to write temp file: {e}")))?
                ?;

                // Validate tar.gz
                let validate = Command::new("tar")
                    .args(["-tzf", tmp_path])
                    .stdout(Stdio::null())
                    .stderr(Stdio::null())
                    .status()
                    .await;

                match validate {
                    Ok(status) if status.success() => {}
                    _ => {
                        let _ = tokio::fs::remove_file(tmp_path).await;
                        crate::activity::log("backup", "restored", false, "Failed to restore config backup", Some("Invalid backup archive"));
                        return Err(Error::other("Invalid backup archive"));
                    }
                }

                // Apply the backup
                let apply = Command::new("sysupgrade")
                    .args(["--restore-backup", tmp_path])
                    .output()
                    .await;

                let _ = tokio::fs::remove_file(tmp_path).await;

                match apply {
                    Ok(output) if output.status.success() => {}
                    Ok(output) => {
                        let msg = format!(
                            "sysupgrade --restore-backup failed (exit {})",
                            output.status.code().unwrap_or(-1)
                        );
                        tracing::error!("{}: {}", msg, String::from_utf8_lossy(&output.stderr));
                        crate::activity::log("backup", "restored", false, "Failed to restore config backup", Some(&msg));
                        return Err(Error::other(msg));
                    }
                    Err(e) => {
                        crate::activity::log("backup", "restored", false, "Failed to restore config backup", Some(&e.to_string()));
                        return Err(Error::other(format!("Failed to run restore: {e}")));
                    }
                }

                crate::activity::log("backup", "restored", true, "Restored config backup (rebooting)", None);

                // Spawn delayed reboot
                tokio::spawn(async {
                    tokio::time::sleep(std::time::Duration::from_secs(1)).await;
                    if let Err(e) = Command::new("reboot").status().await {
                        tracing::error!("failed to reboot after restore: {e}");
                    }
                });

                Ok(Response::builder()
                    .header(header::CONTENT_TYPE, "application/json")
                    .body(Body::from(r#"{"success":true}"#))
                    .unwrap())
            },
            continuations::DEFAULT_TTL,
        ),
    );
    Ok(BackupRestoreRes { upload: guid })
}

#[derive(Debug, Deserialize, Serialize, Parser)]
struct RestoreParams {
    /// Path to the backup file
    file: PathBuf,
}

/// CLI handler: read local file, call backup.restore via RPC, upload the file.
async fn cli_upload(
    HandlerArgs {
        context: ctx,
        parent_method,
        method,
        params: RestoreParams { file },
        ..
    }: HandlerArgs<CliContext, RestoreParams>,
) -> Result<(), Error> {
    let data = std::fs::read(&file).map_err(|e| {
        crate::Error::other(format!("Failed to read {}: {e}", file.display()))
    })?;

    let res: BackupRestoreRes = imbl_value::from_value(
        ctx.call_remote(
            &parent_method.into_iter().chain(method).join("."),
            OrdMap::new(),
            json!({}),
            Empty {},
        )
        .await?,
    )
    .map_err(|e| Error::other(format!("Failed to parse response: {e}")))?;

    ctx.rest_upload(res.upload.as_ref(), data).await?;

    println!("Backup restored. Device is rebooting.");
    Ok(())
}

pub fn backup<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("create", from_fn_async(create).no_cli())
        .subcommand(
            "create",
            from_fn_async(cli_download)
                .no_display()
                .with_about("Download a config backup"),
        )
        .subcommand("restore", from_fn_async(restore).no_cli())
        .subcommand(
            "restore",
            from_fn_async(cli_upload)
                .no_display()
                .with_about("Restore config backup from file"),
        )
}
