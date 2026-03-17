use axum::body::Body;
use axum::http::{HeaderMap, Response, StatusCode, header};
use axum::response::IntoResponse;
use std::process::Stdio;
use tokio::process::Command;

/// GET /api/backup — buffer a config backup as tar.gz and return it.
pub async fn backup_handler(headers: HeaderMap) -> Response<Body> {
    if !crate::middleware::validate_session_from_headers(&headers).await {
        return StatusCode::UNAUTHORIZED.into_response();
    }

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

    // Sanitize hostname for Content-Disposition
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

    // Buffer the full output so we can check exit status before responding
    let output = match Command::new("sysupgrade")
        .args(["--create-backup", "-"])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .kill_on_drop(true)
        .output()
        .await
    {
        Ok(output) => output,
        Err(e) => {
            tracing::error!("failed to spawn sysupgrade --create-backup: {e}");
            return Response::builder()
                .status(500)
                .body(Body::from("Failed to create backup"))
                .unwrap();
        }
    };

    if !output.status.success() {
        tracing::error!(
            "sysupgrade --create-backup failed (exit {}): {}",
            output.status.code().unwrap_or(-1),
            String::from_utf8_lossy(&output.stderr),
        );
        return Response::builder()
            .status(500)
            .body(Body::from("Failed to create backup"))
            .unwrap();
    }

    Response::builder()
        .header(header::CONTENT_TYPE, "application/gzip")
        .header(
            header::CONTENT_DISPOSITION,
            format!("attachment; filename=\"backup-{hostname}-{date}.tar.gz\""),
        )
        .body(Body::from(output.stdout))
        .unwrap()
}

/// POST /api/restore — upload a backup tar.gz and apply it, then reboot.
pub async fn restore_handler(
    headers: HeaderMap,
    mut multipart: axum::extract::Multipart,
) -> Response<Body> {
    if !crate::middleware::validate_session_from_headers(&headers).await {
        return StatusCode::UNAUTHORIZED.into_response();
    }

    // Extract the uploaded file (must be the "file" field)
    let data = loop {
        match multipart.next_field().await {
            Ok(Some(field)) if field.name() == Some("file") => match field.bytes().await {
                Ok(bytes) => break bytes,
                Err(e) => {
                    return Response::builder()
                        .status(400)
                        .body(Body::from(format!("Failed to read upload: {e}")))
                        .unwrap();
                }
            },
            Ok(Some(_)) => continue,
            Ok(None) => {
                return Response::builder()
                    .status(400)
                    .body(Body::from("No file uploaded"))
                    .unwrap();
            }
            Err(e) => {
                return Response::builder()
                    .status(400)
                    .body(Body::from(format!("Multipart error: {e}")))
                    .unwrap();
            }
        }
    };

    // Write to temp file
    let tmp_path = "/tmp/backup-restore.tar.gz";
    if let Err(e) = tokio::fs::write(tmp_path, &data).await {
        return Response::builder()
            .status(500)
            .body(Body::from(format!("Failed to write temp file: {e}")))
            .unwrap();
    }

    // Validate: ensure it's a valid tar.gz
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
            return Response::builder()
                .status(400)
                .body(Body::from("Invalid backup archive"))
                .unwrap();
        }
    }

    // Apply the backup — use .output() so pipes stay open and sysupgrade
    // can write to stderr without SIGPIPE (tokio's .status() drops piped
    // handles before wait, causing broken-pipe kills).
    let apply = Command::new("sysupgrade")
        .args(["--restore-backup", tmp_path])
        .output()
        .await;

    let _ = tokio::fs::remove_file(tmp_path).await;

    match apply {
        Ok(output) if output.status.success() => {}
        Ok(output) => {
            tracing::error!(
                "sysupgrade --restore-backup failed (exit {}): {}",
                output.status.code().unwrap_or(-1),
                String::from_utf8_lossy(&output.stderr),
            );
            return Response::builder()
                .status(500)
                .body(Body::from(format!(
                    "sysupgrade --restore-backup failed (exit {})",
                    output.status.code().unwrap_or(-1)
                )))
                .unwrap();
        }
        Err(e) => {
            return Response::builder()
                .status(500)
                .body(Body::from(format!("Failed to run restore: {e}")))
                .unwrap();
        }
    }

    // Spawn delayed reboot so the response can reach the client
    tokio::spawn(async {
        tokio::time::sleep(std::time::Duration::from_secs(1)).await;
        if let Err(e) = Command::new("reboot").status().await {
            tracing::error!("failed to reboot after restore: {e}");
        }
    });

    Response::builder()
        .header(header::CONTENT_TYPE, "application/json")
        .body(Body::from(r#"{"success":true}"#))
        .unwrap()
}
