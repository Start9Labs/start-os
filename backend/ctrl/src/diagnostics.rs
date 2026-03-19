use axum::body::Body;
use axum::http::{HeaderMap, Response, StatusCode, header};
use axum::response::IntoResponse;
use tokio::process::Command;

/// GET /api/diagnostics — bundle logs and activity into a tar.gz download.
pub async fn diagnostics_handler(headers: HeaderMap) -> Response<Body> {
    if !crate::middleware::validate_session_from_headers(&headers).await {
        return StatusCode::UNAUTHORIZED.into_response();
    }

    let date = chrono::Utc::now().format("%Y-%m-%d");

    // Collect logs from logread
    let logs = match Command::new("logread").output().await {
        Ok(output) if output.status.success() => output.stdout,
        Ok(output) => {
            tracing::warn!("logread exited with {}", output.status);
            output.stdout // partial output is still useful
        }
        Err(e) => {
            tracing::warn!("failed to run logread: {e}");
            format!("Failed to collect logs: {e}").into_bytes()
        }
    };

    // Collect activity log
    let activity = tokio::fs::read("/etc/startwrt/activity.json")
        .await
        .unwrap_or_else(|_| b"[]".to_vec());

    // Build tar.gz in memory
    let gz_bytes = match build_tar_gz(&logs, &activity) {
        Ok(bytes) => bytes,
        Err(e) => {
            tracing::error!("failed to build diagnostics archive: {e}");
            crate::activity::log(
                "diagnostics",
                "downloaded",
                false,
                "Failed to create diagnostics bundle",
                Some(&e.to_string()),
            );
            return Response::builder()
                .status(500)
                .body(Body::from("Failed to create diagnostics bundle"))
                .unwrap();
        }
    };

    crate::activity::log(
        "diagnostics",
        "downloaded",
        true,
        "Downloaded support diagnostics",
        None,
    );

    Response::builder()
        .header(header::CONTENT_TYPE, "application/gzip")
        .header(
            header::CONTENT_DISPOSITION,
            format!("attachment; filename=\"diagnostics-startwrt-{date}.tar.gz\""),
        )
        .body(Body::from(gz_bytes))
        .unwrap()
}

/// Build a tar.gz archive in memory containing logs.txt and activity.json.
fn build_tar_gz(logs: &[u8], activity: &[u8]) -> std::io::Result<Vec<u8>> {
    let buf = Vec::new();
    let encoder = flate2::write::GzEncoder::new(buf, flate2::Compression::default());
    let mut archive = tar::Builder::new(encoder);

    let mut add_file = |name: &str, data: &[u8]| -> std::io::Result<()> {
        let mut header = tar::Header::new_gnu();
        header.set_size(data.len() as u64);
        header.set_mode(0o644);
        header.set_mtime(
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
        );
        header.set_cksum();
        archive.append_data(&mut header, name, data)?;
        Ok(())
    };

    add_file("logs.txt", logs)?;
    add_file("activity.json", activity)?;

    let encoder = archive.into_inner()?;
    encoder.finish()
}
