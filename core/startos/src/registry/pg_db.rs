use chrono::Utc;
use sqlx::{Pool, Postgres};

use crate::Error;

pub struct MetricsParams {
    version: char,
    pkg_id: char,
}

pub struct ActivityParams {
    server_id: char,
    os_version: char,
    arch: char,
}

// TODO: replace pool placeholders
pub async fn record_metrics(
    pool: Pool<Postgres>,
    MetricsParams { version, pkg_id }: MetricsParams,
) -> Result<(), Error> {
    let created_at = Utc::now().to_rfc3339();
    sqlx::query!(
        "INSERT INTO user_activity (created_at, version, pkg_id) VALUES ($1, $2, $3)",
        created_at,
        version,
        pkg_id
    )
    .execute(pool)
    .await?;
    Ok(())
}

pub async fn record_user_activity(
    pool: Pool<Postgres>,
    ActivityParams {
        server_id,
        os_version,
        arch,
    }: ActivityParams,
) -> Result<(), Error> {
    let created_at = Utc::now().to_rfc3339();
    sqlx::query!("INSERT INTO user_activity (created_at, server_id, os_version, arch) VALUES ($1, $2, $3, $4)",
    created_at,
    server_id,
    os_version,
    arch
    )
    .execute(pool)
    .await?;

    Ok(())
}
