use std::ops::Deref;

use chrono::Utc;
use sqlx::{query, Pool, Postgres};

use super::device_info::DeviceInfo;
use crate::context::RpcContext;
use crate::Error;

pub struct MetricsParams {
    version: char,
    pkg_id: char,
}

// TODO: replace pool placeholders (and MetricsParams?)
pub async fn record_metrics(
    pool: Pool<Postgres>,
    MetricsParams { version, pkg_id }: MetricsParams,
) -> Result<(), Error> {
    let created_at = Utc::now().to_rfc3339();
    query!(
        "INSERT INTO user_activity (created_at, version, pkg_id) VALUES ($1, $2, $3)",
        created_at,
        version,
        pkg_id
    )
    .execute(pool)
    .await?;
    Ok(())
}

pub async fn record_user_activity(pool: Pool<Postgres>, context: RpcContext) -> Result<(), Error> {
    let info = DeviceInfo::from(&context);
    let created_at = Utc::now().to_rfc3339();
    let server_id = context.account.read().await.server_id;
    let os_vers = serde_json::to_string(&info.os.version).unwrap();
    let arch = info.hardware.arch.deref();

    query!("INSERT INTO user_activity (created_at, server_id, os_version, arch) VALUES ($1, $2, $3, $4)",
    created_at,
    server_id,
    os_vers,
    arch
    )
    .execute(pool)
    .await?;

    Ok(())
}
