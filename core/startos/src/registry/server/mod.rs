use axum::routing::get;
use axum::{Json, Router};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::RegistryContext;
use crate::prelude::*;
use crate::registry::server::os::{get_os_index, OsIndex};

pub mod admin;
pub mod os;

#[derive(Debug, Default, Deserialize, Serialize, HasModel)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
pub struct RegistryDatabase {
    pub admins: (),
    pub index: FullIndex,
}

#[derive(Debug, Default, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct FullIndex {
    // pub package: PackageIndex,
    pub os: OsIndex,
}

pub async fn get_full_index(ctx: RegistryContext) -> Result<FullIndex, Error> {
    ctx.db.peek().await.into_index().de()
}

pub fn router(ctx: &RegistryContext) -> Router {
    Router::new()
        .route("/index", {
            let ctx = ctx.clone();
            get(|| async { Ok::<_, Error>(Json(get_full_index(ctx).await?)) })
        })
        .nest("/os", os::router(ctx))
        .nest("/admin", admin::router(ctx))
}
