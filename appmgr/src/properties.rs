use anyhow::anyhow;
use clap::ArgMatches;
use rpc_toolkit::command;
use serde_json::Value;

use crate::context::RpcContext;
use crate::s9pk::manifest::PackageId;
use crate::{Error, ErrorKind, ResultExt};

pub fn display_properties(response: Value, _: &ArgMatches<'_>) {
    println!("{}", response);
}

#[command(display(display_properties))]
pub async fn properties(#[context] ctx: RpcContext, #[arg] id: PackageId) -> Result<Value, Error> {
    Ok(fetch_properties(ctx, id).await?)
}

pub async fn fetch_properties(ctx: RpcContext, id: PackageId) -> Result<Value, Error> {
    let mut db = ctx.db.handle();
    let manifest = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&id)
        .and_then(|p| p.installed())
        .expect(&mut db)
        .await
        .with_kind(ErrorKind::NotFound)?
        .manifest()
        .get(&mut db, true)
        .await?
        .to_owned();
    manifest
        .properties
        .execute::<(), Value>(
            &ctx,
            &manifest.id,
            &manifest.version,
            None,
            &manifest.volumes,
            None,
            false,
        )
        .await?
        .map_err(|_| Error::new(anyhow!("Properties failure!"), crate::ErrorKind::Docker))
        .and_then(|a| Ok(a))
}
