use clap::ArgMatches;
use color_eyre::eyre::eyre;
use rpc_toolkit::command;
use serde_json::Value;
use tracing::instrument;

use crate::context::RpcContext;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::{Error, ErrorKind};

pub fn display_properties(response: Value, _: &ArgMatches<'_>) {
    println!("{}", response);
}

#[command(display(display_properties))]
pub async fn properties(#[context] ctx: RpcContext, #[arg] id: PackageId) -> Result<Value, Error> {
    Ok(fetch_properties(ctx, id).await?)
}

#[instrument(skip(ctx))]
pub async fn fetch_properties(ctx: RpcContext, id: PackageId) -> Result<Value, Error> {
    let mut db = ctx.db.handle();
    let manifest: Manifest = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&id)
        .and_then(|p| p.installed())
        .map(|m| m.manifest())
        .get(&mut db, true)
        .await?
        .to_owned()
        .ok_or_else(|| Error::new(eyre!("{} is not installed", id), ErrorKind::NotFound))?;
    if let Some(props) = manifest.properties {
        props
            .execute::<(), Value>(
                &ctx,
                &manifest.id,
                &manifest.version,
                Some(&format!("Properties-{}", rand::random::<u64>())),
                &manifest.volumes,
                None,
                false,
            )
            .await?
            .map_err(|(_, e)| Error::new(eyre!("{}", e), ErrorKind::Docker))
            .and_then(|a| Ok(a))
    } else {
        Ok(Value::Null)
    }
}
