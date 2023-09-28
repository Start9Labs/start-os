use clap::ArgMatches;
use color_eyre::eyre::eyre;
use rpc_toolkit::command;
use serde_json::Value;
use tracing::instrument;

use crate::context::RpcContext;
use crate::prelude::*;
use crate::procedure::ProcedureName;
use crate::s9pk::manifest::PackageId;
use crate::{Error, ErrorKind};

pub fn display_properties(response: Value, _: &ArgMatches) {
    println!("{}", response);
}

#[command(display(display_properties))]
pub async fn properties(#[context] ctx: RpcContext, #[arg] id: PackageId) -> Result<Value, Error> {
    Ok(fetch_properties(ctx, id).await?)
}

#[instrument(skip_all)]
pub async fn fetch_properties(ctx: RpcContext, id: PackageId) -> Result<Value, Error> {
    let peek = ctx.db.peek().await?;

    let manifest = peek
        .as_package_data()
        .as_idx(&id)
        .ok_or_else(|| Error::new(eyre!("{} is not installed", id), ErrorKind::NotFound))?
        .expect_as_installed()?
        .as_manifest()
        .de()?;
    if let Some(props) = manifest.properties {
        props
            .execute::<(), Value>(
                &ctx,
                &manifest.id,
                &manifest.version,
                ProcedureName::Properties,
                &manifest.volumes,
                None,
                None,
            )
            .await?
            .map_err(|(_, e)| Error::new(eyre!("{}", e), ErrorKind::Docker))
            .and_then(|a| Ok(a))
    } else {
        Ok(Value::Null)
    }
}
