use std::collections::BTreeMap;

use clap::Parser;
use models::PackageId;
use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::CliContext;
use crate::prelude::*;
use crate::registry::admin::display_signers;
use crate::registry::context::RegistryContext;
use crate::registry::signer::SignerInfo;
use crate::rpc_continuations::Guid;
use crate::util::serde::HandlerExtSerde;

pub fn signer_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "add",
            from_fn_async(add_package_signer)
                .with_metadata("admin", Value::Bool(true))
                .no_display()
                .with_about("Add package signer")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "remove",
            from_fn_async(remove_package_signer)
                .with_metadata("admin", Value::Bool(true))
                .no_display()
                .with_about("Remove package signer")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "list",
            from_fn_async(list_package_signers)
                .with_display_serializable()
                .with_custom_display_fn(|handle, result| Ok(display_signers(handle.params, result)))
                .with_about("List package signers and related signer info")
                .with_call_remote::<CliContext>(),
        )
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct PackageSignerParams {
    pub id: PackageId,
    pub signer: Guid,
}

pub async fn add_package_signer(
    ctx: RegistryContext,
    PackageSignerParams { id, signer }: PackageSignerParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            ensure_code!(
                db.as_index().as_signers().contains_key(&signer)?,
                ErrorKind::InvalidRequest,
                "unknown signer {signer}"
            );

            db.as_index_mut()
                .as_package_mut()
                .as_packages_mut()
                .as_idx_mut(&id)
                .or_not_found(&id)?
                .as_authorized_mut()
                .mutate(|s| Ok(s.insert(signer)))?;

            Ok(())
        })
        .await.result
}

pub async fn remove_package_signer(
    ctx: RegistryContext,
    PackageSignerParams { id, signer }: PackageSignerParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            if !db
                .as_index_mut()
                .as_package_mut()
                .as_packages_mut()
                .as_idx_mut(&id)
                .or_not_found(&id)?
                .as_authorized_mut()
                .mutate(|s| Ok(s.remove(&signer)))?
            {
                return Err(Error::new(
                    eyre!("signer {signer} is not authorized to sign for {id}"),
                    ErrorKind::NotFound,
                ));
            }

            Ok(())
        })
        .await.result
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ListPackageSignersParams {
    pub id: PackageId,
}

pub async fn list_package_signers(
    ctx: RegistryContext,
    ListPackageSignersParams { id }: ListPackageSignersParams,
) -> Result<BTreeMap<Guid, SignerInfo>, Error> {
    let db = ctx.db.peek().await;
    db.as_index()
        .as_package()
        .as_packages()
        .as_idx(&id)
        .or_not_found(&id)?
        .as_authorized()
        .de()?
        .into_iter()
        .filter_map(|guid| {
            db.as_index()
                .as_signers()
                .as_idx(&guid)
                .map(|s| s.de().map(|s| (guid, s)))
        })
        .collect()
}
