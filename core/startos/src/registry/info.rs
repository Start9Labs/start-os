use std::collections::BTreeMap;
use std::path::PathBuf;

use clap::Parser;
use imbl_value::InternedString;
use itertools::Itertools;
use models::DataUrl;
use rpc_toolkit::{from_fn_async, Context, Empty, HandlerArgs, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::CliContext;
use crate::prelude::*;
use crate::registry::context::RegistryContext;
use crate::registry::package::index::Category;
use crate::util::serde::{HandlerExtSerde, WithIoFormat};

pub fn info_api<C: Context>() -> ParentHandler<C, WithIoFormat<Empty>> {
    ParentHandler::<C, WithIoFormat<Empty>>::new()
        .root_handler(
            from_fn_async(get_info)
                .with_display_serializable()
                .with_about("Display registry name, icon, and package categories")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "set-name",
            from_fn_async(set_name)
                .with_metadata("admin", Value::Bool(true))
                .no_display()
                .with_about("Set the name for the registry")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "set-icon",
            from_fn_async(set_icon)
                .with_metadata("admin", Value::Bool(true))
                .no_cli(),
        )
        .subcommand(
            "set-icon",
            from_fn_async(cli_set_icon)
                .no_display()
                .with_about("Set the icon for the registry"),
        )
}

#[derive(Debug, Default, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RegistryInfo {
    pub name: Option<String>,
    pub icon: Option<DataUrl<'static>>,
    #[ts(as = "BTreeMap::<String, Category>")]
    pub categories: BTreeMap<InternedString, Category>,
}

pub async fn get_info(ctx: RegistryContext) -> Result<RegistryInfo, Error> {
    let peek = ctx.db.peek().await.into_index();
    Ok(RegistryInfo {
        name: peek.as_name().de()?,
        icon: peek.as_icon().de()?,
        categories: peek.as_package().as_categories().de()?,
    })
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetNameParams {
    pub name: String,
}

pub async fn set_name(
    ctx: RegistryContext,
    SetNameParams { name }: SetNameParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| db.as_index_mut().as_name_mut().ser(&Some(name)))
        .await
        .result
}

#[derive(Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetIconParams {
    pub icon: DataUrl<'static>,
}

pub async fn set_icon(
    ctx: RegistryContext,
    SetIconParams { icon }: SetIconParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| db.as_index_mut().as_icon_mut().ser(&Some(icon)))
        .await
        .result
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CliSetIconParams {
    pub icon: PathBuf,
}

pub async fn cli_set_icon(
    HandlerArgs {
        context: ctx,
        parent_method,
        method,
        params: CliSetIconParams { icon },
        ..
    }: HandlerArgs<CliContext, CliSetIconParams>,
) -> Result<(), Error> {
    let data_url = DataUrl::from_path(icon).await?;
    ctx.call_remote::<RegistryContext>(
        &parent_method.into_iter().chain(method).join("."),
        imbl_value::json!({
            "icon": data_url,
        }),
    )
    .await?;
    Ok(())
}
