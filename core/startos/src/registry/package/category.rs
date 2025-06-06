use std::collections::BTreeMap;

use clap::Parser;
use imbl_value::InternedString;
use models::PackageId;
use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::CliContext;
use crate::prelude::*;
use crate::registry::context::RegistryContext;
use crate::registry::package::index::Category;
use crate::util::serde::{display_serializable, HandlerExtSerde, WithIoFormat};

pub fn category_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "add",
            from_fn_async(add_category)
                .with_metadata("admin", Value::Bool(true))
                .no_display()
                .with_about("Add a category to the registry")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "remove",
            from_fn_async(remove_category)
                .with_metadata("admin", Value::Bool(true))
                .no_display()
                .with_about("Remove a category from the registry")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "add-package",
            from_fn_async(add_package)
                .with_metadata("admin", Value::Bool(true))
                .no_display()
                .with_about("Add a package to a category")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "remove-package",
            from_fn_async(remove_package)
                .with_metadata("admin", Value::Bool(true))
                .no_display()
                .with_about("Remove a package from a category")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "list",
            from_fn_async(list_categories)
                .with_display_serializable()
                .with_custom_display_fn(|params, categories| {
                    Ok(display_categories(params.params, categories))
                })
                .with_call_remote::<CliContext>(),
        )
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddCategoryParams {
    #[ts(type = "string")]
    pub id: InternedString,
    pub name: String,
}

pub async fn add_category(
    ctx: RegistryContext,
    AddCategoryParams { id, name }: AddCategoryParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_index_mut()
                .as_package_mut()
                .as_categories_mut()
                .insert(&id, &Category { name })
        })
        .await
        .result?;
    Ok(())
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RemoveCategoryParams {
    #[ts(type = "string")]
    pub id: InternedString,
}

pub async fn remove_category(
    ctx: RegistryContext,
    RemoveCategoryParams { id }: RemoveCategoryParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_index_mut()
                .as_package_mut()
                .as_categories_mut()
                .remove(&id)
        })
        .await
        .result?;
    Ok(())
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AddPackageToCategoryParams {
    #[ts(type = "string")]
    pub id: InternedString,
    pub package: PackageId,
}

pub async fn add_package(
    ctx: RegistryContext,
    AddPackageToCategoryParams { id, package }: AddPackageToCategoryParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_index_mut()
                .as_package_mut()
                .as_packages_mut()
                .as_idx_mut(&package)
                .or_not_found(&package)?
                .as_categories_mut()
                .mutate(|c| Ok(c.insert(id)))
        })
        .await
        .result?;
    Ok(())
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RemovePackageFromCategoryParams {
    #[ts(type = "string")]
    pub id: InternedString,
    pub package: PackageId,
}

pub async fn remove_package(
    ctx: RegistryContext,
    RemovePackageFromCategoryParams { id, package }: RemovePackageFromCategoryParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_index_mut()
                .as_package_mut()
                .as_packages_mut()
                .as_idx_mut(&package)
                .or_not_found(&package)?
                .as_categories_mut()
                .mutate(|c| Ok(c.remove(&id)))
        })
        .await
        .result?;
    Ok(())
}

pub async fn list_categories(
    ctx: RegistryContext,
) -> Result<BTreeMap<InternedString, Category>, Error> {
    ctx.db
        .peek()
        .await
        .into_index()
        .into_package()
        .into_categories()
        .de()
}

pub fn display_categories<T>(
    params: WithIoFormat<T>,
    categories: BTreeMap<InternedString, Category>,
) {
    use prettytable::*;

    if let Some(format) = params.format {
        return display_serializable(format, categories);
    }

    let mut table = Table::new();
    table.add_row(row![bc =>
        "ID",
        "NAME",
    ]);
    for (id, info) in categories {
        table.add_row(row![&*id, &info.name]);
    }
    table.print_tty(false).unwrap();
}
