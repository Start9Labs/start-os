use std::collections::BTreeMap;

use clap::Parser;
use imbl_value::InternedString;
use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::context::CliContext;
use crate::prelude::*;
use crate::registry::context::RegistryContext;
use crate::registry::package::index::Category;
use crate::s9pk::manifest::Description;
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
    #[arg(short, long, help = "Short description for the category")]
    pub short: String,
    #[arg(short, long, help = "Long description for the category")]
    pub long: String,
}

pub async fn add_category(
    ctx: RegistryContext,
    AddCategoryParams {
        id,
        name,
        short,
        long,
    }: AddCategoryParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_index_mut()
                .as_package_mut()
                .as_categories_mut()
                .insert(
                    &id,
                    &Category {
                        name,
                        description: Description { short, long },
                    },
                )
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
        "SHORT DESCRIPTION",
        "LONG DESCRIPTION",
    ]);
    for (id, info) in categories {
        table.add_row(row![
            &*id,
            &info.name,
            &info.description.short,
            &info.description.long,
        ]);
    }
    table.print_tty(false).unwrap();
}
