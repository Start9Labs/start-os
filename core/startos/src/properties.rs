use clap::Parser;
use imbl_value::{json, Value};
use models::PackageId;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};

use crate::prelude::*;
use crate::Error;
use crate::{context::RpcContext, db::model::ExposedUI};

pub fn display_properties(response: Value) {
    println!("{}", response);
}

trait IntoProperties {
    fn into_properties(self, store: &Value) -> Value;
}
impl IntoProperties for Vec<ExposedUI> {
    fn into_properties(self, store: &Value) -> Value {
        let mut data = json!({});
        for ui in self {
            let value = ui.path.get(store);
            data[ui.title] = json!({
                "type": "string",
                "description": ui.description,
                "value": value.map(|x| x.to_string()).unwrap_or_default(),
                "copyable": ui.copyable,
                "qr": ui.qr,
                "masked": ui.masked,
            });
        }
        json!({
            "version": 2,
            "data": data
        })
    }
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct PropertiesParam {
    id: PackageId,
}
// #[command(display(display_properties))]
pub async fn properties(
    ctx: RpcContext,
    PropertiesParam { id }: PropertiesParam,
) -> Result<Value, Error> {
    let peeked = ctx.db.peek().await;
    let data = peeked
        .as_private()
        .as_package_stores()
        .as_idx(&id)
        .map(|x| x.de())
        .unwrap_or_else(|| Ok(json!({})))?;
    Ok(peeked
        .as_public()
        .as_package_data()
        .as_idx(&id)
        .or_not_found(&id)?
        .as_installed()
        .or_not_found(&id)?
        .as_store_exposed_ui()
        .de()?
        .into_properties(&data))
}
