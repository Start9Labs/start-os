use std::collections::BTreeMap;

use clap::Parser;
use imbl_value::{json, InOMap, InternedString, Value};
use models::PackageId;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};

use crate::db::model::package::ExposedUI;
use crate::prelude::*;
use crate::Error;
use crate::{context::RpcContext, db::model::package::StoreExposedUI};

pub fn display_properties(response: Value) {
    println!("{}", response);
}

trait IntoProperties {
    fn into_properties(self, store: &Value) -> Value;
}
impl IntoProperties for ExposedUI {
    fn into_properties(self, store: &Value) -> Value {
        match self {
            ExposedUI::Object { value, description } => {
                json!({
                    "type": "object",
                    "description": description,
                    "value": value.into_iter().map(|(k, v)| (k, v.into_properties(store))).collect::<BTreeMap<String,_>>()
                })
            }
            ExposedUI::String {
                path,
                description,
                masked,
                copyable,
                qr,
            } => json!({
                "type": "string",
                "description": description,
                "value": path.get(store).cloned().unwrap_or_default(),
                "copyable": copyable,
                "qr": qr,
                "masked": masked
            }),
        }
    }
}

impl IntoProperties for StoreExposedUI {
    fn into_properties(self, store: &Value) -> Value {
        Value::Object(
            self.0
                .into_iter()
                .map(|(k, v)| (k, v.into_properties(store)))
                .collect::<InOMap<InternedString, Value>>(),
        )
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
        .as_store_exposed_ui()
        .de()?
        .into_properties(&data))
}
