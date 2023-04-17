use std::sync::Arc;

use models::ProcedureName;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tracing::instrument;

use crate::prelude::*;
use crate::{config::Input, manager::Manager};

#[derive(Debug, Deserialize, Serialize)]
pub struct ConfigRes {
    pub input: Option<Input>,
    pub spec: Value,
}

#[instrument(skip_all)]
pub async fn get(manager: Arc<Manager>) -> Result<ConfigRes, Error> {
    manager
        .run_procedure(ProcedureName::GetConfig, None::<()>, None)
        .await
}

#[instrument(skip_all)]
pub async fn set(manager: Arc<Manager>, input: &Input) -> Result<Value, Error> {
    manager
        .run_procedure(ProcedureName::SetConfig, Some(input), None)
        .await
}
