use serde::{Deserialize, Serialize};

use crate::ActionId;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ProcedureName {
    CreateBackup,
    GetActionInput(ActionId),
    RunAction(ActionId),
}

impl ProcedureName {
    pub fn js_function_name(&self) -> String {
        match self {
            ProcedureName::CreateBackup => "/backup/create".to_string(),
            ProcedureName::RunAction(id) => format!("/actions/{}/run", id),
            ProcedureName::GetActionInput(id) => format!("/actions/{}/getInput", id),
        }
    }
}
