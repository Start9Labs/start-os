use serde::{Deserialize, Serialize};

use crate::ActionId;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ProcedureName {
    GetConfig,
    SetConfig,
    CreateBackup,
    RestoreBackup,
    GetActionInput(ActionId),
    RunAction(ActionId),
    PackageInit,
    PackageUninit,
}

impl ProcedureName {
    pub fn js_function_name(&self) -> String {
        match self {
            ProcedureName::PackageInit => "/packageInit".to_string(),
            ProcedureName::PackageUninit => "/packageUninit".to_string(),
            ProcedureName::SetConfig => "/config/set".to_string(),
            ProcedureName::GetConfig => "/config/get".to_string(),
            ProcedureName::CreateBackup => "/backup/create".to_string(),
            ProcedureName::RestoreBackup => "/backup/restore".to_string(),
            ProcedureName::RunAction(id) => format!("/actions/{}/run", id),
            ProcedureName::GetActionInput(id) => format!("/actions/{}/getInput", id),
        }
    }
}
