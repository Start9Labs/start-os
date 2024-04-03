use serde::{Deserialize, Serialize};

use crate::{ActionId, PackageId};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ProcedureName {
    StartMain,
    StopMain,
    GetConfig,
    SetConfig,
    CreateBackup,
    Properties,
    RestoreBackup,
    ActionMetadata,
    RunAction(ActionId),
    GetAction(ActionId),
    QueryDependency(PackageId),
    UpdateDependency(PackageId),
    Init,
    Uninit,
}

impl ProcedureName {
    pub fn js_function_name(&self) -> String {
        match self {
            ProcedureName::Init => "/init".to_string(),
            ProcedureName::Uninit => "/uninit".to_string(),
            ProcedureName::StartMain => "/main/start".to_string(),
            ProcedureName::StopMain => "/main/stop".to_string(),
            ProcedureName::SetConfig => "/config/set".to_string(),
            ProcedureName::GetConfig => "/config/get".to_string(),
            ProcedureName::CreateBackup => "/backup/create".to_string(),
            ProcedureName::Properties => "/properties".to_string(),
            ProcedureName::RestoreBackup => "/backup/restore".to_string(),
            ProcedureName::ActionMetadata => "/actions/metadata".to_string(),
            ProcedureName::RunAction(id) => format!("/actions/{}/run", id),
            ProcedureName::GetAction(id) => format!("/actions/{}/get", id),
            ProcedureName::QueryDependency(id) => format!("/dependencies/{}/query", id),
            ProcedureName::UpdateDependency(id) => format!("/dependencies/{}/update", id),
        }
    }
}
