use crate::{ActionId, HealthCheckId, PackageId};

#[derive(Debug, Clone)]
pub enum ProcedureName {
    Main, // Usually just run container
    CreateBackup,
    RestoreBackup,
    GetConfig,
    SetConfig,
    Migration,
    Properties,
    Check(PackageId),
    AutoConfig(PackageId),
    Health(HealthCheckId),
    Action(ActionId),
}

impl ProcedureName {
    pub fn docker_name(&self) -> Option<String> {
        match self {
            ProcedureName::Main => None,
            ProcedureName::CreateBackup => Some("CreateBackup".to_string()),
            ProcedureName::RestoreBackup => Some("RestoreBackup".to_string()),
            ProcedureName::GetConfig => Some("GetConfig".to_string()),
            ProcedureName::SetConfig => Some("SetConfig".to_string()),
            ProcedureName::Migration => Some("Migration".to_string()),
            ProcedureName::Properties => Some(format!("Properties-{}", rand::random::<u64>())),
            ProcedureName::Health(id) => Some(format!("{}Health", id)),
            ProcedureName::Action(id) => Some(format!("{}Action", id)),
            ProcedureName::Check(_) => None,
            ProcedureName::AutoConfig(_) => None,
        }
    }
    pub fn js_function_name(&self) -> String {
        match self {
            ProcedureName::Main => "/main".to_string(),
            ProcedureName::CreateBackup => "/createBackup".to_string(),
            ProcedureName::RestoreBackup => "/restoreBackup".to_string(),
            ProcedureName::GetConfig => "/getConfig".to_string(),
            ProcedureName::SetConfig => "/setConfig".to_string(),
            ProcedureName::Migration => "/migration".to_string(),
            ProcedureName::Properties => "/properties".to_string(),
            ProcedureName::Health(id) => format!("/health/{}", id),
            ProcedureName::Action(id) => format!("/action/{}", id),
            ProcedureName::Check(id) => format!("/dependencies/{}/check", id),
            ProcedureName::AutoConfig(id) => format!("/dependencies/{}/autoConfigure", id),
        }
    }
}
