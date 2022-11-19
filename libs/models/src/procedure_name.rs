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
    LongRunning,
    Check(PackageId),
    AutoConfig(PackageId),
    Health(HealthCheckId),
    Action(ActionId),
    Signal,
}

impl ProcedureName {
    pub fn docker_name(&self) -> Option<String> {
        match self {
            ProcedureName::Main => None,
            ProcedureName::LongRunning => None,
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
            ProcedureName::Signal => None,
        }
    }
    pub fn js_function_name(&self) -> Option<String> {
        match self {
            ProcedureName::Main => Some("/main".to_string()),
            ProcedureName::LongRunning => None,
            ProcedureName::CreateBackup => Some("/createBackup".to_string()),
            ProcedureName::RestoreBackup => Some("/restoreBackup".to_string()),
            ProcedureName::GetConfig => Some("/getConfig".to_string()),
            ProcedureName::SetConfig => Some("/setConfig".to_string()),
            ProcedureName::Migration => Some("/migration".to_string()),
            ProcedureName::Properties => Some("/properties".to_string()),
            ProcedureName::Health(id) => Some(format!("/health/{}", id)),
            ProcedureName::Action(id) => Some(format!("/action/{}", id)),
            ProcedureName::Check(id) => Some(format!("/dependencies/{}/check", id)),
            ProcedureName::AutoConfig(id) => Some(format!("/dependencies/{}/autoConfigure", id)),
            ProcedureName::Signal => Some("/handleSignal".to_string()),
        }
    }
}
