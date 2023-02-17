use crate::{ActionId, PackageId};

#[derive(Debug, Clone)]
pub enum ProcedureName {
    Main, // Usually just run container
    CreateBackup,
    RestoreBackup,
    GetConfig,
    SetConfig,
    Properties,
    Init,
    Uninit,
    Check(PackageId),
    AutoConfig(PackageId),
    Action(ActionId),
}

impl ProcedureName {
    pub fn js_function_name(&self) -> Option<String> {
        match self {
            ProcedureName::Main => Some("/main".to_string()),
            ProcedureName::CreateBackup => Some("/createBackup".to_string()),
            ProcedureName::RestoreBackup => Some("/restoreBackup".to_string()),
            ProcedureName::GetConfig => Some("/getConfig".to_string()),
            ProcedureName::SetConfig => Some("/setConfig".to_string()),
            ProcedureName::Init => Some("/init".to_string()),
            ProcedureName::Uninit => Some("/uninit".to_string()),
            ProcedureName::Properties => Some("/properties".to_string()),
            ProcedureName::Action(id) => Some(format!("/action/{}", id)),
            ProcedureName::Check(id) => Some(format!("/dependencies/{}/check", id)),
            ProcedureName::AutoConfig(id) => Some(format!("/dependencies/{}/autoConfigure", id)),
        }
    }
}
