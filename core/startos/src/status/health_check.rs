pub use models::HealthCheckId;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq, TS)]
#[serde(rename_all = "camelCase")]
pub struct HealthCheckResult {
    pub name: String,
    #[serde(flatten)]
    pub kind: HealthCheckResultKind,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq, TS)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "result")]
pub enum HealthCheckResultKind {
    Success { message: Option<String> },
    Disabled { message: Option<String> },
    Starting { message: Option<String> },
    Loading { message: String },
    Failure { message: String },
}
impl std::fmt::Display for HealthCheckResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = &self.name;
        match &self.kind {
            HealthCheckResultKind::Success { message } => {
                if let Some(message) = message {
                    write!(f, "{name}: Succeeded ({message})")
                } else {
                    write!(f, "{name}: Succeeded")
                }
            }
            HealthCheckResultKind::Disabled { message } => {
                if let Some(message) = message {
                    write!(f, "{name}: Disabled ({message})")
                } else {
                    write!(f, "{name}: Disabled")
                }
            }
            HealthCheckResultKind::Starting { message } => {
                if let Some(message) = message {
                    write!(f, "{name}: Starting ({message})")
                } else {
                    write!(f, "{name}: Starting")
                }
            }
            HealthCheckResultKind::Loading { message } => write!(f, "{name}: Loading ({message})"),
            HealthCheckResultKind::Failure { message } => write!(f, "{name}: Failed ({message})"),
        }
    }
}
