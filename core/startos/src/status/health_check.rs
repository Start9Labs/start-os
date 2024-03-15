pub use models::HealthCheckId;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
#[serde(tag = "result")]
pub enum HealthCheckResult {
    Success,
    Disabled,
    Starting,
    Loading { message: String },
    Failure { error: String },
}
impl std::fmt::Display for HealthCheckResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HealthCheckResult::Success => write!(f, "Succeeded"),
            HealthCheckResult::Disabled => write!(f, "Disabled"),
            HealthCheckResult::Starting => write!(f, "Starting"),
            HealthCheckResult::Loading { message } => write!(f, "Loading ({})", message),
            HealthCheckResult::Failure { error } => write!(f, "Failed ({})", error),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq, ts_rs::TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum HealthCheckString {
    Passing,
    Disabled,
    Starting,
    Warning,
    Failure,
}
