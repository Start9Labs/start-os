use std::str::FromStr;

use clap::builder::ValueParserFactory;
pub use models::HealthCheckId;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::util::clap::FromStrParser;

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq, TS)]
#[serde(rename_all = "camelCase")]
pub struct HealthCheckResult {
    pub name: String,
    #[serde(flatten)]
    pub kind: HealthCheckResultKind,
}
// healthCheckName:kind:message OR healthCheckName:kind
impl FromStr for HealthCheckResult {
    type Err = color_eyre::eyre::Report;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let from_parts = |name: &str, kind: &str, message: Option<&str>| {
            let message = message.map(|x| x.to_string());
            let kind = match kind {
                "success" => HealthCheckResultKind::Success { message },
                "disabled" => HealthCheckResultKind::Disabled { message },
                "starting" => HealthCheckResultKind::Starting { message },
                "loading" => HealthCheckResultKind::Loading {
                    message: message.unwrap_or_default(),
                },
                "failure" => HealthCheckResultKind::Failure {
                    message: message.unwrap_or_default(),
                },
                _ => return Err(color_eyre::eyre::eyre!("Invalid health check kind")),
            };
            Ok(Self {
                name: name.to_string(),
                kind,
            })
        };
        let parts = s.split(':').collect::<Vec<_>>();
        match &*parts {
            [name, kind, message] => from_parts(name, kind, Some(message)),
            [name, kind] => from_parts(name, kind, None),
            _ => Err(color_eyre::eyre::eyre!(
                "Could not match the shape of the result ${parts:?}"
            )),
        }
    }
}
impl ValueParserFactory for HealthCheckResult {
    type Parser = FromStrParser<Self>;
    fn value_parser() -> Self::Parser {
        FromStrParser::new()
    }
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
