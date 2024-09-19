use models::{Error, ResultExt};
use serde::{Deserialize, Serialize};
use tokio::process::Command;
use ts_rs::TS;

use crate::util::Invoke;

const KNOWN_CLASSES: &[&str] = &["processor", "display"];

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(tag = "class")]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum LshwDevice {
    Processor(LshwProcessor),
    Display(LshwDisplay),
}
impl LshwDevice {
    pub fn class(&self) -> &'static str {
        match self {
            Self::Processor(_) => "processor",
            Self::Display(_) => "display",
        }
    }
    pub fn product(&self) -> &str {
        match self {
            Self::Processor(hw) => hw.product.as_str(),
            Self::Display(hw) => hw.product.as_str(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
pub struct LshwProcessor {
    pub product: String,
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
pub struct LshwDisplay {
    pub product: String,
}

pub async fn lshw() -> Result<Vec<LshwDevice>, Error> {
    let mut cmd = Command::new("lshw");
    cmd.arg("-json");
    for class in KNOWN_CLASSES {
        cmd.arg("-class").arg(*class);
    }
    Ok(
        serde_json::from_slice::<Vec<serde_json::Value>>(
            &cmd.invoke(crate::ErrorKind::Lshw).await?,
        )
        .with_kind(crate::ErrorKind::Deserialization)?
        .into_iter()
        .filter_map(|v| match serde_json::from_value(v) {
            Ok(a) => Some(a),
            Err(e) => {
                tracing::error!("Failed to parse lshw output: {e}");
                tracing::debug!("{e:?}");
                None
            }
        })
        .collect(),
    )
}
