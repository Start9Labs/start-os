use models::{Error, ResultExt};
use serde::{Deserialize, Serialize};
use tokio::process::Command;

use crate::util::Invoke;

const KNOWN_CLASSES: &[&str] = &["processor", "display"];

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "class")]
#[serde(rename_all = "kebab-case")]
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

#[derive(Debug, Deserialize, Serialize)]
pub struct LshwProcessor {
    pub product: String,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LshwDisplay {
    pub product: String,
}

pub async fn lshw() -> Result<Vec<LshwDevice>, Error> {
    let mut cmd = Command::new("lshw");
    cmd.arg("-json");
    for class in KNOWN_CLASSES {
        cmd.arg("-class").arg(*class);
    }
    serde_json::from_slice(&cmd.invoke(crate::ErrorKind::Lshw).await?)
        .with_kind(crate::ErrorKind::Deserialization)
}
