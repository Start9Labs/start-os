use std::path::PathBuf;

use clap::Parser;
use serde::{Deserialize, Serialize};

pub mod builder;
pub mod docker;
pub mod git_hash;
pub mod header;
pub mod manifest;
pub mod reader;

pub const SIG_CONTEXT: &[u8] = b"s9pk";

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct VerifyParams {
    pub path: PathBuf,
}
