pub use clap::Parser;
pub use serde::{Deserialize, Serialize};
pub use ts_rs::TS;

pub use crate::prelude::*;
use crate::rpc_continuations::Guid;
pub(super) use crate::service::effects::context::EffectContext;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ProcedureId {
    #[serde(default)]
    #[arg(default_value_t, long)]
    pub procedure_id: Guid,
}
