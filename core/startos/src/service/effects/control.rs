use std::str::FromStr;

use clap::builder::ValueParserFactory;
use models::FromStrParser;

use crate::service::effects::prelude::*;

pub async fn restart(
    context: EffectContext,
    ProcedureId { procedure_id }: ProcedureId,
) -> Result<(), Error> {
    let context = context.deref()?;
    context.restart(procedure_id).await?;
    Ok(())
}

pub async fn shutdown(
    context: EffectContext,
    ProcedureId { procedure_id }: ProcedureId,
) -> Result<(), Error> {
    let context = context.deref()?;
    context.stop(procedure_id).await?;
    Ok(())
}

#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum SetMainStatusStatus {
    Running,
    Stopped,
}
impl FromStr for SetMainStatusStatus {
    type Err = color_eyre::eyre::Report;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "running" => Ok(Self::Running),
            "stopped" => Ok(Self::Stopped),
            _ => Err(eyre!("unknown status {s}")),
        }
    }
}
impl ValueParserFactory for SetMainStatusStatus {
    type Parser = FromStrParser<Self>;
    fn value_parser() -> Self::Parser {
        FromStrParser::new()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SetMainStatus {
    status: SetMainStatusStatus,
}
pub async fn set_main_status(
    context: EffectContext,
    SetMainStatus { status }: SetMainStatus,
) -> Result<(), Error> {
    let context = context.deref()?;
    match status {
        SetMainStatusStatus::Running => context.seed.started(),
        SetMainStatusStatus::Stopped => context.seed.stopped(),
    }
    Ok(())
}
