use std::str::FromStr;

use clap::builder::ValueParserFactory;
use models::{FromStrParser, PackageId};

use crate::service::effects::prelude::*;
use crate::service::rpc::CallbackId;
use crate::status::MainStatus;

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

#[derive(Debug, Clone, Serialize, Deserialize, TS, Parser)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct GetStatusParams {
    #[ts(optional)]
    pub package_id: Option<PackageId>,
    #[ts(optional)]
    #[arg(skip)]
    pub callback: Option<CallbackId>,
}

pub async fn get_status(
    context: EffectContext,
    GetStatusParams {
        package_id,
        callback,
    }: GetStatusParams,
) -> Result<MainStatus, Error> {
    let context = context.deref()?;
    let id = package_id.unwrap_or_else(|| context.seed.id.clone());
    let db = context.seed.ctx.db.peek().await;
    let status = db
        .as_public()
        .as_package_data()
        .as_idx(&id)
        .or_not_found(&id)?
        .as_status()
        .de()?;

    if let Some(callback) = callback {
        let callback = callback.register(&context.seed.persistent_container);
        context.seed.ctx.callbacks.add_get_status(
            id,
            super::callbacks::CallbackHandler::new(&context, callback),
        );
    }

    Ok(status)
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
