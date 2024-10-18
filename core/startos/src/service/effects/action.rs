use std::collections::BTreeSet;

use models::{ActionId, PackageId, ReplayId};
use rpc_toolkit::{from_fn_async, Context, HandlerExt, ParentHandler};

use crate::action::{display_action_result, ActionInput, ActionResult};
use crate::db::model::package::{
    ActionMetadata, ActionRequest, ActionRequestCondition, ActionRequestEntry, ActionRequestTrigger,
};
use crate::rpc_continuations::Guid;
use crate::service::cli::ContainerCliContext;
use crate::service::effects::prelude::*;
use crate::util::serde::HandlerExtSerde;

pub fn action_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("export", from_fn_async(export_action).no_cli())
        .subcommand(
            "clear",
            from_fn_async(clear_actions)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "get-input",
            from_fn_async(get_action_input)
                .with_display_serializable()
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand(
            "run",
            from_fn_async(run_action)
                .with_display_serializable()
                .with_custom_display_fn(|args, res| Ok(display_action_result(args.params, res)))
                .with_call_remote::<ContainerCliContext>(),
        )
        .subcommand("request", from_fn_async(request_action).no_cli())
        .subcommand(
            "clear-requests",
            from_fn_async(clear_action_requests)
                .no_display()
                .with_call_remote::<ContainerCliContext>(),
        )
}

#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct ExportActionParams {
    id: ActionId,
    metadata: ActionMetadata,
}
pub async fn export_action(
    context: EffectContext,
    ExportActionParams { id, metadata }: ExportActionParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    let package_id = context.seed.id.clone();
    context
        .seed
        .ctx
        .db
        .mutate(|db| {
            let model = db
                .as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package_id)
                .or_not_found(&package_id)?
                .as_actions_mut();
            let mut value = model.de()?;
            value.insert(id, metadata);
            model.ser(&value)
        })
        .await?;
    Ok(())
}

#[derive(Debug, Clone, Serialize, Deserialize, TS, Parser)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct ClearActionsParams {
    #[arg(long)]
    pub except: Vec<ActionId>,
}

async fn clear_actions(
    context: EffectContext,
    ClearActionsParams { except }: ClearActionsParams,
) -> Result<(), Error> {
    let except: BTreeSet<_> = except.into_iter().collect();
    let context = context.deref()?;
    let package_id = context.seed.id.clone();
    context
        .seed
        .ctx
        .db
        .mutate(|db| {
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package_id)
                .or_not_found(&package_id)?
                .as_actions_mut()
                .mutate(|a| Ok(a.retain(|e, _| except.contains(e))))
        })
        .await?;
    Ok(())
}

#[derive(Debug, Clone, Serialize, Deserialize, TS, Parser)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct GetActionInputParams {
    #[serde(default)]
    #[ts(skip)]
    #[arg(skip)]
    procedure_id: Guid,
    #[ts(optional)]
    package_id: Option<PackageId>,
    action_id: ActionId,
}
async fn get_action_input(
    context: EffectContext,
    GetActionInputParams {
        procedure_id,
        package_id,
        action_id,
    }: GetActionInputParams,
) -> Result<Option<ActionInput>, Error> {
    let context = context.deref()?;

    if let Some(package_id) = package_id {
        context
            .seed
            .ctx
            .services
            .get(&package_id)
            .await
            .as_ref()
            .or_not_found(&package_id)?
            .get_action_input(procedure_id, action_id)
            .await
    } else {
        context.get_action_input(procedure_id, action_id).await
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, TS, Parser)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RunActionParams {
    #[serde(default)]
    #[ts(skip)]
    #[arg(skip)]
    procedure_id: Guid,
    #[ts(optional)]
    package_id: Option<PackageId>,
    action_id: ActionId,
    #[ts(type = "any")]
    input: Value,
}
async fn run_action(
    context: EffectContext,
    RunActionParams {
        procedure_id,
        package_id,
        action_id,
        input,
    }: RunActionParams,
) -> Result<Option<ActionResult>, Error> {
    let context = context.deref()?;

    let package_id = package_id.as_ref().unwrap_or(&context.seed.id);

    if package_id != &context.seed.id {
        return Err(Error::new(
            eyre!("calling actions on other packages is unsupported at this time"),
            ErrorKind::InvalidRequest,
        ));
        context
            .seed
            .ctx
            .services
            .get(&package_id)
            .await
            .as_ref()
            .or_not_found(&package_id)?
            .run_action(procedure_id, action_id, input)
            .await
    } else {
        context.run_action(procedure_id, action_id, input).await
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RequestActionParams {
    #[serde(default)]
    #[ts(skip)]
    procedure_id: Guid,
    replay_id: ReplayId,
    #[serde(flatten)]
    request: ActionRequest,
}
async fn request_action(
    context: EffectContext,
    RequestActionParams {
        procedure_id,
        replay_id,
        request,
    }: RequestActionParams,
) -> Result<(), Error> {
    let context = context.deref()?;

    let src_id = &context.seed.id;
    let active = match &request.when {
        Some(ActionRequestTrigger { once, condition }) => match condition {
            ActionRequestCondition::InputNotMatches => {
                let Some(input) = request.input.as_ref() else {
                    return Err(Error::new(
                        eyre!("input-not-matches trigger requires input to be specified"),
                        ErrorKind::InvalidRequest,
                    ));
                };
                if let Some(service) = context
                    .seed
                    .ctx
                    .services
                    .get(&request.package_id)
                    .await
                    .as_ref()
                {
                    let Some(prev) = service
                        .get_action_input(procedure_id, request.action_id.clone())
                        .await?
                    else {
                        return Err(Error::new(
                            eyre!(
                                "action {} of {} has no input",
                                request.action_id,
                                request.package_id
                            ),
                            ErrorKind::InvalidRequest,
                        ));
                    };
                    if input.matches(prev.value.as_ref()) {
                        if *once {
                            return Ok(());
                        } else {
                            false
                        }
                    } else {
                        true
                    }
                } else {
                    true // update when service is installed
                }
            }
        },
        None => true,
    };
    context
        .seed
        .ctx
        .db
        .mutate(|db| {
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(src_id)
                .or_not_found(src_id)?
                .as_requested_actions_mut()
                .insert(&replay_id, &ActionRequestEntry { active, request })
        })
        .await?;
    Ok(())
}

#[derive(Debug, Clone, Serialize, Deserialize, TS, Parser)]
#[ts(type = "{ only: string[] } | { except: string[] }")]
#[ts(export)]
pub struct ClearActionRequestsParams {
    #[arg(long, conflicts_with = "except")]
    pub only: Option<Vec<ReplayId>>,
    #[arg(long, conflicts_with = "only")]
    pub except: Option<Vec<ReplayId>>,
}

async fn clear_action_requests(
    context: EffectContext,
    ClearActionRequestsParams { only, except }: ClearActionRequestsParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    let package_id = context.seed.id.clone();
    let only = only.map(|only| only.into_iter().collect::<BTreeSet<_>>());
    let except = except.map(|except| except.into_iter().collect::<BTreeSet<_>>());
    context
        .seed
        .ctx
        .db
        .mutate(|db| {
            db.as_public_mut()
                .as_package_data_mut()
                .as_idx_mut(&package_id)
                .or_not_found(&package_id)?
                .as_requested_actions_mut()
                .mutate(|a| {
                    Ok(a.retain(|e, _| {
                        only.as_ref().map_or(true, |only| !only.contains(e))
                            && except.as_ref().map_or(true, |except| except.contains(e))
                    }))
                })
        })
        .await?;
    Ok(())
}
