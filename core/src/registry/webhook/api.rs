use std::collections::BTreeSet;

use chrono::{DateTime, Utc};
use clap::Parser;
use ed25519_dalek::VerifyingKey;
use rpc_toolkit::{Context, HandlerExt, ParentHandler, from_fn_async};
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

use super::{DeliveryAttempt, WebhookEventRecord, dispatcher};
use crate::context::CliContext;
use crate::prelude::*;
use crate::registry::context::RegistryContext;
use crate::rpc_continuations::Guid;
use crate::util::serde::{HandlerExtSerde, Pem};

pub fn webhook_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "subscriber",
            subscriber_api::<C>().with_about("about.commands-registry-webhook-subscriber"),
        )
        .subcommand(
            "list",
            from_fn_async(list_events)
                .with_metadata("admin", Value::Bool(true))
                .with_display_serializable()
                .with_about("about.list-webhook-events")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "replay",
            from_fn_async(replay_event)
                .with_metadata("admin", Value::Bool(true))
                .with_display_serializable()
                .with_about("about.replay-webhook-event")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "pubkey",
            from_fn_async(pubkey)
                .with_metadata("authenticated", Value::Bool(false))
                .with_display_serializable()
                .with_about("about.get-webhook-pubkey")
                .with_call_remote::<CliContext>(),
        )
}

fn subscriber_api<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "add",
            from_fn_async(add_subscriber)
                .with_metadata("admin", Value::Bool(true))
                .with_display_serializable()
                .with_about("about.add-webhook-subscriber")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "remove",
            from_fn_async(remove_subscriber)
                .with_metadata("admin", Value::Bool(true))
                .with_display_serializable()
                .with_about("about.remove-webhook-subscriber")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "list",
            from_fn_async(list_subscribers)
                .with_metadata("admin", Value::Bool(true))
                .with_display_serializable()
                .with_about("about.list-webhook-subscribers")
                .with_call_remote::<CliContext>(),
        )
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SubscriberParams {
    #[ts(type = "string")]
    #[arg(help = "help.arg.webhook-subscriber-url")]
    pub url: Url,
}

pub async fn add_subscriber(
    ctx: RegistryContext,
    SubscriberParams { url }: SubscriberParams,
) -> Result<(), Error> {
    if !matches!(url.scheme(), "http" | "https") {
        return Err(Error::new(
            eyre!("{}", t!("registry.webhook.invalid-subscriber-url")),
            ErrorKind::InvalidRequest,
        ));
    }
    ctx.db
        .mutate(|db| db.as_webhook_subscribers_mut().mutate(|s| Ok(s.insert(url))))
        .await
        .result?;
    Ok(())
}

pub async fn remove_subscriber(
    ctx: RegistryContext,
    SubscriberParams { url }: SubscriberParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_webhook_subscribers_mut()
                .mutate(|s| Ok(s.remove(&url)))
        })
        .await
        .result?;
    Ok(())
}

pub async fn list_subscribers(ctx: RegistryContext) -> Result<BTreeSet<Url>, Error> {
    ctx.db.peek().await.as_webhook_subscribers().de()
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ListEventsParams {
    #[arg(long, help = "help.arg.webhook-topic-filter")]
    pub topic: Option<String>,
    #[ts(type = "string | null")]
    #[arg(long, help = "help.arg.webhook-since")]
    pub since: Option<DateTime<Utc>>,
    #[arg(long, help = "help.arg.webhook-limit")]
    pub limit: Option<usize>,
}

pub async fn list_events(
    ctx: RegistryContext,
    ListEventsParams {
        topic,
        since,
        limit,
    }: ListEventsParams,
) -> Result<Vec<WebhookEventRecord>, Error> {
    let peek = ctx.db.peek().await;
    let mut records: Vec<WebhookEventRecord> = peek
        .as_webhook_log()
        .as_events()
        .as_entries()?
        .into_iter()
        .map(|(_, rec)| rec.de())
        .collect::<Result<_, _>>()?;
    records.sort_by(|a, b| b.event.occurred_at.cmp(&a.event.occurred_at));
    records.retain(|r| {
        topic.as_ref().map_or(true, |t| r.event.topic() == t)
            && since.map_or(true, |s| r.event.occurred_at >= s)
    });
    if let Some(n) = limit {
        records.truncate(n);
    }
    Ok(records)
}

#[derive(Debug, Deserialize, Serialize, Parser, TS)]
#[group(skip)]
#[command(rename_all = "kebab-case")]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ReplayEventParams {
    #[arg(help = "help.arg.webhook-event-id")]
    pub event_id: Guid,
}

pub async fn replay_event(
    ctx: RegistryContext,
    ReplayEventParams { event_id }: ReplayEventParams,
) -> Result<Vec<DeliveryAttempt>, Error> {
    let peek = ctx.db.peek().await;
    let subscribers: BTreeSet<Url> = peek.as_webhook_subscribers().de()?;
    if subscribers.is_empty() {
        return Err(Error::new(
            eyre!("{}", t!("registry.webhook.no-subscriber")),
            ErrorKind::InvalidRequest,
        ));
    }
    let event = peek
        .as_webhook_log()
        .as_events()
        .as_idx(&event_id)
        .ok_or_else(|| {
            Error::new(
                eyre!("{}", t!("registry.webhook.event-not-found", id = event_id.as_ref())),
                ErrorKind::NotFound,
            )
        })?
        .as_event()
        .de()?;

    let mut attempts = Vec::with_capacity(subscribers.len());
    for subscriber in &subscribers {
        let attempt =
            dispatcher::deliver(&ctx.client, subscriber, &ctx.webhook_signing_key, &event, true)
                .await;
        dispatcher::append_attempt(&ctx, &event_id, attempt.clone()).await;
        attempts.push(attempt);
    }
    Ok(attempts)
}

pub async fn pubkey(ctx: RegistryContext) -> Result<Pem<VerifyingKey>, Error> {
    Ok(Pem(ctx.webhook_signing_key.verifying_key()))
}
