use std::collections::BTreeMap;
use std::fmt;
use std::str::FromStr;

use chrono::{DateTime, Utc};
use clap::builder::ValueParserFactory;
use clap::Parser;
use color_eyre::eyre::eyre;
use imbl_value::InternedString;
use models::PackageId;
use rpc_toolkit::{command, from_fn_async, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tracing::instrument;
use ts_rs::TS;

use crate::backup::BackupReport;
use crate::context::{CliContext, RpcContext};
use crate::db::model::DatabaseModel;
use crate::prelude::*;
use crate::util::clap::FromStrParser;
use crate::util::serde::HandlerExtSerde;

// #[command(subcommands(list, delete, delete_before, create))]
pub fn notification() -> ParentHandler {
    ParentHandler::new()
        .subcommand(
            "list",
            from_fn_async(list)
                .with_display_serializable()
                .with_remote_cli::<CliContext>(),
        )
        .subcommand(
            "delete",
            from_fn_async(delete)
                .no_display()
                .with_remote_cli::<CliContext>(),
        )
        .subcommand(
            "delete-before",
            from_fn_async(delete_before)
                .no_display()
                .with_remote_cli::<CliContext>(),
        )
        .subcommand(
            "create",
            from_fn_async(create)
                .no_display()
                .with_remote_cli::<CliContext>(),
        )
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct ListParams {
    #[ts(type = "number | null")]
    before: Option<u32>,
    #[ts(type = "number | null")]
    limit: Option<usize>,
}
// #[command(display(display_serializable))]
#[instrument(skip_all)]
pub async fn list(
    ctx: RpcContext,
    ListParams { before, limit }: ListParams,
) -> Result<Vec<NotificationWithId>, Error> {
    ctx.db
        .mutate(|db| {
            let limit = limit.unwrap_or(40);
            match before {
                None => {
                    let records = db
                        .as_private()
                        .as_notifications()
                        .as_entries()?
                        .into_iter()
                        .take(limit);
                    let notifs = records
                        .into_iter()
                        .map(|(id, notification)| {
                            Ok(NotificationWithId {
                                id,
                                notification: notification.de()?,
                            })
                        })
                        .collect::<Result<Vec<NotificationWithId>, Error>>()?;
                    db.as_public_mut()
                        .as_server_info_mut()
                        .as_unread_notification_count_mut()
                        .ser(&0)?;
                    Ok(notifs)
                }
                Some(before) => {
                    let records = db
                        .as_private()
                        .as_notifications()
                        .as_entries()?
                        .into_iter()
                        .filter(|(id, _)| *id < before)
                        .take(limit);
                    records
                        .into_iter()
                        .map(|(id, notification)| {
                            Ok(NotificationWithId {
                                id,
                                notification: notification.de()?,
                            })
                        })
                        .collect()
                }
            }
        })
        .await
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct DeleteParams {
    #[ts(type = "number")]
    id: u32,
}

pub async fn delete(ctx: RpcContext, DeleteParams { id }: DeleteParams) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_private_mut().as_notifications_mut().remove(&id)?;
            Ok(())
        })
        .await
}
#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct DeleteBeforeParams {
    #[ts(type = "number")]
    before: u32,
}

pub async fn delete_before(
    ctx: RpcContext,
    DeleteBeforeParams { before }: DeleteBeforeParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            for id in db.as_private().as_notifications().keys()? {
                if id < before {
                    db.as_private_mut().as_notifications_mut().remove(&id)?;
                }
            }
            Ok(())
        })
        .await
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct CreateParams {
    package: Option<PackageId>,
    level: NotificationLevel,
    title: String,
    message: String,
}

pub async fn create(
    ctx: RpcContext,
    CreateParams {
        package,
        level,
        title,
        message,
    }: CreateParams,
) -> Result<(), Error> {
    ctx.db
        .mutate(|db| notify(db, package, level, title, message, ()))
        .await
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub enum NotificationLevel {
    Success,
    Info,
    Warning,
    Error,
}
impl fmt::Display for NotificationLevel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NotificationLevel::Success => write!(f, "success"),
            NotificationLevel::Info => write!(f, "info"),
            NotificationLevel::Warning => write!(f, "warning"),
            NotificationLevel::Error => write!(f, "error"),
        }
    }
}
impl ValueParserFactory for NotificationLevel {
    type Parser = FromStrParser<Self>;
    fn value_parser() -> Self::Parser {
        FromStrParser::new()
    }
}

pub struct InvalidNotificationLevel(String);
impl From<InvalidNotificationLevel> for crate::Error {
    fn from(val: InvalidNotificationLevel) -> Self {
        Error::new(
            eyre!("Invalid Notification Level: {}", val.0),
            ErrorKind::ParseDbField,
        )
    }
}
impl FromStr for NotificationLevel {
    type Err = InvalidNotificationLevel;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            s if s == "success" => Ok(NotificationLevel::Success),
            s if s == "info" => Ok(NotificationLevel::Info),
            s if s == "warning" => Ok(NotificationLevel::Warning),
            s if s == "error" => Ok(NotificationLevel::Error),
            s => Err(InvalidNotificationLevel(s.to_string())),
        }
    }
}
impl fmt::Display for InvalidNotificationLevel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Invalid Notification Level: {}", self.0)
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Notifications(pub BTreeMap<u32, Notification>);
impl Notifications {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }
}
impl Map for Notifications {
    type Key = u32;
    type Value = Notification;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Self::key_string(key)
    }
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(InternedString::from_display(key))
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Notification {
    package_id: Option<PackageId>,
    created_at: DateTime<Utc>,
    code: u32,
    level: NotificationLevel,
    title: String,
    message: String,
    data: Value,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NotificationWithId {
    id: u32,
    #[serde(flatten)]
    notification: Notification,
}

pub trait NotificationType:
    serde::Serialize + for<'de> serde::Deserialize<'de> + std::fmt::Debug
{
    const CODE: u32;
}

impl NotificationType for () {
    const CODE: u32 = 0;
}
impl NotificationType for BackupReport {
    const CODE: u32 = 1;
}

#[instrument(skip(subtype, db))]
pub fn notify<T: NotificationType>(
    db: &mut DatabaseModel,
    package_id: Option<PackageId>,
    level: NotificationLevel,
    title: String,
    message: String,
    subtype: T,
) -> Result<(), Error> {
    let data = to_value(&subtype)?;
    db.as_public_mut()
        .as_server_info_mut()
        .as_unread_notification_count_mut()
        .mutate(|c| {
            *c += 1;
            Ok(())
        })?;
    let id = db
        .as_private()
        .as_notifications()
        .keys()?
        .into_iter()
        .max()
        .map_or(0, |id| id + 1);
    db.as_private_mut().as_notifications_mut().insert(
        &id,
        &Notification {
            package_id,
            created_at: Utc::now(),
            code: T::CODE,
            level,
            title,
            message,
            data,
        },
    )
}

#[test]
fn serialization() {
    println!(
        "{}",
        serde_json::json!({ "test": "abcdefg", "num": 32, "nested": { "inner": null, "xyz": [0,2,4]}})
    )
}
