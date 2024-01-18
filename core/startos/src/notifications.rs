use std::collections::HashMap;
use std::fmt;
use std::str::FromStr;

use chrono::{DateTime, TimeZone, Utc};
use clap::builder::ValueParserFactory;
use clap::Parser;
use color_eyre::eyre::eyre;
use models::PackageId;
use rpc_toolkit::{command, from_fn_async, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use sqlx::PgPool;
use tokio::sync::Mutex;
use tracing::instrument;

use crate::backup::BackupReport;
use crate::context::{CliContext, RpcContext};
use crate::prelude::*;
use crate::util::clap::FromStrParser;
use crate::util::serde::HandlerExtSerde;
use crate::{Error, ErrorKind, ResultExt};

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

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct ListParams {
    before: Option<i32>,

    limit: Option<u32>,
}
// #[command(display(display_serializable))]
#[instrument(skip_all)]
pub async fn list(
    ctx: RpcContext,
    ListParams { before, limit }: ListParams,
) -> Result<Vec<Notification>, Error> {
    let limit = limit.unwrap_or(40);
    match before {
        None => {
            let records = sqlx::query!(
                "SELECT id, package_id, created_at, code, level, title, message, data FROM notifications ORDER BY id DESC LIMIT $1",
                limit as i64
            ).fetch_all(&ctx.secret_store).await?;
            let notifs = records
                .into_iter()
                .map(|r| {
                    Ok(Notification {
                        id: r.id as u32,
                        package_id: r.package_id.and_then(|p| p.parse().ok()),
                        created_at: Utc.from_utc_datetime(&r.created_at),
                        code: r.code as u32,
                        level: match r.level.parse::<NotificationLevel>() {
                            Ok(a) => a,
                            Err(e) => return Err(e.into()),
                        },
                        title: r.title,
                        message: r.message,
                        data: match r.data {
                            None => serde_json::Value::Null,
                            Some(v) => match v.parse::<serde_json::Value>() {
                                Ok(a) => a,
                                Err(e) => {
                                    return Err(Error::new(
                                        eyre!("Invalid Notification Data: {}", e),
                                        ErrorKind::ParseDbField,
                                    ))
                                }
                            },
                        },
                    })
                })
                .collect::<Result<Vec<Notification>, Error>>()?;

            ctx.db
                .mutate(|d| {
                    d.as_server_info_mut()
                        .as_unread_notification_count_mut()
                        .ser(&0)
                })
                .await?;
            Ok(notifs)
        }
        Some(before) => {
            let records = sqlx::query!(
                "SELECT id, package_id, created_at, code, level, title, message, data FROM notifications WHERE id < $1 ORDER BY id DESC LIMIT $2",
                before,
                limit as i64
            ).fetch_all(&ctx.secret_store).await?;
            let res = records
                .into_iter()
                .map(|r| {
                    Ok(Notification {
                        id: r.id as u32,
                        package_id: r.package_id.and_then(|p| p.parse().ok()),
                        created_at: Utc.from_utc_datetime(&r.created_at),
                        code: r.code as u32,
                        level: match r.level.parse::<NotificationLevel>() {
                            Ok(a) => a,
                            Err(e) => return Err(e.into()),
                        },
                        title: r.title,
                        message: r.message,
                        data: match r.data {
                            None => serde_json::Value::Null,
                            Some(v) => match v.parse::<serde_json::Value>() {
                                Ok(a) => a,
                                Err(e) => {
                                    return Err(Error::new(
                                        eyre!("Invalid Notification Data: {}", e),
                                        ErrorKind::ParseDbField,
                                    ))
                                }
                            },
                        },
                    })
                })
                .collect::<Result<Vec<Notification>, Error>>()?;
            Ok(res)
        }
    }
}

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct DeleteParams {
    id: i32,
}

pub async fn delete(ctx: RpcContext, DeleteParams { id }: DeleteParams) -> Result<(), Error> {
    sqlx::query!("DELETE FROM notifications WHERE id = $1", id)
        .execute(&ctx.secret_store)
        .await?;
    Ok(())
}
#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
#[command(rename_all = "kebab-case")]
pub struct DeleteBeforeParams {
    before: i32,
}

pub async fn delete_before(
    ctx: RpcContext,
    DeleteBeforeParams { before }: DeleteBeforeParams,
) -> Result<(), Error> {
    sqlx::query!("DELETE FROM notifications WHERE id < $1", before)
        .execute(&ctx.secret_store)
        .await?;
    Ok(())
}
#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "kebab-case")]
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
    ctx.notification_manager
        .notify(ctx.db.clone(), package, level, title, message, (), None)
        .await
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
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
#[derive(Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Notification {
    id: u32,
    package_id: Option<PackageId>, // TODO change for package id newtype
    created_at: DateTime<Utc>,
    code: u32,
    level: NotificationLevel,
    title: String,
    message: String,
    data: serde_json::Value,
}

pub trait NotificationType:
    serde::Serialize + for<'de> serde::Deserialize<'de> + std::fmt::Debug
{
    const CODE: i32;
}

impl NotificationType for () {
    const CODE: i32 = 0;
}
impl NotificationType for BackupReport {
    const CODE: i32 = 1;
}

pub struct NotificationManager {
    sqlite: PgPool,
    cache: Mutex<HashMap<(Option<PackageId>, NotificationLevel, String), i64>>,
}
impl NotificationManager {
    pub fn new(sqlite: PgPool) -> Self {
        NotificationManager {
            sqlite,
            cache: Mutex::new(HashMap::new()),
        }
    }
    #[instrument(skip(db, subtype, self))]
    pub async fn notify<T: NotificationType>(
        &self,
        db: PatchDb,
        package_id: Option<PackageId>,
        level: NotificationLevel,
        title: String,
        message: String,
        subtype: T,
        debounce_interval: Option<u32>,
    ) -> Result<(), Error> {
        let peek = db.peek().await;
        if !self
            .should_notify(&package_id, &level, &title, debounce_interval)
            .await
        {
            return Ok(());
        }
        let mut count = peek.as_server_info().as_unread_notification_count().de()?;
        let sql_package_id = package_id.as_ref().map(|p| &**p);
        let sql_code = T::CODE;
        let sql_level = format!("{}", level);
        let sql_data =
            serde_json::to_string(&subtype).with_kind(crate::ErrorKind::Serialization)?;
        sqlx::query!(
        "INSERT INTO notifications (package_id, code, level, title, message, data) VALUES ($1, $2, $3, $4, $5, $6)",
        sql_package_id,
        sql_code as i32,
        sql_level,
        title,
        message,
        sql_data
    ).execute(&self.sqlite).await?;
        count += 1;
        db.mutate(|db| {
            db.as_server_info_mut()
                .as_unread_notification_count_mut()
                .ser(&count)
        })
        .await
    }
    async fn should_notify(
        &self,
        package_id: &Option<PackageId>,
        level: &NotificationLevel,
        title: &String,
        debounce_interval: Option<u32>,
    ) -> bool {
        let mut guard = self.cache.lock().await;
        let k = (package_id.clone(), level.clone(), title.clone());
        let v = (*guard).get(&k);
        match v {
            None => {
                (*guard).insert(k, Utc::now().timestamp());
                true
            }
            Some(last_issued) => match debounce_interval {
                None => {
                    (*guard).insert(k, Utc::now().timestamp());
                    true
                }
                Some(interval) => {
                    if last_issued + interval as i64 > Utc::now().timestamp() {
                        false
                    } else {
                        (*guard).insert(k, Utc::now().timestamp());
                        true
                    }
                }
            },
        }
    }
}

#[test]
fn serialization() {
    println!(
        "{}",
        serde_json::json!({ "test": "abcdefg", "num": 32, "nested": { "inner": null, "xyz": [0,2,4]}})
    )
}
