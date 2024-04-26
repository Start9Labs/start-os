use std::ops::{Deref, DerefMut};
use std::process::Stdio;
use std::time::{Duration, UNIX_EPOCH};

use axum::extract::ws::{self, WebSocket};
use chrono::{DateTime, Utc};
use clap::Parser;
use color_eyre::eyre::eyre;
use futures::stream::BoxStream;
use futures::{FutureExt, Stream, StreamExt, TryStreamExt};
use models::PackageId;
use rpc_toolkit::yajrc::RpcError;
use rpc_toolkit::{command, from_fn_async, CallRemote, Empty, HandlerExt, ParentHandler};
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::{Child, Command};
use tokio_stream::wrappers::LinesStream;
use tokio_tungstenite::tungstenite::Message;
use tracing::instrument;

use crate::context::{CliContext, RpcContext};
use crate::core::rpc_continuations::{RequestGuid, RpcContinuation};
use crate::error::ResultExt;
use crate::lxc::ContainerId;
use crate::prelude::*;
use crate::util::serde::Reversible;
use crate::util::Invoke;

#[pin_project::pin_project]
pub struct LogStream {
    _child: Option<Child>,
    #[pin]
    entries: BoxStream<'static, Result<JournalctlEntry, Error>>,
}
impl Deref for LogStream {
    type Target = BoxStream<'static, Result<JournalctlEntry, Error>>;
    fn deref(&self) -> &Self::Target {
        &self.entries
    }
}
impl DerefMut for LogStream {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.entries
    }
}
impl Stream for LogStream {
    type Item = Result<JournalctlEntry, Error>;
    fn poll_next(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        let this = self.project();
        Stream::poll_next(this.entries, cx)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.entries.size_hint()
    }
}

#[instrument(skip_all)]
async fn ws_handler(
    first_entry: Option<LogEntry>,
    mut logs: LogStream,
    mut stream: WebSocket,
) -> Result<(), Error> {
    if let Some(first_entry) = first_entry {
        stream
            .send(ws::Message::Text(
                serde_json::to_string(&first_entry).with_kind(ErrorKind::Serialization)?,
            ))
            .await
            .with_kind(ErrorKind::Network)?;
    }

    let mut ws_closed = false;
    while let Some(entry) = tokio::select! {
        a = logs.try_next() => Some(a?),
        a = stream.try_next() => { a.with_kind(crate::ErrorKind::Network)?; ws_closed = true; None }
    } {
        if let Some(entry) = entry {
            let (_, log_entry) = entry.log_entry()?;
            stream
                .send(ws::Message::Text(
                    serde_json::to_string(&log_entry).with_kind(ErrorKind::Serialization)?,
                ))
                .await
                .with_kind(ErrorKind::Network)?;
        }
    }

    if !ws_closed {
        stream
            .send(ws::Message::Close(Some(ws::CloseFrame {
                code: ws::close_code::NORMAL,
                reason: "Log Stream Finished".into(),
            })))
            .await
            .with_kind(ErrorKind::Network)?;
        drop(stream);
    }

    Ok(())
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LogResponse {
    entries: Reversible<LogEntry>,
    start_cursor: Option<String>,
    end_cursor: Option<String>,
}
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LogFollowResponse {
    start_cursor: Option<String>,
    guid: RequestGuid,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct LogEntry {
    timestamp: DateTime<Utc>,
    message: String,
    boot_id: String,
}
impl std::fmt::Display for LogEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{} {}",
            self.timestamp
                .to_rfc3339_opts(chrono::SecondsFormat::Millis, true),
            self.message
        )
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct JournalctlEntry {
    #[serde(rename = "__REALTIME_TIMESTAMP")]
    pub timestamp: String,
    #[serde(rename = "MESSAGE")]
    #[serde(deserialize_with = "deserialize_log_message")]
    pub message: String,
    #[serde(rename = "__CURSOR")]
    pub cursor: String,
    #[serde(rename = "_BOOT_ID")]
    pub boot_id: String,
}
impl JournalctlEntry {
    fn log_entry(self) -> Result<(String, LogEntry), Error> {
        Ok((
            self.cursor,
            LogEntry {
                timestamp: DateTime::<Utc>::from(
                    UNIX_EPOCH + Duration::from_micros(self.timestamp.parse::<u64>()?),
                ),
                message: self.message,
                boot_id: self.boot_id,
            },
        ))
    }
}

fn deserialize_log_message<'de, D: serde::de::Deserializer<'de>>(
    deserializer: D,
) -> std::result::Result<String, D::Error> {
    struct Visitor;
    impl<'de> serde::de::Visitor<'de> for Visitor {
        type Value = String;
        fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(formatter, "a parsable string")
        }
        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(v.trim().to_owned())
        }
        fn visit_unit<E>(self) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(String::new())
        }
        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: serde::de::SeqAccess<'de>,
        {
            String::from_utf8(
                std::iter::repeat_with(|| seq.next_element::<u8>().transpose())
                    .take_while(|a| a.is_some())
                    .flatten()
                    .collect::<Result<Vec<u8>, _>>()?,
            )
            .map(|s| s.trim().to_owned())
            .map_err(serde::de::Error::custom)
        }
    }
    deserializer.deserialize_any(Visitor)
}

/// Defining how we are going to filter on a journalctl cli log.
/// Kernal: (-k --dmesg                 Show kernel message log from the current boot)
/// Unit: ( -u --unit=UNIT             Show logs from the specified unit
///     --user-unit=UNIT        Show logs from the specified user unit))
/// System: Unit is startd, but we also filter on the comm
/// Container: Filtering containers, like podman/docker is done by filtering on the CONTAINER_NAME
#[derive(Debug, Clone)]
pub enum LogSource {
    Kernel,
    Unit(&'static str),
    System,
    Container(ContainerId),
}

pub const SYSTEM_UNIT: &str = "startd";

#[derive(Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct LogsParam {
    id: PackageId,
    #[arg(short = 'l', long = "limit")]
    limit: Option<usize>,
    #[arg(short = 'c', long = "cursor")]
    cursor: Option<String>,
    #[arg(short = 'B', long = "before")]
    #[serde(default)]
    before: bool,
    #[arg(short = 'f', long = "follow")]
    #[serde(default)]
    follow: bool,
}

pub fn logs() -> ParentHandler<LogsParam> {
    ParentHandler::<LogsParam>::new()
        .root_handler(
            from_fn_async(cli_logs)
                .no_display()
                .with_inherited(|params, _| params),
        )
        .root_handler(
            from_fn_async(logs_nofollow)
                .with_inherited(|params, _| params)
                .no_cli(),
        )
        .subcommand(
            "follow",
            from_fn_async(logs_follow)
                .with_inherited(|params, _| params)
                .no_cli(),
        )
}
pub async fn cli_logs(
    ctx: CliContext,
    _: Empty,
    LogsParam {
        id,
        limit,
        cursor,
        before,
        follow,
    }: LogsParam,
) -> Result<(), RpcError> {
    if follow {
        if cursor.is_some() {
            return Err(RpcError::from(Error::new(
                eyre!("The argument '--cursor <cursor>' cannot be used with '--follow'"),
                crate::ErrorKind::InvalidRequest,
            )));
        }
        if before {
            return Err(RpcError::from(Error::new(
                eyre!("The argument '--before' cannot be used with '--follow'"),
                crate::ErrorKind::InvalidRequest,
            )));
        }
        cli_logs_generic_follow(ctx, "package.logs.follow", Some(id), limit).await
    } else {
        cli_logs_generic_nofollow(ctx, "package.logs", Some(id), limit, cursor, before).await
    }
}
pub async fn logs_nofollow(
    ctx: RpcContext,
    _: Empty,
    LogsParam {
        id,
        limit,
        cursor,
        before,
        ..
    }: LogsParam,
) -> Result<LogResponse, Error> {
    let container_id = ctx
        .services
        .get(&id)
        .await
        .as_ref()
        .map(|x| x.container_id())
        .ok_or_else(|| {
            Error::new(
                eyre!("No service found with id: {}", id),
                ErrorKind::NotFound,
            )
        })??;
    fetch_logs(LogSource::Container(container_id), limit, cursor, before).await
}
pub async fn logs_follow(
    ctx: RpcContext,
    _: Empty,
    LogsParam { id, limit, .. }: LogsParam,
) -> Result<LogFollowResponse, Error> {
    let container_id = ctx
        .services
        .get(&id)
        .await
        .as_ref()
        .map(|x| x.container_id())
        .ok_or_else(|| {
            Error::new(
                eyre!("No service found with id: {}", id),
                ErrorKind::NotFound,
            )
        })??;
    follow_logs(ctx, LogSource::Container(container_id), limit).await
}

pub async fn cli_logs_generic_nofollow(
    ctx: CliContext,
    method: &str,
    id: Option<PackageId>,
    limit: Option<usize>,
    cursor: Option<String>,
    before: bool,
) -> Result<(), RpcError> {
    let res = from_value::<LogResponse>(
        ctx.call_remote(
            method,
            imbl_value::json!({
                "id": id,
                "limit": limit,
                "cursor": cursor,
                "before": before,
            }),
        )
        .await?,
    )?;

    for entry in res.entries.iter() {
        println!("{}", entry);
    }

    Ok(())
}

pub async fn cli_logs_generic_follow(
    ctx: CliContext,
    method: &str,
    id: Option<PackageId>,
    limit: Option<usize>,
) -> Result<(), RpcError> {
    let res = from_value::<LogFollowResponse>(
        ctx.call_remote(
            method,
            imbl_value::json!({
                "id": id,
                "limit": limit,
            }),
        )
        .await?,
    )?;

    let mut stream = ctx.ws_continuation(res.guid).await?;
    while let Some(log) = stream.try_next().await? {
        if let Message::Text(log) = log {
            println!("{}", serde_json::from_str::<LogEntry>(&log)?);
        }
    }

    Ok(())
}

pub async fn journalctl(
    id: LogSource,
    limit: usize,
    cursor: Option<&str>,
    before: bool,
    follow: bool,
) -> Result<LogStream, Error> {
    let mut cmd = gen_journalctl_command(&id);

    cmd.arg(format!("--lines={}", limit));

    let cursor_formatted = format!("--after-cursor={}", cursor.unwrap_or(""));
    if cursor.is_some() {
        cmd.arg(&cursor_formatted);
        if before {
            cmd.arg("--reverse");
        }
    }

    let deserialized_entries = String::from_utf8(cmd.invoke(ErrorKind::Journald).await?)?
        .lines()
        .map(serde_json::from_str::<JournalctlEntry>)
        .collect::<Result<Vec<_>, _>>()
        .with_kind(ErrorKind::Deserialization)?;

    if follow {
        let mut follow_cmd = gen_journalctl_command(&id);
        follow_cmd.arg("-f");
        if let Some(last) = deserialized_entries.last() {
            follow_cmd.arg(format!("--after-cursor={}", last.cursor));
            follow_cmd.arg("--lines=all");
        } else {
            follow_cmd.arg("--lines=0");
        }
        let mut child = follow_cmd.stdout(Stdio::piped()).spawn()?;
        let out =
            BufReader::new(child.stdout.take().ok_or_else(|| {
                Error::new(eyre!("No stdout available"), crate::ErrorKind::Journald)
            })?);

        let journalctl_entries = LinesStream::new(out.lines());

        let follow_deserialized_entries = journalctl_entries
            .map_err(|e| Error::new(e, crate::ErrorKind::Journald))
            .and_then(|s| {
                futures::future::ready(
                    serde_json::from_str::<JournalctlEntry>(&s)
                        .with_kind(crate::ErrorKind::Deserialization),
                )
            });

        let entries = futures::stream::iter(deserialized_entries)
            .map(Ok)
            .chain(follow_deserialized_entries)
            .boxed();
        Ok(LogStream {
            _child: Some(child),
            entries,
        })
    } else {
        let entries = futures::stream::iter(deserialized_entries).map(Ok).boxed();

        Ok(LogStream {
            _child: None,
            entries,
        })
    }
}

fn gen_journalctl_command(id: &LogSource) -> Command {
    let mut cmd = match id {
        LogSource::Container(container_id) => {
            let mut cmd = Command::new("lxc-attach");
            cmd.arg(format!("{}", container_id))
                .arg("--")
                .arg("journalctl");
            cmd
        }
        _ => Command::new("journalctl"),
    };
    cmd.kill_on_drop(true);

    cmd.arg("--output=json");
    cmd.arg("--output-fields=MESSAGE");
    match id {
        LogSource::Kernel => {
            cmd.arg("-k");
        }
        LogSource::Unit(id) => {
            cmd.arg("-u");
            cmd.arg(id);
        }
        LogSource::System => {
            cmd.arg("-u");
            cmd.arg(SYSTEM_UNIT);
        }
        LogSource::Container(_container_id) => {
            cmd.arg("-u").arg("container-runtime.service");
        }
    };
    cmd
}

#[instrument(skip_all)]
pub async fn fetch_logs(
    id: LogSource,
    limit: Option<usize>,
    cursor: Option<String>,
    before: bool,
) -> Result<LogResponse, Error> {
    let limit = limit.unwrap_or(50);
    let mut stream = journalctl(id, limit, cursor.as_deref(), before, false).await?;

    let mut entries = Vec::with_capacity(limit);
    let mut start_cursor = None;

    if let Some(first) = tokio::time::timeout(Duration::from_secs(1), stream.try_next())
        .await
        .ok()
        .transpose()?
        .flatten()
    {
        let (cursor, entry) = first.log_entry()?;
        start_cursor = Some(cursor);
        entries.push(entry);
    }

    let (mut end_cursor, entries) = stream
        .try_fold(
            (start_cursor.clone(), entries),
            |(_, mut acc), entry| async move {
                let (cursor, entry) = entry.log_entry()?;
                acc.push(entry);
                Ok((Some(cursor), acc))
            },
        )
        .await?;
    let mut entries = Reversible::new(entries);
    // reverse again so output is always in increasing chronological order
    if cursor.is_some() && before {
        entries.reverse();
        std::mem::swap(&mut start_cursor, &mut end_cursor);
    }
    Ok(LogResponse {
        entries,
        start_cursor,
        end_cursor,
    })
}

#[instrument(skip_all)]
pub async fn follow_logs(
    ctx: RpcContext,
    id: LogSource,
    limit: Option<usize>,
) -> Result<LogFollowResponse, Error> {
    let limit = limit.unwrap_or(50);
    let mut stream = journalctl(id, limit, None, false, true).await?;

    let mut start_cursor = None;
    let mut first_entry = None;

    if let Some(first) = tokio::time::timeout(Duration::from_secs(1), stream.try_next())
        .await
        .ok()
        .transpose()?
        .flatten()
    {
        let (cursor, entry) = first.log_entry()?;
        start_cursor = Some(cursor);
        first_entry = Some(entry);
    }

    let guid = RequestGuid::new();
    ctx.add_continuation(
        guid.clone(),
        RpcContinuation::ws(
            Box::new(move |socket| {
                ws_handler(first_entry, stream, socket)
                    .map(|x| match x {
                        Ok(_) => (),
                        Err(e) => {
                            tracing::error!("Error in log stream: {}", e);
                        }
                    })
                    .boxed()
            }),
            Duration::from_secs(30),
        ),
    )
    .await;
    Ok(LogFollowResponse { start_cursor, guid })
}

// #[tokio::test]
// pub async fn test_logs() {
//     let response = fetch_logs(
//         // change `tor.service` to an actual journald unit on your machine
//         // LogSource::Service("tor.service"),
//         // first run `docker run --name=hello-world.embassy --log-driver=journald hello-world`
//         LogSource::Container("hello-world".parse().unwrap()),
//         // Some(5),
//         None,
//         None,
//         // Some("s=1b8c418e28534400856c27b211dd94fd;i=5a7;b=97571c13a1284f87bc0639b5cff5acbe;m=740e916;t=5ca073eea3445;x=f45bc233ca328348".to_owned()),
//         false,
//         true,
//     )
//     .await
//     .unwrap();
//     let serialized = serde_json::to_string_pretty(&response).unwrap();
//     println!("{}", serialized);
// }

// #[tokio::test]
// pub async fn test_logs() {
//     let mut cmd = Command::new("journalctl");
//     cmd.kill_on_drop(true);

//     cmd.arg("-f");
//     cmd.arg("CONTAINER_NAME=hello-world.embassy");

//     let mut child = cmd.stdout(Stdio::piped()).spawn().unwrap();
//     let out = BufReader::new(
//         child
//             .stdout
//             .take()
//             .ok_or_else(|| Error::new(eyre!("No stdout available"), crate::ErrorKind::Journald))
//             .unwrap(),
//     );

//     let mut journalctl_entries = LinesStream::new(out.lines());

//     while let Some(line) = journalctl_entries.try_next().await.unwrap() {
//         dbg!(line);
//     }
// }
