use std::future::Future;
use std::marker::PhantomData;
use std::process::Stdio;
use std::time::{Duration, UNIX_EPOCH};

use chrono::{DateTime, Utc};
use color_eyre::eyre::eyre;
use futures::{FutureExt, SinkExt, StreamExt, TryStreamExt};
use hyper::upgrade::Upgraded;
use hyper::Error as HyperError;
use rpc_toolkit::command;
use rpc_toolkit::yajrc::RpcError;
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::{Child, Command};
use tokio::task::JoinError;
use tokio_stream::wrappers::LinesStream;
use tokio_tungstenite::tungstenite::protocol::frame::coding::CloseCode;
use tokio_tungstenite::tungstenite::protocol::CloseFrame;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::WebSocketStream;
use tracing::instrument;

use crate::context::{CliContext, RpcContext};
use crate::core::rpc_continuations::{RequestGuid, RpcContinuation};
use crate::error::ResultExt;
use crate::procedure::docker::DockerProcedure;
use crate::s9pk::manifest::PackageId;
use crate::util::{display_none, serde::Reversible};
use crate::{Error, ErrorKind};

pub struct LogStream {
    _child: Child,
    first_entry: Option<LogEntry>,
    entries: futures::stream::BoxStream<'static, Result<JournalctlEntry, Error>>,
}

#[instrument(skip(logs, ws_fut))]
async fn ws_handler<
    WSFut: Future<Output = Result<Result<WebSocketStream<Upgraded>, HyperError>, JoinError>>,
>(
    mut logs: LogStream,
    ws_fut: WSFut,
) -> Result<(), Error> {
    let mut stream = ws_fut
        .await
        .with_kind(crate::ErrorKind::Network)?
        .with_kind(crate::ErrorKind::Unknown)?;

    if let Some(first_entry) = logs.first_entry {
        stream
            .send(Message::Text(
                serde_json::to_string(&first_entry).with_kind(ErrorKind::Serialization)?,
            ))
            .await
            .with_kind(ErrorKind::Network)?;
    }

    while let Some(entry) = logs.entries.try_next().await? {
        let (_, log_entry) = entry.log_entry()?;
        stream
            .send(Message::Text(
                serde_json::to_string(&log_entry).with_kind(ErrorKind::Serialization)?,
            ))
            .await
            .with_kind(ErrorKind::Network)?;
    }

    stream
        .close(Some(CloseFrame {
            code: CloseCode::Normal,
            reason: "Log Stream Finished".into(),
        }))
        .await
        .with_kind(ErrorKind::Network)?;

    Ok(())
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
#[serde(rename_all = "kebab-case", tag = "type")]
pub enum LogResponse {
    NoFollow {
        entries: Reversible<LogEntry>,
        start_cursor: Option<String>,
        end_cursor: Option<String>,
    },
    Follow {
        start_cursor: Option<String>,
        guid: RequestGuid,
    },
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
pub struct LogEntry {
    timestamp: DateTime<Utc>,
    message: String,
}
impl std::fmt::Display for LogEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} {}", self.timestamp, self.message)
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct JournalctlEntry {
    #[serde(rename = "__REALTIME_TIMESTAMP")]
    timestamp: String,
    #[serde(rename = "MESSAGE")]
    #[serde(deserialize_with = "deserialize_string_or_utf8_array")]
    message: String,
    #[serde(rename = "__CURSOR")]
    cursor: String,
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
            },
        ))
    }
}

fn deserialize_string_or_utf8_array<'de, D: serde::de::Deserializer<'de>>(
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
            Ok(v.to_owned())
        }
        fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(v)
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
                    .filter_map(|a| a)
                    .collect::<Result<Vec<u8>, _>>()?,
            )
            .map_err(serde::de::Error::custom)
        }
    }
    deserializer.deserialize_any(Visitor)
}

#[derive(Debug)]
pub enum LogSource {
    Kernel,
    Service(&'static str),
    Container(PackageId),
}

#[command(
    custom_cli(cli_logs(async, context(CliContext))),
    display(display_none)
)]
pub async fn logs(
    #[context] ctx: RpcContext,
    #[arg] id: PackageId,
    #[arg(short = 'l', long = "limit")] limit: Option<usize>,
    #[arg(short = 'c', long = "cursor")] cursor: Option<String>,
    #[arg(short = 'B', long = "before")] before: bool,
    #[arg(short = 'f', long = "follow")] follow: bool,
) -> Result<LogResponse, Error> {
    Ok(fetch_logs(
        LogSource::Container(id),
        limit,
        cursor,
        before,
        if follow {
            Some(FollowArgs { ctx })
        } else {
            None
        },
    )
    .await?)
}
pub async fn cli_logs(
    ctx: CliContext,
    id: PackageId,
    limit: Option<usize>,
    cursor: Option<String>,
    before: bool,
    follow: bool,
) -> Result<(), RpcError> {
    cli_logs_generic(ctx, "package.logs", Some(id), limit, cursor, before, follow).await
}

pub struct FollowArgs {
    pub ctx: RpcContext,
}

pub async fn cli_logs_generic(
    ctx: CliContext,
    method: &str,
    id: Option<PackageId>,
    limit: Option<usize>,
    cursor: Option<String>,
    before: bool,
    follow: bool,
) -> Result<(), RpcError> {
    let res = rpc_toolkit::command_helpers::call_remote(
        ctx.clone(),
        method,
        serde_json::json!({
            "id": id,
            "limit": limit,
            "cursor": cursor,
            "before": before,
            "follow": follow,
        }),
        PhantomData::<LogResponse>,
    )
    .await?
    .result?;

    match res {
        LogResponse::NoFollow { entries, .. } => {
            for entry in entries.iter() {
                println!("{}", entry);
            }
        }
        LogResponse::Follow { guid, .. } => {
            let mut base_url = ctx.base_url.clone();
            let ws_scheme = match base_url.scheme() {
                "https" => "wss",
                "http" => "ws",
                _ => {
                    return Err(Error::new(
                        eyre!("Cannot parse scheme from base URL"),
                        crate::ErrorKind::ParseUrl,
                    )
                    .into())
                }
            };
            base_url.set_scheme(ws_scheme).or_else(|_| {
                Err(Error::new(
                    eyre!("Cannot set URL scheme"),
                    crate::ErrorKind::ParseUrl,
                ))
            })?;
            let (mut stream, _) =
                // base_url is "http://127.0.0.1/", with a trailing slash, so we don't put a leading slash in this path:
                tokio_tungstenite::connect_async(format!("{}ws/rpc/{}", base_url, guid)).await?;
            while let Some(log) = stream.try_next().await? {
                match log {
                    Message::Text(log) => {
                        println!("{}", serde_json::from_str::<LogEntry>(&log)?);
                    }
                    _ => (),
                }
            }
        }
    }

    Ok(())
}

#[instrument(skip(follow))]
pub async fn fetch_logs(
    id: LogSource,
    limit: Option<usize>,
    cursor: Option<String>,
    before: bool,
    follow: Option<FollowArgs>,
) -> Result<LogResponse, Error> {
    let mut cmd = Command::new("journalctl");
    cmd.kill_on_drop(true);

    let limit = limit.unwrap_or(50);

    cmd.arg("--output=json");
    cmd.arg("--output-fields=MESSAGE");
    cmd.arg(format!("-n{}", limit));
    match id {
        LogSource::Kernel => {
            cmd.arg("-k");
        }
        LogSource::Service(id) => {
            cmd.arg("-u");
            cmd.arg(id);
        }
        LogSource::Container(id) => {
            cmd.arg(format!(
                "CONTAINER_NAME={}",
                DockerProcedure::container_name(&id, None)
            ));
        }
    };

    let cursor_formatted = format!("--after-cursor={}", cursor.clone().unwrap_or("".to_owned()));
    let mut get_prev_logs_and_reverse = false;
    if cursor.is_some() {
        cmd.arg(&cursor_formatted);
        if before {
            get_prev_logs_and_reverse = true;
        }
    }
    if get_prev_logs_and_reverse {
        cmd.arg("--reverse");
    }
    if follow.is_some() {
        cmd.arg("--follow");
    }

    let mut child = cmd.stdout(Stdio::piped()).spawn()?;
    let out = BufReader::new(
        child
            .stdout
            .take()
            .ok_or_else(|| Error::new(eyre!("No stdout available"), crate::ErrorKind::Journald))?,
    );

    let journalctl_entries = LinesStream::new(out.lines());

    let mut deserialized_entries = journalctl_entries
        .map_err(|e| Error::new(e, crate::ErrorKind::Journald))
        .and_then(|s| {
            futures::future::ready(
                serde_json::from_str::<JournalctlEntry>(&s)
                    .with_kind(crate::ErrorKind::Deserialization),
            )
        });

    let mut entries = Vec::with_capacity(limit);
    let mut start_cursor = None;
    let mut first_entry = None;

    if let Some(first) =
        tokio::time::timeout(Duration::from_secs(1), deserialized_entries.try_next())
            .await
            .ok()
            .transpose()?
            .flatten()
    {
        let (cursor, entry) = first.log_entry()?;
        start_cursor = Some(cursor);
        if follow.is_some() {
            first_entry = Some(entry);
        } else {
            entries.push(entry);
        }
    }

    if let Some(follow) = follow {
        let guid = RequestGuid::new();
        follow
            .ctx
            .add_continuation(
                guid.clone(),
                RpcContinuation::ws(
                    Box::new(move |ws_fut| {
                        ws_handler(
                            LogStream {
                                _child: child,
                                first_entry,
                                entries: deserialized_entries.boxed(),
                            },
                            ws_fut,
                        )
                        .boxed()
                    }),
                    Duration::from_secs(30),
                ),
            )
            .await;
        return Ok(LogResponse::Follow { start_cursor, guid });
    }

    let (mut end_cursor, entries) = deserialized_entries
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
    if get_prev_logs_and_reverse {
        entries.reverse();
        std::mem::swap(&mut start_cursor, &mut end_cursor);
    }
    Ok(LogResponse::NoFollow {
        entries,
        start_cursor,
        end_cursor,
    })
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

#[tokio::test]
pub async fn test_logs() {
    let mut cmd = Command::new("journalctl");
    cmd.kill_on_drop(true);

    cmd.arg("-f");
    cmd.arg("CONTAINER_NAME=hello-world.embassy");

    let mut child = cmd.stdout(Stdio::piped()).spawn().unwrap();
    let out = BufReader::new(
        child
            .stdout
            .take()
            .ok_or_else(|| Error::new(eyre!("No stdout available"), crate::ErrorKind::Journald))
            .unwrap(),
    );

    let mut journalctl_entries = LinesStream::new(out.lines());

    while let Some(line) = journalctl_entries.try_next().await.unwrap() {
        dbg!(line);
    }
}
