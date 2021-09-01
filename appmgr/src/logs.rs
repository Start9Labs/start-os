use std::process::Stdio;
use std::time::{Duration, UNIX_EPOCH};

use anyhow::anyhow;
use chrono::{DateTime, Utc};
use clap::ArgMatches;
use futures::TryStreamExt;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::Command;
use tokio_stream::wrappers::LinesStream;

use crate::context::{EitherContext};
use crate::error::ResultExt;
use crate::util::Reversible;
use crate::Error;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
#[serde(rename_all = "kebab-case")]
pub struct LogResponse {
    entries: Reversible<LogEntry>,
    start_cursor: Option<String>,
    end_cursor: Option<String>,
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

fn display_logs(all: LogResponse, _: &ArgMatches<'_>) {
    for entry in all.entries.iter() {
        println!("{}", entry);
    }
}

#[command(display(display_logs))]
pub async fn logs(
    #[context] _: EitherContext,
    #[arg] id: String,
    #[arg] limit: Option<usize>,
    #[arg] cursor: Option<String>,
    #[arg] before_flag: Option<bool>,
) -> Result<LogResponse, Error> {
    Ok(logs_inner(id, limit, cursor, before_flag.unwrap_or(false)).await?)
}

#[derive(Serialize, Deserialize, Debug)]
struct JournalctlEntry {
    #[serde(rename = "__REALTIME_TIMESTAMP")]
    timestamp: String,
    #[serde(rename = "MESSAGE")]
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

async fn logs_inner(
    id: String,
    limit: Option<usize>,
    cursor: Option<String>,
    before_flag: bool,
) -> Result<LogResponse, Error> {
    let limit = limit.unwrap_or(50);
    let limit_formatted = format!("-n{}", limit);
    let mut get_prev_logs_and_reverse = false;

    let mut args_out = vec![
        "-u",
        &id,
        "--output=json",
        "--output-fields=MESSAGE",
        &limit_formatted,
    ];

    let cursor_formatted = format!("--after-cursor={}", cursor.clone().unwrap_or("".to_owned()));
    if cursor.is_some() {
        args_out.push(&cursor_formatted);
        if before_flag {
            get_prev_logs_and_reverse = true;
        }
    }
    if get_prev_logs_and_reverse {
        args_out.push("--reverse");
    }

    let mut child = Command::new("journalctl")
        .args(args_out)
        .stdout(Stdio::piped())
        .spawn()?;
    let out =
        BufReader::new(child.stdout.take().ok_or_else(|| {
            Error::new(anyhow!("No stdout available"), crate::ErrorKind::Journald)
        })?);

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

    if let Some(first) = deserialized_entries.try_next().await? {
        let (cursor, entry) = first.log_entry()?;
        start_cursor = Some(cursor);
        entries.push(entry);
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
    Ok(LogResponse {
        entries,
        start_cursor,
        end_cursor,
    })
}

#[tokio::test]
pub async fn test_logs() {
    let response =
        logs_inner(
            // find a journalctl unit on your machine
            "tor.service".to_owned(),
            // Some(5),
            None,
            None,
            // Some("s=1b8c418e28534400856c27b211dd94fd;i=5a7;b=97571c13a1284f87bc0639b5cff5acbe;m=740e916;t=5ca073eea3445;x=f45bc233ca328348".to_owned()),
            false,
        ).await.unwrap();
    let serialized = serde_json::to_string_pretty(&response).unwrap();
    println!("{}", serialized);
}
