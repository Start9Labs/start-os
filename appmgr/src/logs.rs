use std::time::{UNIX_EPOCH, Duration};

use serde::{Deserialize, Serialize};

use chrono::{DateTime, Utc, TimeZone};
use clap::ArgMatches;
use rpc_toolkit::command;
use tokio::process::Command;

use crate::context::{EitherContext, RpcContext};
use crate::util::display_serializable;
use crate::Error;
use crate::error::ResultExt;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
#[serde(rename_all = "kebab-case")]

pub struct LogResponse {
    entries: Vec<LogEntry>,
    start_cursor: String,
    end_cursor: String,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
pub struct CursorLogEntry {
    timestamp: String,
    message: String,
    cursor: String,
}
impl std::fmt::Display for CursorLogEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} {} {}", self.timestamp, self.message, self.cursor)
    }
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
pub struct LogEntry {
    timestamp: String,
    message: String,
}
impl std::fmt::Display for LogEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} {}", self.timestamp, self.message)
    }
}

// TODO
fn display_logs(all: LogResponse, matches: &ArgMatches<'_>) {
    use prettytable::*;

    if matches.is_present("format") {
        return display_serializable(all, matches);
    }

    let mut table = Table::new();
    table.add_row(row![bc =>
        "TIMESTAMP",
        "MESSAGE",
    ]);
    for entry in all.entries {
        let row = row![&format!("{}", entry.timestamp), &entry.message,];
        table.add_row(row);
    }
    table.print_tty(false);
}

#[command(rpc_only)]
pub async fn logs(
    #[context] _: EitherContext,
    #[arg] id: String,
    #[arg] limit: Option<usize>,
    #[arg] cursor: Option<String>,
    #[arg] reverse: Option<bool>,
) -> Result<LogResponse, Error> {
    Ok(logs_inner(id, limit, cursor, reverse).await?)
}

#[derive(Serialize, Deserialize, Debug)]
struct JournalctlEntry {
    #[serde(rename="__REALTIME_TIMESTAMP")]
    timestamp: String,
    #[serde(rename="MESSAGE")]
    message: String,
    #[serde(rename="__CURSOR")]
    cursor : String,
    #[serde(skip, rename="__MONOTONIC_TIMESTAMP")]
    monotonic_timestamp: String,
    #[serde(skip, rename="_BOOT_ID")]
    boot_id: String,
}

async fn logs_inner(
    id: String,
    limit: Option<usize>,
    cursor: Option<String>,
    reverse: Option<bool>,
) -> Result<LogResponse, Error> {
    println!("id = {:?}", id);

    let limit = limit.unwrap_or(50);
    let limit_formatted = format!("-n{}", limit);
    println!("limit = {:?}", limit_formatted);

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
        if reverse.is_some() {
            args_out.push("--reverse");
        }
    }

    println!("args_out = {:?}", args_out);

    let out = Command::new("journalctl")
        .args(args_out)
        .output()
        .await?
        .stdout;
    let out_string = String::from_utf8(out)?;
    let journalctl_entries = out_string.lines();
    println!("Found {} lines", journalctl_entries.clone().collect::<Vec<&str>>().len());
    let mut cursor_log_entries = journalctl_entries
        .map(|s| {
            let journalctl_entry: JournalctlEntry = serde_json::from_str(s)
                .with_kind(crate::ErrorKind::Unknown) // TODO find actual error
                .unwrap();
            CursorLogEntry { 
                timestamp: DateTime::<Utc>::from(
                    UNIX_EPOCH + Duration::from_micros(
                        journalctl_entry.timestamp.parse::<u64>().unwrap()
                    )
                ).format("%Y-%m-%d %H:%M:%S.%f").to_string(),
                message: journalctl_entry.message,
                cursor: journalctl_entry.cursor,
            }
        })
        .collect::<Vec<CursorLogEntry>>();
    // reverse output again if we reversed it above
    if cursor.is_some() && reverse.is_some() {
        cursor_log_entries.reverse();
    }
    let mut log_entries:Vec<LogEntry> = Vec::new(); // TODO do this in a less horrible way
    for cle in &cursor_log_entries {
        log_entries.push(LogEntry{timestamp: cle.clone().timestamp, message: cle.clone().message});
    }
    Ok(
        LogResponse {
            entries: log_entries,
            start_cursor: cursor_log_entries[0].clone().cursor,
            end_cursor: cursor_log_entries[cursor_log_entries.len()-1].clone().cursor,
        }
    )
}

#[tokio::test]
pub async fn test_logs() {
    println!(
        "{:#?}",
        logs_inner(
            // find a journalctl unit on your machine
            "tor.service".to_owned(), 
            Some(5), 
            None,
            // Some("s=1b8c418e28534400856c27b211dd94fd;i=e067d;b=97571c13a1284f87bc0639b5cff5acbe;m=3b6a68a823;t=5ca4e2be1576e;x=9a6f65a5917f8e9f".to_owned()), 
            // Some(true), 
            None,
        )
        .await
        .unwrap()
    )
}
