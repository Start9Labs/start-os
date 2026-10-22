use std::process::Stdio;
use std::time::Duration;

use axum::extract::ws::Message;
use imbl_value::InternedString;
use serde::{Deserialize, Serialize};
use startos::util::net::WebSocket;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::Command;

use crate::continuations::{Guid, RpcContinuation};
use crate::invoke::Invoke;
use crate::prelude::*;
use crate::ServerContext;

#[derive(Debug, Serialize, Deserialize, visit_rs::VisitFields)]
#[serde(rename_all = "camelCase")]
pub struct LogEntry {
    pub timestamp: String,
    pub message: String,
}
rpc_toolkit::reflect_ts!(LogEntry);

#[derive(Debug, Serialize, Deserialize, visit_rs::VisitFields)]
#[serde(rename_all = "camelCase")]
pub struct LogsResponse {
    pub entries: Vec<LogEntry>,
}
rpc_toolkit::reflect_ts!(LogsResponse);

/// Parse a logread line into a LogEntry.
///
/// logread format: "Dow Mon DD HH:MM:SS YYYY facility.pri source: msg"
/// The timestamp is always the first 24 characters (fixed-width syslog format).
pub fn parse_logread_line(line: &str) -> Option<LogEntry> {
    let bytes = line.as_bytes();
    if bytes.len() < 25 || bytes[24] != b' ' {
        return None;
    }
    if !bytes[20..24].iter().all(|b| b.is_ascii_digit()) {
        return None;
    }
    Some(LogEntry {
        timestamp: line[..24].to_string(),
        message: line[25..].to_string(),
    })
}

#[derive(Deserialize, Serialize, visit_rs::VisitFields)]
pub struct FollowLogsParams {
    #[visit(ts(skip))]
    #[serde(rename = "__Auth_signer")]
    signer: Option<InternedString>,
}
rpc_toolkit::reflect_ts!(FollowLogsParams);

#[derive(Serialize, Deserialize, visit_rs::VisitFields)]
pub struct FollowLogsRes {
    /// Streams each new [`LogEntry`] as JSON from `/ws/rpc/{guid}`.
    pub guid: Guid,
}
rpc_toolkit::reflect_ts!(FollowLogsRes);

pub async fn follow_logs(
    ctx: ServerContext,
    FollowLogsParams { signer }: FollowLogsParams,
) -> Result<FollowLogsRes, Error> {
    let guid = Guid::new();
    ctx.continuations
        .add(
            guid.clone(),
            RpcContinuation::ws_authed(ctx.clone(), signer, stream_logs, Duration::from_secs(30)),
        )
        .await;
    Ok(FollowLogsRes { guid })
}

async fn stream_logs(mut ws: WebSocket) {
    let mut child = match Command::new("logread")
        .arg("-f")
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .kill_on_drop(true)
        .spawn()
    {
        Ok(child) => child,
        Err(e) => {
            tracing::error!("failed to spawn logread: {e}");
            return;
        }
    };

    let stdout = child.stdout.take().unwrap();
    let mut reader = BufReader::new(stdout).lines();

    loop {
        tokio::select! {
            line = reader.next_line() => {
                match line {
                    Ok(Some(line)) => {
                        let Some(entry) = parse_logread_line(&line) else {
                            continue;
                        };
                        let Ok(json) = serde_json::to_string(&entry) else {
                            continue;
                        };
                        if ws.send(Message::Text(json.into())).await.is_err() {
                            break;
                        }
                    }
                    Ok(None) => break,
                    Err(_) => break,
                }
            }
            msg = ws.recv() => {
                match msg {
                    Some(Ok(_)) => continue,
                    _ => break,
                }
            }
        }
    }
}

pub async fn get_logs(_ctx: ServerContext) -> Result<LogsResponse, Error> {
    let output = Command::new("logread")
        .invoke(ErrorKind::Filesystem.into())
        .await?;

    let stdout = String::from_utf8_lossy(&output);
    let entries = stdout.lines().filter_map(parse_logread_line).collect();

    Ok(LogsResponse { entries })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_typical_line() {
        let line =
            "Thu Mar  3 12:34:56 2026 daemon.info dnsmasq[1234]: query[A] example.com from 192.168.1.100";
        let entry = parse_logread_line(line).unwrap();
        assert_eq!(entry.timestamp, "Thu Mar  3 12:34:56 2026");
        assert_eq!(
            entry.message,
            "daemon.info dnsmasq[1234]: query[A] example.com from 192.168.1.100"
        );
    }

    #[test]
    fn test_parse_two_digit_day() {
        let line = "Mon Mar 13 08:00:00 2026 kern.info kernel: [12345.678] something happened";
        let entry = parse_logread_line(line).unwrap();
        assert_eq!(entry.timestamp, "Mon Mar 13 08:00:00 2026");
        assert_eq!(
            entry.message,
            "kern.info kernel: [12345.678] something happened"
        );
    }

    #[test]
    fn test_parse_empty_line() {
        assert!(parse_logread_line("").is_none());
    }

    #[test]
    fn test_parse_short_line() {
        assert!(parse_logread_line("too short").is_none());
    }

    #[test]
    fn test_parse_no_year() {
        assert!(parse_logread_line("Thu Mar  3 12:34:56 abcd rest of message").is_none());
    }
}
