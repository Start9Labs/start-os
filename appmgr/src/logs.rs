use std::process::Stdio;
use std::time::{Duration, UNIX_EPOCH};

use chrono::{DateTime, Utc};
use clap::ArgMatches;
use color_eyre::eyre::eyre;
use futures::TryStreamExt;
use rpc_toolkit::command;
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::Command;
use tokio_stream::wrappers::LinesStream;
use tracing::instrument;

use crate::action::docker::DockerAction;
use crate::error::ResultExt;
use crate::s9pk::manifest::PackageId;
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
    Service(&'static str),
    Container(PackageId),
}

pub fn display_logs(all: LogResponse, _: &ArgMatches<'_>) {
    for entry in all.entries.iter() {
        println!("{}", entry);
    }
}

#[command(display(display_logs))]
pub async fn logs(
    #[arg] id: PackageId,
    #[arg] limit: Option<usize>,
    #[arg] cursor: Option<String>,
    #[arg] before_flag: Option<bool>,
) -> Result<LogResponse, Error> {
    Ok(fetch_logs(
        LogSource::Container(id),
        limit,
        cursor,
        before_flag.unwrap_or(false),
    )
    .await?)
}

#[instrument]
pub async fn fetch_logs(
    id: LogSource,
    limit: Option<usize>,
    cursor: Option<String>,
    before_flag: bool,
) -> Result<LogResponse, Error> {
    let limit = limit.unwrap_or(50);
    let limit_formatted = format!("-n{}", limit);

    let mut args = vec!["--output=json", "--output-fields=MESSAGE", &limit_formatted];
    let id_formatted = match id {
        LogSource::Service(id) => {
            args.push("-u");
            id.to_owned()
        }
        LogSource::Container(id) => {
            format!("CONTAINER_NAME={}", DockerAction::container_name(&id, None))
        }
    };
    args.push(&id_formatted);

    let cursor_formatted = format!("--after-cursor={}", cursor.clone().unwrap_or("".to_owned()));
    let mut get_prev_logs_and_reverse = false;
    if cursor.is_some() {
        args.push(&cursor_formatted);
        if before_flag {
            get_prev_logs_and_reverse = true;
        }
    }
    if get_prev_logs_and_reverse {
        args.push("--reverse");
    }

    let mut child = Command::new("journalctl")
        .args(args)
        .stdout(Stdio::piped())
        .spawn()?;
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
    let response = fetch_logs(
        // change `tor.service` to an actual journald unit on your machine
        // LogSource::Service("tor.service"),
        // first run `docker run --name=hello-world.embassy --log-driver=journald hello-world`
        LogSource::Container("hello-world".parse().unwrap()),
        // Some(5),
        None,
        None,
        // Some("s=1b8c418e28534400856c27b211dd94fd;i=5a7;b=97571c13a1284f87bc0639b5cff5acbe;m=740e916;t=5ca073eea3445;x=f45bc233ca328348".to_owned()),
        false,
    )
    .await
    .unwrap();
    let serialized = serde_json::to_string_pretty(&response).unwrap();
    println!("{}", serialized);
}
