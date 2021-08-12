use chrono::{DateTime, Utc};
use clap::ArgMatches;
use rpc_toolkit::command;
use tokio::process::Command;

use crate::context::RpcContext;
use crate::{Error, ErrorKind, ResultExt};

pub const SYSTEMD_UNIT: &'static str = "embassyd";

fn parse_datetime(text: &str, _matches: &ArgMatches) -> Result<DateTime<Utc>, Error> {
    text.parse().with_kind(ErrorKind::ParseTimestamp)
}

#[command(rpc_only)]
pub async fn logs(
    #[context] _ctx: RpcContext,
    #[arg(parse(crate::system::parse_datetime))] before: Option<DateTime<Utc>>,
    #[arg] limit: Option<usize>,
) -> Result<Vec<(String, String)>, Error> {
    let before = before.unwrap_or(Utc::now());
    let limit = limit.unwrap_or(50);
    // Journalctl has unexpected behavior where "until" does not play well with "lines" unless the output is reversed.
    // Keep this in mind if you are changing the code below
    let out = Command::new("journalctl")
        .args(&[
            "-u",
            SYSTEMD_UNIT,
            &format!(
                "-U\"{} {} UTC\"",
                before.date().naive_utc(),
                before.time().format("%H:%M:%S")
            ),
            "--output=short-iso",
            "--no-hostname",
            "--utc",
            "--reverse",
            &format!("-n{}", limit),
        ])
        .output()
        .await?
        .stdout;
    let out_string = String::from_utf8(out)?;
    let lines = out_string.lines();
    let mut split_lines = lines
        .skip(1) // ditch the journalctl header
        .map(|s| {
            // split the timestamp off from the log line
            let (ts, l) = s.split_once(" ").unwrap();
            (ts.to_owned(), l.to_owned())
        })
        .collect::<Vec<(String, String)>>();
    // reverse output again because we reversed it above
    split_lines.reverse();
    Ok(split_lines)
}

#[test]
pub fn test_output() {
    println!(
        "{} {} UTC",
        Utc::now().date().naive_utc(),
        Utc::now().time().format("%H:%M:%S")
    )
}
