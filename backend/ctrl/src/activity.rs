use crate::utils::HandlerExtSerde;
use crate::{CliContext, Error, ServerContext};
use clap::Parser;
use rpc_toolkit::{from_fn_async, HandlerExt as _, ParentHandler};
use serde::{Deserialize, Serialize};
use std::fs;
use std::io;
use std::path::Path;
use tracing::instrument;

const ACTIVITY_DIR: &str = "/etc/startwrt";
const ACTIVITY_FILE: &str = "/etc/startwrt/activity.json";
const MAX_ENTRIES: usize = 1000;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActivityEntry {
    pub id: String,
    pub timestamp: String,
    pub category: String,
    pub action: String,
    pub success: bool,
    pub summary: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ActivityListResponse {
    pub entries: Vec<ActivityEntry>,
    pub total: usize,
}

#[derive(Debug, Deserialize, Serialize, Default, Parser)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct ActivityListParams {
    #[serde(default)]
    #[clap(long)]
    pub offset: Option<usize>,
    #[serde(default)]
    #[clap(long)]
    pub limit: Option<usize>,
}

#[derive(Debug, Deserialize, Serialize, Parser)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct ActivityDeleteParams {
    #[clap(long)]
    pub id: String,
}

fn read_log() -> Vec<ActivityEntry> {
    fs::read(ACTIVITY_FILE)
        .ok()
        .and_then(|b| serde_json::from_slice(&b).ok())
        .unwrap_or_default()
}

fn write_log(entries: &[ActivityEntry]) -> io::Result<()> {
    let dir = Path::new(ACTIVITY_DIR);
    if !dir.exists() {
        fs::create_dir_all(dir)?;
    }
    let tmp = format!("{ACTIVITY_FILE}.tmp");
    fs::write(&tmp, serde_json::to_vec(entries).map_err(|e| io::Error::new(io::ErrorKind::Other, e))?)?;
    fs::rename(&tmp, ACTIVITY_FILE)?;
    Ok(())
}

/// Log an activity entry. Best-effort — never propagates errors.
pub fn log(category: &str, action: &str, success: bool, summary: &str, error: Option<&str>) {
    let _ = log_inner(category, action, success, summary, error);
}

fn log_inner(
    category: &str,
    action: &str,
    success: bool,
    summary: &str,
    error: Option<&str>,
) -> io::Result<()> {
    let mut entries = read_log();

    entries.insert(
        0,
        ActivityEntry {
            id: format!("{:x}", rand::random::<u128>()),
            timestamp: chrono::Utc::now().to_rfc3339(),
            category: category.to_string(),
            action: action.to_string(),
            success,
            summary: summary.to_string(),
            error: error.map(String::from),
        },
    );

    entries.truncate(MAX_ENTRIES);
    write_log(&entries)
}

/// Convenience: log success or failure from a Result, return it unchanged.
pub fn log_result<T, E: std::fmt::Display>(
    category: &str,
    action: &str,
    subject: &str,
    result: Result<T, E>,
) -> Result<T, E> {
    match &result {
        Ok(_) => log(
            category,
            action,
            true,
            &capitalize_action_summary(action, subject),
            None,
        ),
        Err(e) => log(
            category,
            action,
            false,
            &format!("Failed to {} {}", action.replace('-', " "), subject),
            Some(&e.to_string()),
        ),
    }
    result
}

fn capitalize_action_summary(action: &str, subject: &str) -> String {
    let verb = match action {
        "created" => "Created",
        "updated" => "Updated",
        "deleted" => "Deleted",
        "configured" => "Configured",
        "enabled" => "Enabled",
        "disabled" => "Disabled",
        "blocked" => "Blocked",
        "unblocked" => "Unblocked",
        "forgotten" => "Forgot",
        "added" => "Added",
        "peer-added" => "Added",
        "peer-deleted" => "Deleted",
        "downloaded" => "Downloaded",
        "restored" => "Restored",
        "recovered" => "Recovered",
        _ => action,
    };
    format!("{verb} {subject}")
}

// ── RPC Handlers ────────────────────────────────────────────────

#[instrument(skip_all)]
async fn list(
    _ctx: ServerContext,
    params: ActivityListParams,
) -> Result<ActivityListResponse, Error> {
    let entries = read_log();
    let total = entries.len();
    let offset = params.offset.unwrap_or(0);
    let limit = params.limit.unwrap_or(50);
    let page = entries.into_iter().skip(offset).take(limit).collect();
    Ok(ActivityListResponse {
        entries: page,
        total,
    })
}

#[instrument(skip_all)]
async fn delete(
    _ctx: ServerContext,
    ActivityDeleteParams { id }: ActivityDeleteParams,
) -> Result<(), Error> {
    let mut entries = read_log();
    let before = entries.len();
    entries.retain(|e| e.id != id);
    if entries.len() == before {
        return Err(Error::other("Activity entry not found"));
    }
    write_log(&entries).map_err(|e| Error::other(format!("Failed to write activity log: {e}")))?;
    Ok(())
}

#[instrument(skip_all)]
async fn clear(_ctx: ServerContext) -> Result<(), Error> {
    write_log(&[]).map_err(|e| Error::other(format!("Failed to clear activity log: {e}")))?;
    Ok(())
}

pub fn activity<C: rpc_toolkit::Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "list",
            from_fn_async(list)
                .with_display_serializable()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "delete",
            from_fn_async(delete)
                .no_display()
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "clear",
            from_fn_async(clear)
                .no_display()
                .with_call_remote::<CliContext>(),
        )
}
