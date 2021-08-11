use chrono::{DateTime, Utc};
use clap::ArgMatches;
use rpc_toolkit::command;
use tokio::process::Command;
use tokio::sync::RwLock;

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

#[derive(serde::Serialize, Clone, Debug)]
pub struct Celsius(f64);
#[derive(serde::Serialize, Clone, Debug)]
pub struct Percentage(f64);
#[derive(serde::Serialize, Clone, Debug)]
pub struct MebiBytes(f64);
#[derive(serde::Serialize, Clone, Debug)]
pub struct GigaBytes(f64);

#[derive(serde::Serialize, Clone, Debug)]
pub struct MetricsGeneral {
    temperature: Celsius,
}
#[derive(serde::Serialize, Clone, Debug)]
pub struct MetricsMemory {
    percentage_used: Percentage,
    total: MebiBytes,
    free: MebiBytes,
    used: MebiBytes,
    swap_total: MebiBytes,
    swap_free: MebiBytes,
    swap_used: MebiBytes,
}
#[derive(serde::Serialize, Clone, Debug)]
pub struct MetricsCpu {
    idle: Percentage,
    user_space: Percentage,
    kernel_space: Percentage,
    wait: Percentage,
    percentage_used: Percentage,
}
#[derive(serde::Serialize, Clone, Debug)]
pub struct MetricsDisk {
    size: GigaBytes,
    used: GigaBytes,
    available: GigaBytes,
    used_percentage: Percentage,
}
#[derive(serde::Serialize, Clone, Debug)]
pub struct Metrics {
    general: MetricsGeneral,
    memory: MetricsMemory,
    cpu: MetricsCpu,
    disk: MetricsDisk,
}

#[command(rpc_only)]
pub async fn metrics(#[context] ctx: RpcContext) -> Result<Metrics, Error> {
    let metrics_val = ctx.metrics_cache.read().await;
    match (*metrics_val).clone() {
        None => Err(Error {
            source: anyhow::anyhow!("No Metrics Found"),
            kind: ErrorKind::NotFound,
            revision: None,
        }),
        Some(metrics_val) => Ok(metrics_val),
    }
}

pub async fn launch_metrics_task(cache: &RwLock<Option<Metrics>>) {
    // fetch init temp
    // fetch init cpu
    // fetch init memory
    // fetch init disk usage
    {
        // lock for writing
        // write
    }
    // launch persistent temp task
    let temp_task = launch_temp_task(cache);
    // launch persistent cpu task
    let cpu_task = launch_cpu_task(cache);
    // launch persistent mem task
    let mem_task = launch_mem_task(cache);
    // launch persistent disk task
    let disk_task = launch_disk_task(cache);
    temp_task.await;
    cpu_task.await;
    mem_task.await;
    disk_task.await;
}

async fn launch_temp_task(cache: &RwLock<Option<Metrics>>) {
    loop {
        // read /sys/class/thermal/thermal_zone0/temp
        let val = 5f64;
        {
            let mut lock = cache.write().await;
            (*lock).as_mut().unwrap().general.temperature = Celsius(val)
        }
        tokio::time::sleep(tokio::time::Duration::from_secs(4)).await;
    }
}
async fn launch_cpu_task(cache: &RwLock<Option<Metrics>>) {
    loop {
        // read /proc/stat, diff against previous metrics, compute cpu load
        let val = MetricsCpu {
            idle: Percentage(0.0),
            user_space: Percentage(0.0),
            kernel_space: Percentage(0.0),
            wait: Percentage(0.0),
            percentage_used: Percentage(0.0),
        };
        {
            let mut lock = cache.write().await;
            (*lock).as_mut().unwrap().cpu = val;
        }
        tokio::time::sleep(tokio::time::Duration::from_secs(4)).await;
    }
}
async fn launch_mem_task(cache: &RwLock<Option<Metrics>>) {
    loop {
        // read /proc/meminfo
        let val = MetricsMemory {
            percentage_used: Percentage(0.0),
            total: MebiBytes(0.0),
            free: MebiBytes(0.0),
            used: MebiBytes(0.0),
            swap_total: MebiBytes(0.0),
            swap_free: MebiBytes(0.0),
            swap_used: MebiBytes(0.0),
        };
        {
            let mut lock = cache.write().await;
            (*lock).as_mut().unwrap().memory = val;
        }
        tokio::time::sleep(tokio::time::Duration::from_secs(4)).await;
    }
}
async fn launch_disk_task(cache: &RwLock<Option<Metrics>>) {
    loop {
        // run df and capture output
        let val = MetricsDisk {
            size: GigaBytes(0.0),
            used: GigaBytes(0.0),
            available: GigaBytes(0.0),
            used_percentage: Percentage(0.0),
        };
        {
            let mut lock = cache.write().await;
            (*lock).as_mut().unwrap().disk = val;
        }
        tokio::time::sleep(tokio::time::Duration::from_secs(60)).await;
    }
}

#[test]
pub fn test_output() {
    println!(
        "{} {} UTC",
        Utc::now().date().naive_utc(),
        Utc::now().time().format("%H:%M:%S")
    )
}
