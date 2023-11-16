use std::collections::BTreeSet;
use std::fmt;

use chrono::Utc;
use clap::ArgMatches;
use color_eyre::eyre::eyre;
use futures::FutureExt;
use rpc_toolkit::command;
use rpc_toolkit::yajrc::RpcError;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use tokio::process::Command;
use tokio::sync::broadcast::Receiver;
use tokio::sync::RwLock;
use tracing::instrument;

use crate::context::{CliContext, RpcContext};
use crate::disk::util::{get_available, get_used};
use crate::logs::{
    cli_logs_generic_follow, cli_logs_generic_nofollow, fetch_logs, follow_logs, LogFollowResponse,
    LogResponse, LogSource,
};
use crate::prelude::*;
use crate::shutdown::Shutdown;
use crate::util::cpupower::{get_available_governors, set_governor, Governor};
use crate::util::serde::{display_serializable, IoFormat};
use crate::util::{display_none, Invoke};
use crate::{Error, ErrorKind, ResultExt};

#[command(subcommands(zram, governor))]
pub async fn experimental() -> Result<(), Error> {
    Ok(())
}

pub async fn enable_zram() -> Result<(), Error> {
    let mem_info = get_mem_info().await?;
    Command::new("modprobe")
        .arg("zram")
        .invoke(ErrorKind::Zram)
        .await?;
    tokio::fs::write("/sys/block/zram0/comp_algorithm", "lz4")
        .await
        .with_kind(ErrorKind::Zram)?;
    tokio::fs::write(
        "/sys/block/zram0/disksize",
        format!("{}M", mem_info.total.0 as u64 / 4),
    )
    .await
    .with_kind(ErrorKind::Zram)?;
    Command::new("mkswap")
        .arg("/dev/zram0")
        .invoke(ErrorKind::Zram)
        .await?;
    Command::new("swapon")
        .arg("-p")
        .arg("5")
        .arg("/dev/zram0")
        .invoke(ErrorKind::Zram)
        .await?;
    Ok(())
}

#[command(display(display_none))]
pub async fn zram(#[context] ctx: RpcContext, #[arg] enable: bool) -> Result<(), Error> {
    let db = ctx.db.peek().await;

    let zram = db.as_server_info().as_zram().de()?;
    if enable == zram {
        return Ok(());
    }
    if enable {
        enable_zram().await?;
    } else {
        Command::new("swapoff")
            .arg("/dev/zram0")
            .invoke(ErrorKind::Zram)
            .await?;
        tokio::fs::write("/sys/block/zram0/reset", "1")
            .await
            .with_kind(ErrorKind::Zram)?;
    }
    ctx.db
        .mutate(|v| {
            v.as_server_info_mut().as_zram_mut().ser(&enable)?;
            Ok(())
        })
        .await?;
    Ok(())
}

#[derive(Debug, Deserialize, Serialize)]
pub struct GovernorInfo {
    current: Option<Governor>,
    available: BTreeSet<Governor>,
}

fn display_governor_info(arg: GovernorInfo, matches: &ArgMatches) {
    use prettytable::*;

    if matches.is_present("format") {
        return display_serializable(arg, matches);
    }

    let mut table = Table::new();
    table.add_row(row![bc -> "GOVERNORS"]);
    for entry in arg.available {
        if Some(&entry) == arg.current.as_ref() {
            table.add_row(row![g -> format!("* {entry} (current)")]);
        } else {
            table.add_row(row![entry]);
        }
    }
    table.print_tty(false).unwrap();
}

#[command(display(display_governor_info))]
pub async fn governor(
    #[context] ctx: RpcContext,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
    #[arg] set: Option<Governor>,
) -> Result<GovernorInfo, Error> {
    let available = get_available_governors().await?;
    if let Some(set) = set {
        if !available.contains(&set) {
            return Err(Error::new(
                eyre!("Governor {set} not available"),
                ErrorKind::InvalidRequest,
            ));
        }
        set_governor(&set).await?;
        ctx.db
            .mutate(|d| d.as_server_info_mut().as_governor_mut().ser(&Some(set)))
            .await?;
    }
    let current = ctx.db.peek().await.as_server_info().as_governor().de()?;
    Ok(GovernorInfo { current, available })
}

#[derive(Serialize, Deserialize)]
pub struct TimeInfo {
    now: String,
    uptime: u64,
}

fn display_time(arg: TimeInfo, matches: &ArgMatches) {
    use std::fmt::Write;

    use prettytable::*;

    if matches.is_present("format") {
        return display_serializable(arg, matches);
    }

    let days = arg.uptime / (24 * 60 * 60);
    let days_s = arg.uptime % (24 * 60 * 60);
    let hours = days_s / (60 * 60);
    let hours_s = arg.uptime % (60 * 60);
    let minutes = hours_s / 60;
    let seconds = arg.uptime % 60;
    let mut uptime_string = String::new();
    if days > 0 {
        write!(&mut uptime_string, "{days} days").unwrap();
    }
    if hours > 0 {
        if !uptime_string.is_empty() {
            uptime_string += ", ";
        }
        write!(&mut uptime_string, "{hours} hours").unwrap();
    }
    if minutes > 0 {
        if !uptime_string.is_empty() {
            uptime_string += ", ";
        }
        write!(&mut uptime_string, "{minutes} minutes").unwrap();
    }
    if !uptime_string.is_empty() {
        uptime_string += ", ";
    }
    write!(&mut uptime_string, "{seconds} seconds").unwrap();

    let mut table = Table::new();
    table.add_row(row![bc -> "NOW", &arg.now]);
    table.add_row(row![bc -> "UPTIME", &uptime_string]);
    table.print_tty(false).unwrap();
}

#[command(display(display_time))]
pub async fn time(
    #[context] ctx: RpcContext,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<TimeInfo, Error> {
    Ok(TimeInfo {
        now: Utc::now().to_rfc3339(),
        uptime: ctx.start_time.elapsed().as_secs(),
    })
}

#[command(
    custom_cli(cli_logs(async, context(CliContext))),
    subcommands(self(logs_nofollow(async)), logs_follow),
    display(display_none)
)]
pub async fn logs(
    #[arg(short = 'l', long = "limit")] limit: Option<usize>,
    #[arg(short = 'c', long = "cursor")] cursor: Option<String>,
    #[arg(short = 'B', long = "before", default)] before: bool,
    #[arg(short = 'f', long = "follow", default)] follow: bool,
) -> Result<(Option<usize>, Option<String>, bool, bool), Error> {
    Ok((limit, cursor, before, follow))
}
pub async fn cli_logs(
    ctx: CliContext,
    (limit, cursor, before, follow): (Option<usize>, Option<String>, bool, bool),
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
        cli_logs_generic_follow(ctx, "server.logs.follow", None, limit).await
    } else {
        cli_logs_generic_nofollow(ctx, "server.logs", None, limit, cursor, before).await
    }
}
pub async fn logs_nofollow(
    _ctx: (),
    (limit, cursor, before, _): (Option<usize>, Option<String>, bool, bool),
) -> Result<LogResponse, Error> {
    fetch_logs(LogSource::System, limit, cursor, before).await
}

#[command(rpc_only, rename = "follow", display(display_none))]
pub async fn logs_follow(
    #[context] ctx: RpcContext,
    #[parent_data] (limit, _, _, _): (Option<usize>, Option<String>, bool, bool),
) -> Result<LogFollowResponse, Error> {
    follow_logs(ctx, LogSource::System, limit).await
}

#[command(
    rename = "kernel-logs",
    custom_cli(cli_kernel_logs(async, context(CliContext))),
    subcommands(self(kernel_logs_nofollow(async)), kernel_logs_follow),
    display(display_none)
)]
pub async fn kernel_logs(
    #[arg(short = 'l', long = "limit")] limit: Option<usize>,
    #[arg(short = 'c', long = "cursor")] cursor: Option<String>,
    #[arg(short = 'B', long = "before", default)] before: bool,
    #[arg(short = 'f', long = "follow", default)] follow: bool,
) -> Result<(Option<usize>, Option<String>, bool, bool), Error> {
    Ok((limit, cursor, before, follow))
}
pub async fn cli_kernel_logs(
    ctx: CliContext,
    (limit, cursor, before, follow): (Option<usize>, Option<String>, bool, bool),
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
        cli_logs_generic_follow(ctx, "server.kernel-logs.follow", None, limit).await
    } else {
        cli_logs_generic_nofollow(ctx, "server.kernel-logs", None, limit, cursor, before).await
    }
}
pub async fn kernel_logs_nofollow(
    _ctx: (),
    (limit, cursor, before, _): (Option<usize>, Option<String>, bool, bool),
) -> Result<LogResponse, Error> {
    fetch_logs(LogSource::Kernel, limit, cursor, before).await
}

#[command(rpc_only, rename = "follow", display(display_none))]
pub async fn kernel_logs_follow(
    #[context] ctx: RpcContext,
    #[parent_data] (limit, _, _, _): (Option<usize>, Option<String>, bool, bool),
) -> Result<LogFollowResponse, Error> {
    follow_logs(ctx, LogSource::Kernel, limit).await
}

#[derive(Serialize, Deserialize)]
pub struct MetricLeaf<T> {
    value: T,
    unit: Option<String>,
}

#[derive(Clone, Debug)]
pub struct Celsius(f64);
impl fmt::Display for Celsius {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:.1}°C", self.0)
    }
}
impl Serialize for Celsius {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        MetricLeaf {
            value: format!("{:.1}", self.0),
            unit: Some(String::from("°C")),
        }
        .serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for Celsius {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = MetricLeaf::<String>::deserialize(deserializer)?;
        Ok(Celsius(s.value.parse().map_err(serde::de::Error::custom)?))
    }
}
#[derive(Clone, Debug)]
pub struct Percentage(f64);
impl Serialize for Percentage {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        MetricLeaf {
            value: format!("{:.1}", self.0),
            unit: Some(String::from("%")),
        }
        .serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for Percentage {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = MetricLeaf::<String>::deserialize(deserializer)?;
        Ok(Percentage(
            s.value.parse().map_err(serde::de::Error::custom)?,
        ))
    }
}

#[derive(Clone, Debug)]
pub struct MebiBytes(pub f64);
impl Serialize for MebiBytes {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        MetricLeaf {
            value: format!("{:.2}", self.0),
            unit: Some(String::from("MiB")),
        }
        .serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for MebiBytes {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = MetricLeaf::<String>::deserialize(deserializer)?;
        Ok(MebiBytes(
            s.value.parse().map_err(serde::de::Error::custom)?,
        ))
    }
}

#[derive(Clone, Debug)]
pub struct GigaBytes(f64);
impl Serialize for GigaBytes {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        MetricLeaf {
            value: format!("{:.2}", self.0),
            unit: Some(String::from("GB")),
        }
        .serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for GigaBytes {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = MetricLeaf::<String>::deserialize(deserializer)?;
        Ok(GigaBytes(
            s.value.parse().map_err(serde::de::Error::custom)?,
        ))
    }
}

#[derive(Deserialize, Serialize, Clone, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct MetricsGeneral {
    pub temperature: Option<Celsius>,
}
#[derive(Deserialize, Serialize, Clone, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct MetricsMemory {
    pub percentage_used: Percentage,
    pub total: MebiBytes,
    pub available: MebiBytes,
    pub used: MebiBytes,
    pub zram_total: MebiBytes,
    pub zram_available: MebiBytes,
    pub zram_used: MebiBytes,
}
#[derive(Deserialize, Serialize, Clone, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct MetricsCpu {
    percentage_used: Percentage,
    idle: Percentage,
    user_space: Percentage,
    kernel_space: Percentage,
    wait: Percentage,
}
#[derive(Deserialize, Serialize, Clone, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct MetricsDisk {
    percentage_used: Percentage,
    used: GigaBytes,
    available: GigaBytes,
    capacity: GigaBytes,
}
#[derive(Deserialize, Serialize, Clone, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct Metrics {
    general: MetricsGeneral,
    memory: MetricsMemory,
    cpu: MetricsCpu,
    disk: MetricsDisk,
}

#[command(display(display_serializable))]
pub async fn metrics(
    #[context] ctx: RpcContext,
    #[allow(unused_variables)]
    #[arg(long = "format")]
    format: Option<IoFormat>,
) -> Result<Metrics, Error> {
    match ctx.metrics_cache.read().await.clone() {
        None => Err(Error {
            source: color_eyre::eyre::eyre!("No Metrics Found"),
            kind: ErrorKind::NotFound,
            revision: None,
        }),
        Some(metrics_val) => Ok(metrics_val),
    }
}

pub async fn launch_metrics_task<F: FnMut() -> Receiver<Option<Shutdown>>>(
    cache: &RwLock<Option<Metrics>>,
    mut mk_shutdown: F,
) {
    // fetch init temp
    let init_temp = match get_temp().await {
        Ok(a) => Some(a),
        Err(e) => {
            tracing::error!("Could not get initial temperature: {}", e);
            tracing::debug!("{:?}", e);
            None
        }
    };
    // fetch init cpu
    let init_cpu;
    let proc_stat;
    loop {
        match get_proc_stat().await {
            Ok(mut ps) => match get_cpu_info(&mut ps).await {
                Ok(mc) => {
                    proc_stat = ps;
                    init_cpu = mc;
                    break;
                }
                Err(e) => {
                    tracing::error!("Could not get initial cpu info: {}", e);
                    tracing::debug!("{:?}", e);
                }
            },
            Err(e) => {
                tracing::error!("Could not get initial proc stat: {}", e);
                tracing::debug!("{:?}", e);
            }
        }
        tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
    }
    // fetch init memory
    let init_mem;
    loop {
        match get_mem_info().await {
            Ok(a) => {
                init_mem = a;
                break;
            }
            Err(e) => {
                tracing::error!("Could not get initial mem info: {}", e);
                tracing::debug!("{:?}", e);
            }
        }
        tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
    }
    // fetch init disk usage
    let init_disk;
    loop {
        match get_disk_info().await {
            Ok(a) => {
                init_disk = a;
                break;
            }
            Err(e) => {
                tracing::error!("Could not get initial disk info: {}", e);
                tracing::debug!("{:?}", e);
            }
        }
        tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
    }
    {
        // lock for writing
        let mut guard = cache.write().await;
        // write
        *guard = Some(Metrics {
            general: MetricsGeneral {
                temperature: init_temp,
            },
            memory: init_mem,
            cpu: init_cpu,
            disk: init_disk,
        })
    }

    let mut task_vec = Vec::new();
    // launch persistent temp task
    if cache
        .read()
        .await
        .as_ref()
        .unwrap()
        .general
        .temperature
        .is_some()
    {
        task_vec.push(launch_temp_task(cache, mk_shutdown()).boxed());
    }
    // launch persistent cpu task
    task_vec.push(launch_cpu_task(cache, proc_stat, mk_shutdown()).boxed());
    // launch persistent mem task
    task_vec.push(launch_mem_task(cache, mk_shutdown()).boxed());
    // launch persistent disk task
    task_vec.push(launch_disk_task(cache, mk_shutdown()).boxed());

    futures::future::join_all(task_vec).await;
}

async fn launch_temp_task(
    cache: &RwLock<Option<Metrics>>,
    mut shutdown: Receiver<Option<Shutdown>>,
) {
    loop {
        match get_temp().await {
            Ok(a) => {
                let mut lock = cache.write().await;
                (*lock).as_mut().unwrap().general.temperature = Some(a)
            }
            Err(e) => {
                tracing::error!("Could not get new temperature: {}", e);
                tracing::debug!("{:?}", e);
            }
        }
        tokio::select! {
            _ = shutdown.recv() => return,
            _ = tokio::time::sleep(tokio::time::Duration::from_secs(4)) => (),
        }
    }
}

async fn launch_cpu_task(
    cache: &RwLock<Option<Metrics>>,
    mut init: ProcStat,
    mut shutdown: Receiver<Option<Shutdown>>,
) {
    loop {
        // read /proc/stat, diff against previous metrics, compute cpu load
        match get_cpu_info(&mut init).await {
            Ok(info) => {
                let mut lock = cache.write().await;
                (*lock).as_mut().unwrap().cpu = info;
            }
            Err(e) => {
                tracing::error!("Could not get new CPU Metrics: {}", e);
                tracing::debug!("{:?}", e);
            }
        }
        tokio::select! {
            _ = shutdown.recv() => return,
            _ = tokio::time::sleep(tokio::time::Duration::from_secs(4)) => (),
        }
    }
}

async fn launch_mem_task(
    cache: &RwLock<Option<Metrics>>,
    mut shutdown: Receiver<Option<Shutdown>>,
) {
    loop {
        // read /proc/meminfo
        match get_mem_info().await {
            Ok(a) => {
                let mut lock = cache.write().await;
                (*lock).as_mut().unwrap().memory = a;
            }
            Err(e) => {
                tracing::error!("Could not get new Memory Metrics: {}", e);
                tracing::debug!("{:?}", e);
            }
        }
        tokio::select! {
            _ = shutdown.recv() => return,
            _ = tokio::time::sleep(tokio::time::Duration::from_secs(4)) => (),
        }
    }
}
async fn launch_disk_task(
    cache: &RwLock<Option<Metrics>>,
    mut shutdown: Receiver<Option<Shutdown>>,
) {
    loop {
        // run df and capture output
        match get_disk_info().await {
            Ok(a) => {
                let mut lock = cache.write().await;
                (*lock).as_mut().unwrap().disk = a;
            }
            Err(e) => {
                tracing::error!("Could not get new Disk Metrics: {}", e);
                tracing::debug!("{:?}", e);
            }
        }
        tokio::select! {
            _ = shutdown.recv() => return,
            _ = tokio::time::sleep(tokio::time::Duration::from_secs(60)) => (),
        }
    }
}

#[instrument(skip_all)]
async fn get_temp() -> Result<Celsius, Error> {
    let temp = serde_json::from_slice::<serde_json::Value>(
        &Command::new("sensors")
            .arg("-j")
            .invoke(ErrorKind::Filesystem)
            .await?,
    )
    .with_kind(ErrorKind::Deserialization)?
    .as_object()
    .into_iter()
    .flatten()
    .flat_map(|(_, v)| v.as_object())
    .flatten()
    .flat_map(|(_, v)| v.as_object())
    .flatten()
    .filter_map(|(k, v)| {
        // we have seen so far that `temp1` is always a composite reading of some sort, so we should just use that for each chip
        if k.trim() == "temp1_input" {
            v.as_f64()
        } else {
            None
        }
    })
    .reduce(f64::max)
    .ok_or_else(|| Error::new(eyre!("No temperatures available"), ErrorKind::Filesystem))?;
    Ok(Celsius(temp))
}

#[derive(Debug, Clone)]
pub struct ProcStat {
    user: u64,
    nice: u64,
    system: u64,
    idle: u64,
    iowait: u64,
    irq: u64,
    softirq: u64,
    // below are only applicable to virtualized environments
    // steal: u64,
    // guest: u64,
    // guest_nice: u64,
}
impl ProcStat {
    fn total(&self) -> u64 {
        self.user + self.nice + self.system + self.idle + self.iowait + self.irq + self.softirq
    }
    fn user(&self) -> u64 {
        self.user + self.nice
    }
    fn system(&self) -> u64 {
        self.system + self.irq + self.softirq
    }
    fn used(&self) -> u64 {
        self.user() + self.system()
    }
}

#[instrument(skip_all)]
async fn get_proc_stat() -> Result<ProcStat, Error> {
    use tokio::io::AsyncBufReadExt;
    let mut cpu_line = String::new();
    let _n = tokio::io::BufReader::new(tokio::fs::File::open("/proc/stat").await?)
        .read_line(&mut cpu_line)
        .await?;
    let stats: Vec<u64> = cpu_line
        .split_whitespace()
        .skip(1)
        .map(|s| {
            s.parse::<u64>().map_err(|e| {
                Error::new(
                    color_eyre::eyre::eyre!("Invalid /proc/stat column value: {}", e),
                    ErrorKind::ParseSysInfo,
                )
            })
        })
        .collect::<Result<Vec<u64>, Error>>()?;

    if stats.len() < 10 {
        Err(Error {
            source: color_eyre::eyre::eyre!(
                "Columns missing from /proc/stat. Need 10, found {}",
                stats.len()
            ),
            kind: ErrorKind::ParseSysInfo,
            revision: None,
        })
    } else {
        Ok(ProcStat {
            user: stats[0],
            nice: stats[1],
            system: stats[2],
            idle: stats[3],
            iowait: stats[4],
            irq: stats[5],
            softirq: stats[6],
        })
    }
}

#[instrument(skip_all)]
async fn get_cpu_info(last: &mut ProcStat) -> Result<MetricsCpu, Error> {
    let new = get_proc_stat().await?;
    let total_old = last.total();
    let total_new = new.total();
    let total_diff = total_new - total_old;
    let res = MetricsCpu {
        user_space: Percentage((new.user() - last.user()) as f64 * 100.0 / total_diff as f64),
        kernel_space: Percentage((new.system() - last.system()) as f64 * 100.0 / total_diff as f64),
        idle: Percentage((new.idle - last.idle) as f64 * 100.0 / total_diff as f64),
        wait: Percentage((new.iowait - last.iowait) as f64 * 100.0 / total_diff as f64),
        percentage_used: Percentage((new.used() - last.used()) as f64 * 100.0 / total_diff as f64),
    };
    *last = new;
    Ok(res)
}

pub struct MemInfo {
    mem_total: Option<u64>,
    mem_free: Option<u64>,
    mem_available: Option<u64>,
    buffers: Option<u64>,
    cached: Option<u64>,
    slab: Option<u64>,
    zram_total: Option<u64>,
    zram_free: Option<u64>,
}
#[instrument(skip_all)]
pub async fn get_mem_info() -> Result<MetricsMemory, Error> {
    let contents = tokio::fs::read_to_string("/proc/meminfo").await?;
    let mut mem_info = MemInfo {
        mem_total: None,
        mem_free: None,
        mem_available: None,
        buffers: None,
        cached: None,
        slab: None,
        zram_total: None,
        zram_free: None,
    };
    fn get_num_kb(l: &str) -> Result<u64, Error> {
        let e = Error::new(
            color_eyre::eyre::eyre!("Invalid meminfo line: {}", l),
            ErrorKind::ParseSysInfo,
        );
        match l.split_whitespace().skip(1).next() {
            Some(x) => match x.parse() {
                Ok(y) => Ok(y),
                Err(_) => Err(e),
            },
            None => Err(e),
        }
    }
    for entry in contents.lines() {
        match entry {
            _ if entry.starts_with("MemTotal") => mem_info.mem_total = Some(get_num_kb(entry)?),
            _ if entry.starts_with("MemFree") => mem_info.mem_free = Some(get_num_kb(entry)?),
            _ if entry.starts_with("MemAvailable") => {
                mem_info.mem_available = Some(get_num_kb(entry)?)
            }
            _ if entry.starts_with("Buffers") => mem_info.buffers = Some(get_num_kb(entry)?),
            _ if entry.starts_with("Cached") => mem_info.cached = Some(get_num_kb(entry)?),
            _ if entry.starts_with("Slab") => mem_info.slab = Some(get_num_kb(entry)?),
            _ if entry.starts_with("SwapTotal") => mem_info.zram_total = Some(get_num_kb(entry)?),
            _ if entry.starts_with("SwapFree") => mem_info.zram_free = Some(get_num_kb(entry)?),
            _ => (),
        }
    }
    fn ensure_present(a: Option<u64>, field: &str) -> Result<u64, Error> {
        a.ok_or(Error::new(
            color_eyre::eyre::eyre!("{} missing from /proc/meminfo", field),
            ErrorKind::ParseSysInfo,
        ))
    }
    let mem_total = ensure_present(mem_info.mem_total, "MemTotal")?;
    let mem_free = ensure_present(mem_info.mem_free, "MemFree")?;
    let mem_available = ensure_present(mem_info.mem_available, "MemAvailable")?;
    let buffers = ensure_present(mem_info.buffers, "Buffers")?;
    let cached = ensure_present(mem_info.cached, "Cached")?;
    let slab = ensure_present(mem_info.slab, "Slab")?;
    let zram_total_k = ensure_present(mem_info.zram_total, "SwapTotal")?;
    let zram_free_k = ensure_present(mem_info.zram_free, "SwapFree")?;

    let total = MebiBytes(mem_total as f64 / 1024.0);
    let available = MebiBytes(mem_available as f64 / 1024.0);
    let used = MebiBytes((mem_total - mem_free - buffers - cached - slab) as f64 / 1024.0);
    let zram_total = MebiBytes(zram_total_k as f64 / 1024.0);
    let zram_available = MebiBytes(zram_free_k as f64 / 1024.0);
    let zram_used = MebiBytes((zram_total_k - zram_free_k) as f64 / 1024.0);
    let percentage_used = Percentage((total.0 - available.0) / total.0 * 100.0);
    Ok(MetricsMemory {
        percentage_used,
        total,
        available,
        used,
        zram_total,
        zram_available,
        zram_used,
    })
}

#[instrument(skip_all)]
async fn get_disk_info() -> Result<MetricsDisk, Error> {
    let package_used_task = get_used("/embassy-data/package-data");
    let package_available_task = get_available("/embassy-data/package-data");
    let os_used_task = get_used("/embassy-data/main");
    let os_available_task = get_available("/embassy-data/main");

    let (package_used, package_available, os_used, os_available) = futures::try_join!(
        package_used_task,
        package_available_task,
        os_used_task,
        os_available_task,
    )?;

    let total_used = package_used + os_used;
    let total_available = package_available + os_available;
    let total_size = total_used + total_available;
    let total_percentage = total_used as f64 / total_size as f64 * 100.0f64;

    Ok(MetricsDisk {
        capacity: GigaBytes(total_size as f64 / 1_000_000_000.0),
        used: GigaBytes(total_used as f64 / 1_000_000_000.0),
        available: GigaBytes(total_available as f64 / 1_000_000_000.0),
        percentage_used: Percentage(total_percentage as f64),
    })
}

#[tokio::test]
#[ignore]
pub async fn test_get_temp() {
    println!("{}", get_temp().await.unwrap())
}

#[tokio::test]
pub async fn test_get_proc_stat() {
    println!("{:?}", get_proc_stat().await.unwrap())
}

#[tokio::test]
pub async fn test_get_mem_info() {
    println!("{:?}", get_mem_info().await.unwrap())
}

#[tokio::test]
#[ignore]
pub async fn test_get_disk_usage() {
    println!("{:?}", get_disk_info().await.unwrap())
}
