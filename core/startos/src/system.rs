use std::collections::BTreeSet;
use std::fmt;
use std::sync::Arc;
use std::time::Duration;

use chrono::Utc;
use clap::Parser;
use color_eyre::eyre::eyre;
use futures::{FutureExt, TryStreamExt};
use imbl::vector;
use imbl_value::InternedString;
use rpc_toolkit::{from_fn_async, Context, Empty, HandlerExt, ParentHandler};
use rustls::RootCertStore;
use rustls_pki_types::CertificateDer;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use tokio::process::Command;
use tokio::sync::broadcast::Receiver;
use tracing::instrument;
use ts_rs::TS;

use crate::context::{CliContext, RpcContext};
use crate::disk::util::{get_available, get_used};
use crate::logs::{LogSource, LogsParams, SYSTEM_UNIT};
use crate::prelude::*;
use crate::rpc_continuations::{Guid, RpcContinuation, RpcContinuations};
use crate::shutdown::Shutdown;
use crate::util::cpupower::{get_available_governors, set_governor, Governor};
use crate::util::io::open_file;
use crate::util::net::WebSocketExt;
use crate::util::serde::{display_serializable, HandlerExtSerde, WithIoFormat};
use crate::util::sync::Watch;
use crate::util::Invoke;
use crate::{MAIN_DATA, PACKAGE_DATA};

pub fn experimental<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand(
            "zram",
            from_fn_async(zram)
                .no_display()
                .with_about("Enable zram")
                .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "governor",
            from_fn_async(governor)
                .with_display_serializable()
                .with_custom_display_fn(|handle, result| {
                    Ok(display_governor_info(handle.params, result))
                })
                .with_about("Show current and available CPU governors")
                .with_call_remote::<CliContext>(),
        )
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

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct ZramParams {
    enable: bool,
}

pub async fn zram(ctx: RpcContext, ZramParams { enable }: ZramParams) -> Result<(), Error> {
    let db = ctx.db.peek().await;

    let zram = db.as_public().as_server_info().as_zram().de()?;
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
            v.as_public_mut()
                .as_server_info_mut()
                .as_zram_mut()
                .ser(&enable)?;
            Ok(())
        })
        .await
        .result?;
    Ok(())
}

#[derive(Debug, Deserialize, Serialize)]
pub struct GovernorInfo {
    current: Option<Governor>,
    available: BTreeSet<Governor>,
}

fn display_governor_info(params: WithIoFormat<GovernorParams>, result: GovernorInfo) {
    use prettytable::*;

    if let Some(format) = params.format {
        return display_serializable(format, params);
    }

    let mut table = Table::new();
    table.add_row(row![bc -> "GOVERNORS"]);
    for entry in result.available {
        if Some(&entry) == result.current.as_ref() {
            table.add_row(row![g -> format!("* {entry} (current)")]);
        } else {
            table.add_row(row![entry]);
        }
    }
    table.print_tty(false).unwrap();
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct GovernorParams {
    set: Option<Governor>,
}

pub async fn governor(
    ctx: RpcContext,
    GovernorParams { set, .. }: GovernorParams,
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
            .mutate(|d| {
                d.as_public_mut()
                    .as_server_info_mut()
                    .as_governor_mut()
                    .ser(&Some(set))
            })
            .await
            .result?;
    }
    let current = ctx
        .db
        .peek()
        .await
        .as_public()
        .as_server_info()
        .as_governor()
        .de()?;
    Ok(GovernorInfo { current, available })
}

#[derive(Serialize, Deserialize)]
pub struct TimeInfo {
    now: String,
    uptime: u64,
}

pub fn display_time(params: WithIoFormat<Empty>, arg: TimeInfo) {
    use std::fmt::Write;

    use prettytable::*;

    if let Some(format) = params.format {
        return display_serializable(format, arg);
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

pub async fn time(ctx: RpcContext, _: Empty) -> Result<TimeInfo, Error> {
    Ok(TimeInfo {
        now: Utc::now().to_rfc3339(),
        uptime: ctx.start_time.elapsed().as_secs(),
    })
}

pub fn logs<C: Context + AsRef<RpcContinuations>>() -> ParentHandler<C, LogsParams> {
    crate::logs::logs(|_: &C, _| async { Ok(LogSource::Unit(SYSTEM_UNIT)) })
}

pub fn kernel_logs<C: Context + AsRef<RpcContinuations>>() -> ParentHandler<C, LogsParams> {
    crate::logs::logs(|_: &C, _| async { Ok(LogSource::Kernel) })
}

const DISABLE_KIOSK_PATH: &str =
    "/media/startos/config/overlay/etc/systemd/system/getty@tty1.service.d/autologin.conf";

pub async fn sync_kiosk(kiosk: Option<bool>) -> Result<(), Error> {
    if let Some(kiosk) = kiosk {
        if kiosk {
            enable_kiosk().await?;
        } else {
            disable_kiosk().await?;
        }
    }
    Ok(())
}

pub async fn enable_kiosk() -> Result<(), Error> {
    if tokio::fs::metadata(DISABLE_KIOSK_PATH).await.is_ok() {
        crate::util::io::delete_file(DISABLE_KIOSK_PATH).await?;
    }
    Ok(())
}

pub async fn disable_kiosk() -> Result<(), Error> {
    crate::util::io::create_file(DISABLE_KIOSK_PATH)
        .await?
        .sync_all()
        .await?;
    Ok(())
}

pub fn kiosk<C: Context>() -> ParentHandler<C> {
    ParentHandler::<C>::new()
        .subcommand(
            "enable",
            from_fn_async(|ctx: RpcContext| async move {
                ctx.db
                    .mutate(|db| {
                        db.as_public_mut()
                            .as_server_info_mut()
                            .as_kiosk_mut()
                            .ser(&Some(true))
                    })
                    .await
                    .result?;
                enable_kiosk().await
            })
            .no_display()
            .with_about("Enable kiosk mode")
            .with_call_remote::<CliContext>(),
        )
        .subcommand(
            "disable",
            from_fn_async(|ctx: RpcContext| async move {
                ctx.db
                    .mutate(|db| {
                        db.as_public_mut()
                            .as_server_info_mut()
                            .as_kiosk_mut()
                            .ser(&Some(false))
                    })
                    .await
                    .result?;
                disable_kiosk().await
            })
            .no_display()
            .with_about("Disable kiosk mode")
            .with_call_remote::<CliContext>(),
        )
}

#[derive(Serialize, Deserialize)]
pub struct MetricLeaf<T> {
    value: T,
    unit: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, TS)]
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
#[derive(Clone, Debug, PartialEq, PartialOrd, TS)]
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

#[derive(Clone, Debug, TS)]
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

#[derive(Clone, Debug, PartialEq, PartialOrd, TS)]
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

#[derive(Deserialize, Serialize, Clone, Debug, TS)]
#[serde(rename_all = "camelCase")]
pub struct MetricsGeneral {
    pub temperature: Option<Celsius>,
}

#[derive(Deserialize, Serialize, Clone, Debug, TS)]
#[serde(rename_all = "camelCase")]
pub struct MetricsMemory {
    pub percentage_used: Percentage,
    pub total: MebiBytes,
    pub available: MebiBytes,
    pub used: MebiBytes,
    pub zram_total: MebiBytes,
    pub zram_available: MebiBytes,
    pub zram_used: MebiBytes,
}

#[derive(Deserialize, Serialize, Clone, Debug, TS)]
#[serde(rename_all = "camelCase")]
pub struct MetricsCpu {
    percentage_used: Percentage,
    idle: Percentage,
    user_space: Percentage,
    kernel_space: Percentage,
    wait: Percentage,
}

#[derive(Deserialize, Serialize, Clone, Debug, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
pub struct MetricsDisk {
    percentage_used: Percentage,
    used: GigaBytes,
    available: GigaBytes,
    capacity: GigaBytes,
}

#[derive(Deserialize, Serialize, Clone, Debug, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct Metrics {
    general: MetricsGeneral,
    memory: MetricsMemory,
    cpu: MetricsCpu,
    disk: MetricsDisk,
}

// #[command(display(display_serializable))]
pub async fn metrics(ctx: RpcContext) -> Result<Metrics, Error> {
    ctx.metrics_cache.read().or_not_found("No Metrics Found")
}

#[derive(Deserialize, Serialize, Clone, Debug, TS)]
#[serde(rename_all = "camelCase")]
pub struct MetricsFollowResponse {
    pub guid: Guid,
    pub metrics: Metrics,
}

#[derive(Deserialize, Serialize, Parser, TS)]
#[serde(rename_all = "camelCase")]
#[command(rename_all = "kebab-case")]
pub struct MetricsFollowParams {
    #[ts(skip)]
    #[serde(rename = "__auth_session")] // from Auth middleware
    session: Option<InternedString>,
}

pub async fn metrics_follow(
    ctx: RpcContext,
    MetricsFollowParams { session }: MetricsFollowParams,
) -> Result<MetricsFollowResponse, Error> {
    let mut local_cache = ctx.metrics_cache.clone();
    let metrics = local_cache
        .peek_and_mark_seen(|m| m.clone())
        .or_not_found("No Metrics Found")?;
    let guid = Guid::new();
    ctx.rpc_continuations
        .add(
            guid.clone(),
            RpcContinuation::ws_authed(
                ctx.clone(),
                session,
                |mut ws| async move {
                    let res = async {
                        loop {
                            use axum::extract::ws::Message;
                            tokio::select! {
                                _ = local_cache.changed() => {
                                    ws.send(Message::Text(
                                        local_cache
                                            .peek(|m| serde_json::to_string(&m))
                                            .with_kind(ErrorKind::Serialization)?
                                    )).await.with_kind(ErrorKind::Network)?;
                                }
                                msg = ws.try_next() => {
                                    if msg.with_kind(crate::ErrorKind::Network)?.is_none() {
                                        break;
                                    }
                                }
                            }
                        }
                        Ok::<_, Error>("complete")
                    }
                    .await;
                    ws.close_result(res).await.log_err();
                },
                Duration::from_secs(30),
            ),
        )
        .await;
    Ok(MetricsFollowResponse { guid, metrics })
}

pub async fn launch_metrics_task<F: FnMut() -> Receiver<Option<Shutdown>>>(
    cache: &Watch<Option<Metrics>>,
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

    let should_launch_temp_task = init_temp.is_some();

    cache.send(Some(Metrics {
        general: MetricsGeneral {
            temperature: init_temp,
        },
        memory: init_mem,
        cpu: init_cpu,
        disk: init_disk,
    }));

    let mut task_vec = Vec::new();
    // launch persistent temp task
    if should_launch_temp_task {
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
    cache: &Watch<Option<Metrics>>,
    mut shutdown: Receiver<Option<Shutdown>>,
) {
    loop {
        match get_temp().await {
            Ok(a) => {
                cache.send_if_modified(|c| {
                    c.as_mut().unwrap().general.temperature.replace(a) != Some(a)
                });
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
    cache: &Watch<Option<Metrics>>,
    mut init: ProcStat,
    mut shutdown: Receiver<Option<Shutdown>>,
) {
    loop {
        // read /proc/stat, diff against previous metrics, compute cpu load
        match get_cpu_info(&mut init).await {
            Ok(info) => {
                cache.send_modify(|c| c.as_mut().unwrap().cpu = info);
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

async fn launch_mem_task(cache: &Watch<Option<Metrics>>, mut shutdown: Receiver<Option<Shutdown>>) {
    loop {
        // read /proc/meminfo
        match get_mem_info().await {
            Ok(a) => {
                cache.send_modify(|c| c.as_mut().unwrap().memory = a);
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
    cache: &Watch<Option<Metrics>>,
    mut shutdown: Receiver<Option<Shutdown>>,
) {
    loop {
        // run df and capture output
        match get_disk_info().await {
            Ok(a) => {
                cache.send_if_modified(|c| {
                    let c = c.as_mut().unwrap();
                    if c.disk != a {
                        c.disk = a;
                        true
                    } else {
                        false
                    }
                });
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
    let _n = tokio::io::BufReader::new(open_file("/proc/stat").await?)
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
        Err(Error::new(
            eyre!(
                "Columns missing from /proc/stat. Need 10, found {}",
                stats.len()
            ),
            ErrorKind::ParseSysInfo,
        ))
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
    let package_used_task = get_used(PACKAGE_DATA);
    let package_available_task = get_available(PACKAGE_DATA);
    let os_used_task = get_used(MAIN_DATA);
    let os_available_task = get_available(MAIN_DATA);

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

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Parser, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct SmtpValue {
    #[arg(long)]
    pub server: String,
    #[arg(long)]
    pub port: u16,
    #[arg(long)]
    pub from: String,
    #[arg(long)]
    pub login: String,
    #[arg(long)]
    pub password: Option<String>,
}
pub async fn set_system_smtp(ctx: RpcContext, smtp: SmtpValue) -> Result<(), Error> {
    let smtp = Some(smtp);
    ctx.db
        .mutate(|db| {
            db.as_public_mut()
                .as_server_info_mut()
                .as_smtp_mut()
                .ser(&smtp)
        })
        .await
        .result?;
    if let Some(callbacks) = ctx.callbacks.get_system_smtp() {
        callbacks.call(vector![to_value(&smtp)?]).await?;
    }
    Ok(())
}
pub async fn clear_system_smtp(ctx: RpcContext) -> Result<(), Error> {
    ctx.db
        .mutate(|db| {
            db.as_public_mut()
                .as_server_info_mut()
                .as_smtp_mut()
                .ser(&None)
        })
        .await
        .result?;
    if let Some(callbacks) = ctx.callbacks.get_system_smtp() {
        callbacks.call(vector![Value::Null]).await?;
    }
    Ok(())
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Parser, TS)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct TestSmtpParams {
    #[arg(long)]
    pub server: String,
    #[arg(long)]
    pub port: u16,
    #[arg(long)]
    pub from: String,
    #[arg(long)]
    pub to: String,
    #[arg(long)]
    pub login: String,
    #[arg(long)]
    pub password: Option<String>,
}
pub async fn test_smtp(
    _: RpcContext,
    TestSmtpParams {
        server,
        port,
        from,
        to,
        login,
        password,
    }: TestSmtpParams,
) -> Result<(), Error> {
    #[cfg(feature = "mail-send")]
    {
        use mail_send::mail_builder::{self, MessageBuilder};
        use mail_send::SmtpClientBuilder;
        use rustls_pki_types::pem::PemObject;

        let Some(pass_val) = password else {
            return Err(Error::new(
                eyre!("mail-send requires a password"),
                ErrorKind::InvalidRequest,
            ));
        };

        let mut root_cert_store = RootCertStore::empty();
        let pem = tokio::fs::read("/etc/ssl/certs/ca-certificates.crt").await?;
        for cert in CertificateDer::pem_slice_iter(&pem) {
            root_cert_store.add_parsable_certificates([cert.with_kind(ErrorKind::OpenSsl)?]);
        }

        let cfg = Arc::new(
            rustls::ClientConfig::builder_with_provider(Arc::new(
                rustls::crypto::ring::default_provider(),
            ))
            .with_safe_default_protocol_versions()?
            .with_root_certificates(root_cert_store)
            .with_no_client_auth(),
        );
        let client = SmtpClientBuilder::new_with_tls_config(server, port, cfg)
            .implicit_tls(false)
            .credentials((login.split("@").next().unwrap().to_owned(), pass_val));

        fn parse_address<'a>(addr: &'a str) -> mail_builder::headers::address::Address<'a> {
            if addr.find("<").map_or(false, |start| {
                addr.find(">").map_or(false, |end| start < end)
            }) {
                addr.split_once("<")
                    .map(|(name, addr)| (name.trim(), addr.strip_suffix(">").unwrap_or(addr)))
                    .unwrap()
                    .into()
            } else {
                addr.into()
            }
        }

        let message = MessageBuilder::new()
            .from(parse_address(&from))
            .to(parse_address(&to))
            .subject("StartOS Test Email")
            .text_body("This is a test email sent from your StartOS Server");
        client
            .connect()
            .await
            .map_err(|e| {
                Error::new(
                    eyre!("mail-send connection error: {:?}", e),
                    ErrorKind::Unknown,
                )
            })?
            .send(message)
            .await
            .map_err(|e| Error::new(eyre!("mail-send send error: {:?}", e), ErrorKind::Unknown))?;
        Ok(())
    }
    #[cfg(not(feature = "mail-send"))]
    Err(Error::new(
        eyre!("test-smtp requires mail-send feature to be enabled"),
        ErrorKind::InvalidRequest,
    ))
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
