use std::panic::UnwindSafe;
use std::time::Duration;

use futures::future::pending;
use futures::stream::BoxStream;
use futures::{Future, FutureExt, StreamExt, TryFutureExt};
use helpers::NonDetachingJoinHandle;
use imbl_value::{InOMap, InternedString};
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncSeek, AsyncWrite};
use tokio::sync::watch;
use ts_rs::TS;

use crate::db::model::{Database, DatabaseModel};
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref SPINNER: ProgressStyle = ProgressStyle::with_template("{spinner} {msg}...").unwrap();
    static ref PERCENTAGE: ProgressStyle = ProgressStyle::with_template("{msg} {percent}% {wide_bar} [{human_pos}/{human_len}] [{per_sec} {eta}]").unwrap();
    static ref PERCENTAGE_BYTES: ProgressStyle = ProgressStyle::with_template("{msg} {percent}% {wide_bar} [{binary_bytes}/{binary_total_bytes}] [{binary_bytes_per_sec} {eta}]").unwrap();
    static ref STEPS: ProgressStyle = ProgressStyle::with_template("{spinner} {wide_msg} [{human_pos}/?] [{per_sec} {elapsed}]").unwrap();
    static ref BYTES: ProgressStyle = ProgressStyle::with_template("{spinner} {wide_msg} [{bytes}/?] [{binary_bytes_per_sec} {elapsed}]").unwrap();
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, TS)]
#[serde(untagged)]
pub enum Progress {
    NotStarted(()),
    Complete(bool),
    Progress {
        #[ts(type = "number")]
        done: u64,
        #[ts(type = "number | null")]
        total: Option<u64>,
    },
}
impl Progress {
    pub fn new() -> Self {
        Progress::NotStarted(())
    }
    pub fn update_bar(self, bar: &ProgressBar, bytes: bool) {
        match self {
            Self::NotStarted(()) => {
                bar.set_style(SPINNER.clone());
            }
            Self::Complete(false) => {
                bar.set_style(SPINNER.clone());
                bar.tick();
            }
            Self::Complete(true) => {
                bar.finish();
            }
            Self::Progress { done, total: None } => {
                if bytes {
                    bar.set_style(BYTES.clone());
                } else {
                    bar.set_style(STEPS.clone());
                }
                bar.set_position(done);
                bar.tick();
            }
            Self::Progress {
                done,
                total: Some(total),
            } => {
                if bytes {
                    bar.set_style(PERCENTAGE_BYTES.clone());
                } else {
                    bar.set_style(PERCENTAGE.clone());
                }
                bar.set_position(done);
                bar.set_length(total);
                bar.tick();
            }
        }
    }
    pub fn start(&mut self) {
        *self = match *self {
            Self::NotStarted(()) => Self::Complete(false),
            a => a,
        };
    }
    pub fn set_done(&mut self, done: u64) {
        *self = match *self {
            Self::Complete(false) | Self::NotStarted(()) => Self::Progress { done, total: None },
            Self::Progress { mut done, total } => {
                if let Some(total) = total {
                    if done > total {
                        done = total;
                    }
                }
                Self::Progress { done, total }
            }
            Self::Complete(true) => Self::Complete(true),
        };
    }
    pub fn set_total(&mut self, total: u64) {
        *self = match *self {
            Self::Complete(false) | Self::NotStarted(()) => Self::Progress {
                done: 0,
                total: Some(total),
            },
            Self::Progress { done, .. } => Self::Progress {
                done,
                total: Some(total),
            },
            Self::Complete(true) => Self::Complete(true),
        }
    }
    pub fn add_total(&mut self, total: u64) {
        if let Self::Progress {
            done,
            total: Some(old),
        } = *self
        {
            *self = Self::Progress {
                done,
                total: Some(old + total),
            };
        } else {
            self.set_total(total)
        }
    }
    pub fn complete(&mut self) {
        *self = Self::Complete(true);
    }
    pub fn is_complete(&self) -> bool {
        matches!(self, Self::Complete(true))
    }
}
impl std::ops::Add<u64> for Progress {
    type Output = Self;
    fn add(self, rhs: u64) -> Self::Output {
        match self {
            Self::Complete(false) | Self::NotStarted(()) => Self::Progress {
                done: rhs,
                total: None,
            },
            Self::Progress { done, total } => {
                let mut done = done + rhs;
                if let Some(total) = total {
                    if done > total {
                        done = total;
                    }
                }
                Self::Progress { done, total }
            }
            Self::Complete(true) => Self::Complete(true),
        }
    }
}
impl std::ops::AddAssign<u64> for Progress {
    fn add_assign(&mut self, rhs: u64) {
        *self = *self + rhs;
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[ts(export)]
pub struct NamedProgress {
    #[ts(type = "string")]
    pub name: InternedString,
    pub progress: Progress,
}

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[ts(export)]
pub struct FullProgress {
    pub overall: Progress,
    pub phases: Vec<NamedProgress>,
}
impl FullProgress {
    pub fn new() -> Self {
        Self {
            overall: Progress::new(),
            phases: Vec::new(),
        }
    }
}

#[derive(Clone)]
pub struct FullProgressTracker {
    overall: watch::Sender<Progress>,
    phases: watch::Sender<InOMap<InternedString, watch::Receiver<Progress>>>,
}
impl FullProgressTracker {
    pub fn new() -> Self {
        let (overall, _) = watch::channel(Progress::new());
        let (phases, _) = watch::channel(InOMap::new());
        Self { overall, phases }
    }
    pub fn snapshot(&self) -> FullProgress {
        FullProgress {
            overall: *self.overall.borrow(),
            phases: self
                .phases
                .borrow()
                .iter()
                .map(|(name, progress)| NamedProgress {
                    name: name.clone(),
                    progress: *progress.borrow(),
                })
                .collect(),
        }
    }
    pub fn stream(&self, min_interval: Option<Duration>) -> BoxStream<'static, FullProgress> {
        struct StreamState {
            overall: watch::Receiver<Progress>,
            phases_recv: watch::Receiver<InOMap<InternedString, watch::Receiver<Progress>>>,
            phases: InOMap<InternedString, watch::Receiver<Progress>>,
        }
        let mut overall = self.overall.subscribe();
        overall.mark_changed(); // make sure stream starts with a value
        let phases_recv = self.phases.subscribe();
        let phases = phases_recv.borrow().clone();
        let state = StreamState {
            overall,
            phases_recv,
            phases,
        };
        futures::stream::unfold(
            state,
            move |StreamState {
                      mut overall,
                      mut phases_recv,
                      mut phases,
                  }| async move {
                let changed = phases
                    .iter_mut()
                    .map(|(_, p)| async move { p.changed().or_else(|_| pending()).await }.boxed())
                    .chain([overall.changed().boxed()])
                    .chain([phases_recv.changed().boxed()])
                    .map(|fut| fut.map(|r| r.unwrap_or_default()))
                    .collect_vec();
                if let Some(min_interval) = min_interval {
                    tokio::join!(
                        tokio::time::sleep(min_interval),
                        futures::future::select_all(changed),
                    );
                } else {
                    futures::future::select_all(changed).await;
                }

                for (name, phase) in &*phases_recv.borrow_and_update() {
                    if !phases.contains_key(name) {
                        phases.insert(name.clone(), phase.clone());
                    }
                }

                let o = *overall.borrow_and_update();

                Some((
                    FullProgress {
                        overall: o,
                        phases: phases
                            .iter_mut()
                            .map(|(name, progress)| NamedProgress {
                                name: name.clone(),
                                progress: *progress.borrow_and_update(),
                            })
                            .collect(),
                    },
                    StreamState {
                        overall,
                        phases_recv,
                        phases,
                    },
                ))
            },
        )
        .boxed()
    }
    pub fn sync_to_db<DerefFn>(
        &self,
        db: TypedPatchDb<Database>,
        deref: DerefFn,
        min_interval: Option<Duration>,
    ) -> impl Future<Output = Result<(), Error>> + 'static
    where
        DerefFn: Fn(&mut DatabaseModel) -> Option<&mut Model<FullProgress>> + 'static,
        for<'a> &'a DerefFn: UnwindSafe + Send,
    {
        let mut stream = self.stream(min_interval);
        async move {
            while let Some(progress) = stream.next().await {
                if db
                    .mutate(|v| {
                        if let Some(p) = deref(v) {
                            p.ser(&progress)?;
                            Ok(progress.overall.is_complete())
                        } else {
                            Ok(true)
                        }
                    })
                    .await?
                {
                    break;
                }
            }
            Ok(())
        }
    }
    pub fn progress_bar_task(&self, name: &str) -> NonDetachingJoinHandle<()> {
        let mut stream = self.stream(None);
        let mut bar = PhasedProgressBar::new(name);
        tokio::spawn(async move {
            while let Some(progress) = stream.next().await {
                bar.update(&progress);
                if progress.overall.is_complete() {
                    break;
                }
            }
        })
        .into()
    }
    pub fn add_phase(
        &self,
        name: InternedString,
        overall_contribution: Option<u64>,
    ) -> PhaseProgressTrackerHandle {
        if let Some(overall_contribution) = overall_contribution {
            self.overall
                .send_modify(|o| o.add_total(overall_contribution));
        }
        let (send, recv) = watch::channel(Progress::new());
        self.phases.send_modify(|p| {
            p.insert(name, recv);
        });
        PhaseProgressTrackerHandle {
            overall: self.overall.clone(),
            overall_contribution,
            contributed: 0,
            progress: send,
        }
    }
    pub fn complete(&self) {
        self.overall.send_modify(|o| o.complete());
    }
}

pub struct PhaseProgressTrackerHandle {
    overall: watch::Sender<Progress>,
    overall_contribution: Option<u64>,
    contributed: u64,
    progress: watch::Sender<Progress>,
}
impl PhaseProgressTrackerHandle {
    fn update_overall(&mut self) {
        if let Some(overall_contribution) = self.overall_contribution {
            let contribution = match *self.progress.borrow() {
                Progress::Complete(true) => overall_contribution,
                Progress::Progress {
                    done,
                    total: Some(total),
                } => ((done as f64 / total as f64) * overall_contribution as f64) as u64,
                _ => 0,
            };
            if contribution > self.contributed {
                self.overall
                    .send_modify(|o| *o += contribution - self.contributed);
                self.contributed = contribution;
            }
        }
    }
    pub fn start(&mut self) {
        self.progress.send_modify(|p| p.start());
    }
    pub fn set_done(&mut self, done: u64) {
        self.progress.send_modify(|p| p.set_done(done));
        self.update_overall();
    }
    pub fn set_total(&mut self, total: u64) {
        self.progress.send_modify(|p| p.set_total(total));
        self.update_overall();
    }
    pub fn add_total(&mut self, total: u64) {
        self.progress.send_modify(|p| p.add_total(total));
        self.update_overall();
    }
    pub fn complete(&mut self) {
        self.progress.send_modify(|p| p.complete());
        self.update_overall();
    }
    pub fn writer<W>(self, writer: W) -> ProgressTrackerWriter<W> {
        ProgressTrackerWriter {
            writer,
            progress: self,
        }
    }
}
impl std::ops::AddAssign<u64> for PhaseProgressTrackerHandle {
    fn add_assign(&mut self, rhs: u64) {
        self.progress.send_modify(|p| *p += rhs);
        self.update_overall();
    }
}

#[pin_project::pin_project]
pub struct ProgressTrackerWriter<W> {
    #[pin]
    writer: W,
    progress: PhaseProgressTrackerHandle,
}
impl<W> ProgressTrackerWriter<W> {
    pub fn new(writer: W, progress: PhaseProgressTrackerHandle) -> Self {
        Self { writer, progress }
    }
    pub fn into_inner(self) -> (W, PhaseProgressTrackerHandle) {
        (self.writer, self.progress)
    }
}
impl<W: AsyncWrite> AsyncWrite for ProgressTrackerWriter<W> {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<Result<usize, std::io::Error>> {
        let this = self.project();
        match this.writer.poll_write(cx, buf) {
            std::task::Poll::Ready(Ok(n)) => {
                *this.progress += n as u64;
                std::task::Poll::Ready(Ok(n))
            }
            a => a,
        }
    }
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        self.project().writer.poll_flush(cx)
    }
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        self.project().writer.poll_shutdown(cx)
    }
    fn is_write_vectored(&self) -> bool {
        self.writer.is_write_vectored()
    }
    fn poll_write_vectored(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        bufs: &[std::io::IoSlice<'_>],
    ) -> std::task::Poll<Result<usize, std::io::Error>> {
        self.project().writer.poll_write_vectored(cx, bufs)
    }
}
impl<W: AsyncSeek> AsyncSeek for ProgressTrackerWriter<W> {
    fn start_seek(
        self: std::pin::Pin<&mut Self>,
        position: std::io::SeekFrom,
    ) -> std::io::Result<()> {
        self.project().writer.start_seek(position)
    }
    fn poll_complete(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::io::Result<u64>> {
        let this = self.project();
        match this.writer.poll_complete(cx) {
            std::task::Poll::Ready(Ok(n)) => {
                this.progress.set_done(n);
                std::task::Poll::Ready(Ok(n))
            }
            a => a,
        }
    }
}

pub struct PhasedProgressBar {
    multi: MultiProgress,
    overall: ProgressBar,
    phases: InOMap<InternedString, ProgressBar>,
}
impl PhasedProgressBar {
    pub fn new(name: &str) -> Self {
        let multi = MultiProgress::new();
        Self {
            overall: multi.add(
                ProgressBar::new(0)
                    .with_style(SPINNER.clone())
                    .with_message(name.to_owned()),
            ),
            multi,
            phases: InOMap::new(),
        }
    }
    pub fn update(&mut self, progress: &FullProgress) {
        for phase in progress.phases.iter() {
            if !self.phases.contains_key(&phase.name) {
                self.phases.insert(
                    phase.name.clone(),
                    self.multi
                        .add(ProgressBar::new(0).with_style(SPINNER.clone()))
                        .with_message((&*phase.name).to_owned()),
                );
            }
        }
        progress.overall.update_bar(&self.overall, false);
        for (name, bar) in self.phases.iter() {
            if let Some(progress) = progress.phases.iter().find_map(|p| {
                if &p.name == name {
                    Some(p.progress)
                } else {
                    None
                }
            }) {
                progress.update_bar(bar, true);
            }
        }
    }
}
