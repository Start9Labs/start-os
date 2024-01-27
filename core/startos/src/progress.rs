use std::panic::UnwindSafe;
use std::sync::Arc;
use std::time::Duration;

use futures::Future;
use imbl_value::{InOMap, InternedString};
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncSeek, AsyncWrite};
use tokio::sync::{mpsc, watch};

use crate::db::model::DatabaseModel;
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref SPINNER: ProgressStyle = ProgressStyle::with_template("{spinner} {wide_msg}...").unwrap();
    static ref PERCENTAGE: ProgressStyle = ProgressStyle::with_template("{msg} {percent}% {wide_bar} [{bytes}/{total_bytes}] [{binary_bytes_per_sec} {eta}]").unwrap();
    static ref BYTES: ProgressStyle = ProgressStyle::with_template("{spinner} {wide_msg} [{bytes}/?] [{binary_bytes_per_sec} {elapsed}]").unwrap();
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(untagged)]
pub enum Progress {
    Complete(bool),
    Progress { done: u64, total: Option<u64> },
}
impl Progress {
    pub fn new() -> Self {
        Progress::Complete(false)
    }
    pub fn update_bar(self, bar: &ProgressBar) {
        match self {
            Self::Complete(false) => {
                bar.set_style(SPINNER.clone());
                bar.tick();
            }
            Self::Complete(true) => {
                bar.finish();
            }
            Self::Progress { done, total: None } => {
                bar.set_style(BYTES.clone());
                bar.set_position(done);
                bar.tick();
            }
            Self::Progress {
                done,
                total: Some(total),
            } => {
                bar.set_style(PERCENTAGE.clone());
                bar.set_position(done);
                bar.set_length(total);
                bar.tick();
            }
        }
    }
    pub fn set_done(&mut self, done: u64) {
        *self = match *self {
            Self::Complete(false) => Self::Progress { done, total: None },
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
            Self::Complete(false) => Self::Progress {
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
}
impl std::ops::Add<u64> for Progress {
    type Output = Self;
    fn add(self, rhs: u64) -> Self::Output {
        match self {
            Self::Complete(false) => Self::Progress {
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

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct NamedProgress {
    pub name: InternedString,
    pub progress: Progress,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
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

pub struct FullProgressTracker {
    overall: Arc<watch::Sender<Progress>>,
    phases: watch::Sender<InOMap<InternedString, watch::Receiver<Progress>>>,
    new_phase: (
        mpsc::UnboundedSender<(InternedString, watch::Receiver<Progress>)>,
        mpsc::UnboundedReceiver<(InternedString, watch::Receiver<Progress>)>,
    ),
}
impl FullProgressTracker {
    pub fn new() -> Self {
        Self {
            overall: Arc::new(watch::channel(Progress::new()).0),
            phases: watch::channel(InOMap::new()).0,
            new_phase: mpsc::unbounded_channel(),
        }
    }
    fn fill_phases(&mut self) {
        while let Ok((name, phase)) = self.new_phase.1.try_recv() {
            self.phases.send_modify(|p| {
                p.insert(name, phase);
            });
        }
    }
    pub fn snapshot(&mut self) -> FullProgress {
        self.fill_phases();
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
    pub async fn changed(&mut self) {
        let mut phase_cardinality = self.phases.subscribe();
        let phase_cardinality_changed = phase_cardinality.changed();
        self.fill_phases();
        let mut overall = self.overall.subscribe();
        let mut phases = self
            .phases
            .borrow()
            .iter()
            .map(|(_, w)| w.clone())
            .collect_vec();
        tokio::select! {
            _ = phase_cardinality_changed => (),
            _ = overall.changed() => (),
            _ = futures::future::select_all(phases.iter_mut().map(|p| Box::pin(p.changed()))) => (),
        }
    }
    pub fn handle(&self) -> FullProgressTrackerHandle {
        FullProgressTrackerHandle {
            overall: self.overall.clone(),
            new_phase: self.new_phase.0.clone(),
        }
    }
    pub fn sync_to_db<DerefFn>(
        mut self,
        db: PatchDb,
        deref: DerefFn,
        min_interval: Option<Duration>,
    ) -> impl Future<Output = Result<(), Error>> + 'static
    where
        DerefFn: Fn(&mut DatabaseModel) -> Option<&mut Model<FullProgress>> + 'static,
        for<'a> &'a DerefFn: UnwindSafe + Send,
    {
        async move {
            loop {
                let progress = self.snapshot();
                if db
                    .mutate(|v| {
                        if let Some(p) = deref(v) {
                            p.ser(&progress)?;
                            Ok(false)
                        } else {
                            Ok(true)
                        }
                    })
                    .await?
                {
                    break;
                }
                tokio::join!(self.changed(), async {
                    if let Some(interval) = min_interval {
                        tokio::time::sleep(interval).await
                    } else {
                        futures::future::ready(()).await
                    }
                });
            }
            Ok(())
        }
    }
}

#[derive(Clone)]
pub struct FullProgressTrackerHandle {
    overall: Arc<watch::Sender<Progress>>,
    new_phase: mpsc::UnboundedSender<(InternedString, watch::Receiver<Progress>)>,
}
impl FullProgressTrackerHandle {
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
        let _ = self.new_phase.send((name, recv));
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
    overall: Arc<watch::Sender<Progress>>,
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
                } => done * overall_contribution / total,
                _ => 0,
            };
            if contribution > self.contributed {
                self.overall
                    .send_modify(|o| *o += contribution - self.contributed);
                self.contributed = contribution;
            }
        }
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
        progress.overall.update_bar(&self.overall);
        for (name, bar) in self.phases.iter() {
            if let Some(progress) = progress.phases.iter().find_map(|p| {
                if &p.name == name {
                    Some(p.progress)
                } else {
                    None
                }
            }) {
                progress.update_bar(bar);
            }
        }
    }
}
