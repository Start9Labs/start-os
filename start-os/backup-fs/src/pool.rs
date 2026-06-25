//! Key-sharded worker pool for offloading blocking backend I/O from the
//! FUSE dispatch thread.
//!
//! fuser's `Session::run` is a single-threaded loop. If a filesystem
//! handler blocks (stuck CIFS write, long ext4 journal commit,
//! unresponsive USB), every other FUSE request queues behind it and
//! rsync workers stall.
//!
//! The pool decouples request receipt from the work itself: the
//! dispatch thread parses arguments, hands the `Reply*` to a worker via
//! a channel, then returns to accept the next request. fuser's
//! `Reply*` types are `Send + 'static`, so this is mechanically safe —
//! fuser does not care who replies or when, as long as every request
//! eventually gets exactly one reply.
//!
//! **Ordering matters.** Two writes for the same file submitted in
//! order must land at their destination in order, and an fsync after
//! them must observe their effects. A naive MPMC pool where N workers
//! race to `recv()` can reorder same-file ops. We fix that by sharding:
//! each job carries a routing key (typically the inode number), and
//! jobs with the same key always land on the same worker — preserving
//! per-file FIFO while still running different files in parallel.
//!
//! Different keys may collide onto the same worker (head-of-line
//! blocking within a shard). For a backup workload with thousands of
//! inodes across 32 shards that collision is rare and self-limiting.

use std::num::NonZeroUsize;
use std::sync::OnceLock;
use std::thread::JoinHandle;

use crossbeam_channel::{bounded, Receiver, Sender};

type Job = Box<dyn FnOnce() + Send + 'static>;

pub struct WorkerPool {
    /// One sender per worker. `submit_for(key)` picks `senders[key % N]`.
    senders: Vec<Sender<Job>>,
    handles: Vec<JoinHandle<()>>,
}

impl WorkerPool {
    pub fn new(size: NonZeroUsize, per_worker_queue: usize) -> Self {
        let n = size.get();
        let mut senders = Vec::with_capacity(n);
        let mut handles = Vec::with_capacity(n);
        for i in 0..n {
            let (tx, rx) = bounded::<Job>(per_worker_queue);
            senders.push(tx);
            handles.push(
                std::thread::Builder::new()
                    .name(format!("backup-fs-worker-{i}"))
                    .spawn(move || worker_loop(rx))
                    .expect("spawn worker"),
            );
        }
        Self { senders, handles }
    }

    /// Route a job to the shard identified by `key`. All jobs submitted
    /// with the same key execute serially on the same worker, in the
    /// order they were submitted.
    pub fn submit_for<F: FnOnce() + Send + 'static>(&self, key: u64, f: F) {
        let idx = (key as usize) % self.senders.len();
        // send blocks if the per-worker queue is full — that back-
        // pressures the dispatch thread rather than letting memory
        // grow unbounded under a sustained stall.
        let _ = self.senders[idx].send(Box::new(f));
    }
}

impl Drop for WorkerPool {
    fn drop(&mut self) {
        // Close every sender; each worker's recv() returns Err and the
        // thread exits. Join them so any in-flight I/O finishes before
        // the process shuts down.
        self.senders.clear();
        for h in self.handles.drain(..) {
            let _ = h.join();
        }
    }
}

fn worker_loop(rx: Receiver<Job>) {
    while let Ok(job) = rx.recv() {
        // Catch panics so one buggy job doesn't permanently reduce pool
        // capacity. The originating FUSE request won't get a reply;
        // that's a protocol leak we trade for resilience.
        let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(job));
    }
}

/// Global pool. `BACKUPFS_WORKERS` overrides; default is min(32,
/// 2 * logical CPUs).
pub fn global() -> &'static WorkerPool {
    static POOL: OnceLock<WorkerPool> = OnceLock::new();
    POOL.get_or_init(|| {
        let default = std::thread::available_parallelism()
            .map(|p| p.get().saturating_mul(2).min(32))
            .unwrap_or(8);
        let size = std::env::var("BACKUPFS_WORKERS")
            .ok()
            .and_then(|s| s.parse::<usize>().ok())
            .filter(|&n| n > 0)
            .unwrap_or(default);
        // Per-worker queue depth of 8 — enough to absorb a request
        // burst without starving the dispatch thread when back-
        // pressure kicks in.
        WorkerPool::new(NonZeroUsize::new(size).unwrap(), 8)
    })
}
