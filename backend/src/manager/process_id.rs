use nix::unistd::Pid;
use serde::{Deserialize, Serialize};

/// Know what the process is called
#[derive(Debug, Serialize, Deserialize, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcessId(pub u32);
impl From<ProcessId> for Pid {
    fn from(pid: ProcessId) -> Self {
        Pid::from_raw(pid.0 as i32)
    }
}
impl From<Pid> for ProcessId {
    fn from(pid: Pid) -> Self {
        ProcessId(pid.as_raw() as u32)
    }
}
impl From<i32> for ProcessId {
    fn from(pid: i32) -> Self {
        ProcessId(pid as u32)
    }
}
