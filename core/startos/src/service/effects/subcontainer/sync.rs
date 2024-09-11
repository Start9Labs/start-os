use std::borrow::Cow;
use std::collections::BTreeMap;
use std::ffi::{c_int, OsStr, OsString};
use std::fs::File;
use std::io::IsTerminal;
use std::os::unix::process::CommandExt;
use std::path::{Path, PathBuf};
use std::process::{Command as StdCommand, Stdio};

use nix::sched::CloneFlags;
use nix::unistd::Pid;
use signal_hook::consts::signal::*;
use tokio::sync::oneshot;
use tty_spawn::TtySpawn;
use unshare::Command as NSCommand;

use crate::service::effects::prelude::*;
use crate::service::effects::ContainerCliContext;

const FWD_SIGNALS: &[c_int] = &[
    SIGABRT, SIGALRM, SIGCONT, SIGHUP, SIGINT, SIGIO, SIGPIPE, SIGPROF, SIGQUIT, SIGTERM, SIGTRAP,
    SIGTSTP, SIGTTIN, SIGTTOU, SIGURG, SIGUSR1, SIGUSR2, SIGVTALRM,
];

struct NSPid(Vec<i32>);
impl procfs::FromBufRead for NSPid {
    fn from_buf_read<R: std::io::BufRead>(r: R) -> procfs::ProcResult<Self> {
        for line in r.lines() {
            let line = line?;
            if let Some(row) = line.trim().strip_prefix("NSpid") {
                return Ok(Self(
                    row.split_ascii_whitespace()
                        .map(|pid| pid.parse::<i32>())
                        .collect::<Result<Vec<_>, _>>()?,
                ));
            }
        }
        Err(procfs::ProcError::Incomplete(None))
    }
}

fn open_file_read(path: impl AsRef<Path>) -> Result<File, Error> {
    File::open(&path).with_ctx(|_| {
        (
            ErrorKind::Filesystem,
            lazy_format!("open r {}", path.as_ref().display()),
        )
    })
}

#[derive(Debug, Clone, Serialize, Deserialize, Parser)]
pub struct ExecParams {
    #[arg(short = 'e', long = "env")]
    env: Option<PathBuf>,
    #[arg(short = 'w', long = "workdir")]
    workdir: Option<PathBuf>,
    #[arg(short = 'u', long = "user")]
    user: Option<String>,
    chroot: PathBuf,
    #[arg(trailing_var_arg = true)]
    command: Vec<OsString>,
}
impl ExecParams {
    fn exec(&self) -> Result<(), Error> {
        let ExecParams {
            env,
            workdir,
            user,
            chroot,
            command,
        } = self;
        let Some(([command], args)) = command.split_at_checked(1) else {
            return Err(Error::new(
                eyre!("command cannot be empty"),
                ErrorKind::InvalidRequest,
            ));
        };
        let env_string = if let Some(env) = &env {
            std::fs::read_to_string(env)
                .with_ctx(|_| (ErrorKind::Filesystem, lazy_format!("read {env:?}")))?
        } else {
            Default::default()
        };
        let env = env_string
            .lines()
            .map(|l| l.trim())
            .filter_map(|l| l.split_once("="))
            .collect::<BTreeMap<_, _>>();
        std::os::unix::fs::chroot(chroot)
            .with_ctx(|_| (ErrorKind::Filesystem, lazy_format!("chroot {chroot:?}")))?;
        let command = which::which_in(
            command,
            env.get("PATH")
                .copied()
                .map(Cow::Borrowed)
                .or_else(|| std::env::var("PATH").ok().map(Cow::Owned))
                .as_deref(),
            workdir.as_deref().unwrap_or(Path::new("/")),
        )
        .with_kind(ErrorKind::Filesystem)?;
        let mut cmd = StdCommand::new(command);
        cmd.args(args);
        for (k, v) in env {
            cmd.env(k, v);
        }

        if let Some(uid) = user.as_deref().and_then(|u| u.parse::<u32>().ok()) {
            cmd.uid(uid);
        } else if let Some(user) = user {
            let (uid, gid) = std::fs::read_to_string("/etc/passwd")
                .with_ctx(|_| (ErrorKind::Filesystem, "read /etc/passwd"))?
                .lines()
                .find_map(|l| {
                    let mut split = l.trim().split(":");
                    if user != split.next()? {
                        return None;
                    }
                    split.next(); // throw away x
                    Some((split.next()?.parse().ok()?, split.next()?.parse().ok()?))
                    // uid gid
                })
                .or_not_found(lazy_format!("{user} in /etc/passwd"))?;
            cmd.uid(uid);
            cmd.gid(gid);
        };
        if let Some(workdir) = workdir {
            cmd.current_dir(workdir);
        } else {
            cmd.current_dir("/");
        }
        Err(cmd.exec().into())
    }
}

pub fn launch(
    _: ContainerCliContext,
    ExecParams {
        env,
        workdir,
        user,
        chroot,
        command,
    }: ExecParams,
) -> Result<(), Error> {
    use unshare::{Namespace, Stdio};

    use crate::service::cli::ContainerCliContext;
    let mut sig = signal_hook::iterator::Signals::new(FWD_SIGNALS)?;
    let mut cmd = NSCommand::new("/usr/bin/start-cli");
    cmd.arg("subcontainer").arg("launch-init");
    if let Some(env) = env {
        cmd.arg("--env").arg(env);
    }
    if let Some(workdir) = workdir {
        cmd.arg("--workdir").arg(workdir);
    }
    if let Some(user) = user {
        cmd.arg("--user").arg(user);
    }
    cmd.arg(&chroot);
    cmd.args(&command);
    cmd.unshare(&[Namespace::Pid, Namespace::Cgroup, Namespace::Ipc]);
    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());
    let (stdin_send, stdin_recv) = oneshot::channel();
    std::thread::spawn(move || {
        if let Ok(mut stdin) = stdin_recv.blocking_recv() {
            std::io::copy(&mut std::io::stdin(), &mut stdin).unwrap();
        }
    });
    let (stdout_send, stdout_recv) = oneshot::channel();
    std::thread::spawn(move || {
        if let Ok(mut stdout) = stdout_recv.blocking_recv() {
            std::io::copy(&mut stdout, &mut std::io::stdout()).unwrap();
        }
    });
    let (stderr_send, stderr_recv) = oneshot::channel();
    std::thread::spawn(move || {
        if let Ok(mut stderr) = stderr_recv.blocking_recv() {
            std::io::copy(&mut stderr, &mut std::io::stderr()).unwrap();
        }
    });
    if chroot.join("proc/1").exists() {
        let ns_id = procfs::process::Process::new_with_root(chroot.join("proc"))
            .with_ctx(|_| (ErrorKind::Filesystem, "open subcontainer procfs"))?
            .namespaces()
            .with_ctx(|_| (ErrorKind::Filesystem, "read subcontainer pid 1 ns"))?
            .0
            .get(OsStr::new("pid"))
            .or_not_found("pid namespace")?
            .identifier;
        for proc in
            procfs::process::all_processes().with_ctx(|_| (ErrorKind::Filesystem, "open procfs"))?
        {
            let proc = proc.with_ctx(|_| (ErrorKind::Filesystem, "read single process details"))?;
            let pid = proc.pid();
            if proc
                .namespaces()
                .with_ctx(|_| (ErrorKind::Filesystem, lazy_format!("read pid {} ns", pid)))?
                .0
                .get(OsStr::new("pid"))
                .map_or(false, |ns| ns.identifier == ns_id)
            {
                let pids = proc.read::<NSPid>("status").with_ctx(|_| {
                    (
                        ErrorKind::Filesystem,
                        lazy_format!("read pid {} NSpid", pid),
                    )
                })?;
                if pids.0.len() == 2 && pids.0[1] == 1 {
                    nix::sys::signal::kill(Pid::from_raw(pid), nix::sys::signal::SIGKILL)
                        .with_ctx(|_| {
                            (
                                ErrorKind::Filesystem,
                                lazy_format!(
                                    "kill pid {} (determined to be pid 1 in subcontainer)",
                                    pid
                                ),
                            )
                        })?;
                }
            }
        }
        nix::mount::umount(&chroot.join("proc"))
            .with_ctx(|_| (ErrorKind::Filesystem, "unmounting subcontainer procfs"))?;
    }
    let mut child = cmd
        .spawn()
        .map_err(color_eyre::eyre::Report::msg)
        .with_ctx(|_| (ErrorKind::Filesystem, "spawning child process"))?;
    let pid = child.pid();
    std::thread::spawn(move || {
        for sig in sig.forever() {
            nix::sys::signal::kill(
                Pid::from_raw(pid),
                Some(nix::sys::signal::Signal::try_from(sig).unwrap()),
            )
            .unwrap();
        }
    });
    stdin_send
        .send(child.stdin.take().unwrap())
        .unwrap_or_default();
    stdout_send
        .send(child.stdout.take().unwrap())
        .unwrap_or_default();
    stderr_send
        .send(child.stderr.take().unwrap())
        .unwrap_or_default();
    // TODO: subreaping, signal handling
    let exit = child
        .wait()
        .with_ctx(|_| (ErrorKind::Filesystem, "waiting on child process"))?;
    if let Some(code) = exit.code() {
        std::process::exit(code);
    } else {
        if exit.success() {
            Ok(())
        } else {
            Err(Error::new(
                color_eyre::eyre::Report::msg(exit),
                ErrorKind::Unknown,
            ))
        }
    }
}

pub fn launch_init(_: ContainerCliContext, params: ExecParams) -> Result<(), Error> {
    nix::mount::mount(
        Some("proc"),
        &params.chroot.join("proc"),
        Some("proc"),
        nix::mount::MsFlags::empty(),
        None::<&str>,
    )
    .with_ctx(|_| (ErrorKind::Filesystem, "mount procfs"))?;
    if params.command.is_empty() {
        signal_hook::iterator::Signals::new(signal_hook::consts::TERM_SIGNALS)?
            .forever()
            .next();
        std::process::exit(0)
    } else {
        params.exec()
    }
}

pub fn exec(
    _: ContainerCliContext,
    ExecParams {
        env,
        workdir,
        user,
        chroot,
        command,
    }: ExecParams,
) -> Result<(), Error> {
    if std::io::stdin().is_terminal() {
        let mut cmd = TtySpawn::new("/usr/bin/start-cli");
        cmd.arg("subcontainer").arg("exec-command");
        if let Some(env) = env {
            cmd.arg("--env").arg(env);
        }
        if let Some(workdir) = workdir {
            cmd.arg("--workdir").arg(workdir);
        }
        if let Some(user) = user {
            cmd.arg("--user").arg(user);
        }
        cmd.arg(&chroot);
        cmd.args(command.iter());
        nix::sched::setns(
            open_file_read(chroot.join("proc/1/ns/pid"))?,
            CloneFlags::CLONE_NEWPID,
        )
        .with_ctx(|_| (ErrorKind::Filesystem, "set pid ns"))?;
        nix::sched::setns(
            open_file_read(chroot.join("proc/1/ns/cgroup"))?,
            CloneFlags::CLONE_NEWCGROUP,
        )
        .with_ctx(|_| (ErrorKind::Filesystem, "set cgroup ns"))?;
        nix::sched::setns(
            open_file_read(chroot.join("proc/1/ns/ipc"))?,
            CloneFlags::CLONE_NEWIPC,
        )
        .with_ctx(|_| (ErrorKind::Filesystem, "set ipc ns"))?;
        std::process::exit(cmd.spawn().with_kind(ErrorKind::Filesystem)?);
    }
    let mut sig = signal_hook::iterator::Signals::new(FWD_SIGNALS)?;
    let (send_pid, recv_pid) = oneshot::channel();
    std::thread::spawn(move || {
        if let Ok(pid) = recv_pid.blocking_recv() {
            for sig in sig.forever() {
                nix::sys::signal::kill(
                    Pid::from_raw(pid),
                    Some(nix::sys::signal::Signal::try_from(sig).unwrap()),
                )
                .unwrap();
            }
        }
    });
    let mut cmd = StdCommand::new("/usr/bin/start-cli");
    cmd.arg("subcontainer").arg("exec-command");
    if let Some(env) = env {
        cmd.arg("--env").arg(env);
    }
    if let Some(workdir) = workdir {
        cmd.arg("--workdir").arg(workdir);
    }
    if let Some(user) = user {
        cmd.arg("--user").arg(user);
    }
    cmd.arg(&chroot);
    cmd.args(&command);
    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());
    let (stdin_send, stdin_recv) = oneshot::channel();
    std::thread::spawn(move || {
        if let Ok(mut stdin) = stdin_recv.blocking_recv() {
            std::io::copy(&mut std::io::stdin(), &mut stdin).unwrap();
        }
    });
    let (stdout_send, stdout_recv) = oneshot::channel();
    std::thread::spawn(move || {
        if let Ok(mut stdout) = stdout_recv.blocking_recv() {
            std::io::copy(&mut stdout, &mut std::io::stdout()).unwrap();
        }
    });
    let (stderr_send, stderr_recv) = oneshot::channel();
    std::thread::spawn(move || {
        if let Ok(mut stderr) = stderr_recv.blocking_recv() {
            std::io::copy(&mut stderr, &mut std::io::stderr()).unwrap();
        }
    });
    nix::sched::setns(
        open_file_read(chroot.join("proc/1/ns/pid"))?,
        CloneFlags::CLONE_NEWPID,
    )
    .with_ctx(|_| (ErrorKind::Filesystem, "set pid ns"))?;
    nix::sched::setns(
        open_file_read(chroot.join("proc/1/ns/cgroup"))?,
        CloneFlags::CLONE_NEWCGROUP,
    )
    .with_ctx(|_| (ErrorKind::Filesystem, "set cgroup ns"))?;
    nix::sched::setns(
        open_file_read(chroot.join("proc/1/ns/ipc"))?,
        CloneFlags::CLONE_NEWIPC,
    )
    .with_ctx(|_| (ErrorKind::Filesystem, "set ipc ns"))?;
    let mut child = cmd
        .spawn()
        .map_err(color_eyre::eyre::Report::msg)
        .with_ctx(|_| (ErrorKind::Filesystem, "spawning child process"))?;
    send_pid.send(child.id() as i32).unwrap_or_default();
    stdin_send
        .send(child.stdin.take().unwrap())
        .unwrap_or_default();
    stdout_send
        .send(child.stdout.take().unwrap())
        .unwrap_or_default();
    stderr_send
        .send(child.stderr.take().unwrap())
        .unwrap_or_default();
    let exit = child
        .wait()
        .with_ctx(|_| (ErrorKind::Filesystem, "waiting on child process"))?;
    if let Some(code) = exit.code() {
        std::process::exit(code);
    } else {
        if exit.success() {
            Ok(())
        } else {
            Err(Error::new(
                color_eyre::eyre::Report::msg(exit),
                ErrorKind::Unknown,
            ))
        }
    }
}

pub fn exec_command(_: ContainerCliContext, params: ExecParams) -> Result<(), Error> {
    params.exec()
}