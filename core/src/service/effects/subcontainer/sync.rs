use std::ffi::{OsStr, OsString, c_int};
use std::fs::File;
use std::io::{BufRead, BufReader, IsTerminal, Read};
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd, RawFd};
use std::os::unix::process::{CommandExt, ExitStatusExt};
use std::path::{Path, PathBuf};
use std::process::{Command as StdCommand, Stdio};
use std::sync::Arc;

use nix::errno::Errno;
use nix::sched::CloneFlags;
use nix::sys::socket::{
    recvmsg, sendmsg, ControlMessage, ControlMessageOwned, MsgFlags,
};
use nix::unistd::Pid;
use signal_hook::consts::signal::*;
use termion::raw::IntoRawMode;
use tokio::sync::oneshot;

use crate::CAP_1_KiB;
use crate::service::effects::ContainerCliContext;
use crate::service::effects::prelude::*;
use crate::util::io::TermSize;

const FWD_SIGNALS: &[c_int] = &[
    SIGABRT, SIGALRM, SIGCONT, SIGHUP, SIGINT, SIGIO, SIGPIPE, SIGPROF, SIGQUIT, SIGTERM, SIGTRAP,
    SIGTSTP, SIGTTIN, SIGTTOU, SIGURG, SIGUSR1, SIGUSR2, SIGVTALRM,
];

pub fn kill_init(procfs: &Path, chroot: &Path) -> Result<(), Error> {
    if chroot.join("proc/1").exists() {
        let ns_id = procfs::process::Process::new_with_root(chroot.join("proc/1"))
            .with_ctx(|_| (ErrorKind::Filesystem, "open subcontainer procfs"))?
            .namespaces()
            .with_ctx(|_| (ErrorKind::Filesystem, "read subcontainer pid 1 ns"))?
            .0
            .get(OsStr::new("pid"))
            .or_not_found("pid namespace")?
            .identifier;
        for proc in procfs::process::all_processes_with_root(procfs)
            .with_ctx(|_| (ErrorKind::Filesystem, "open procfs"))?
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
                let pids = proc.read::<_, NSPid>("status").with_ctx(|_| {
                    (
                        ErrorKind::Filesystem,
                        lazy_format!("read pid {} NSpid", pid),
                    )
                })?;
                if pids.0.len() == 2 && pids.0[1] == 1 {
                    match nix::sys::signal::kill(
                        Pid::from_raw(pid),
                        Some(nix::sys::signal::SIGKILL),
                    ) {
                        Err(Errno::ESRCH) => Ok(()),
                        a => a,
                    }
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
    Ok(())
}

struct NSPid(Vec<i32>);
impl procfs::FromBufRead for NSPid {
    fn from_buf_read<R: std::io::BufRead>(r: R) -> procfs::ProcResult<Self> {
        for line in r.lines() {
            let line = line?;
            if let Some(row) = line.trim().strip_prefix("NSpid:") {
                return Ok(Self(
                    row.trim()
                        .split_ascii_whitespace()
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

const LOG_DRAIN_SOCK: &str = ".log-drain.sock";

fn log_drain_sock_path(chroot: &Path) -> PathBuf {
    chroot.join(LOG_DRAIN_SOCK)
}

/// Send file descriptors to the log drain socket via SCM_RIGHTS.
/// Best-effort: silently returns on any failure.
fn send_drain_fds(chroot: &Path, fds: &[RawFd]) {
    let sock_path = log_drain_sock_path(chroot);
    let stream = match std::os::unix::net::UnixStream::connect(&sock_path) {
        Ok(s) => s,
        Err(e) => {
            tracing::debug!("no log drain socket: {e}");
            return;
        }
    };
    let cmsg = [ControlMessage::ScmRights(fds)];
    let iov = [std::io::IoSlice::new(&[0u8])];
    if let Err(e) = sendmsg::<()>(stream.as_raw_fd(), &iov, &cmsg, MsgFlags::empty(), None) {
        tracing::warn!("failed to send drain fds: {e}");
    }
}

/// Receive file descriptors from a Unix stream via SCM_RIGHTS.
fn recv_drain_fds(stream: &std::os::unix::net::UnixStream) -> Result<Vec<OwnedFd>, nix::Error> {
    let mut buf = [0u8; 1];
    let mut iov = [std::io::IoSliceMut::new(&mut buf)];
    let mut cmsg_buf = nix::cmsg_space!([RawFd; 4]);
    let msg = recvmsg::<()>(
        stream.as_raw_fd(),
        &mut iov,
        Some(&mut cmsg_buf),
        MsgFlags::empty(),
    )?;
    let mut fds = Vec::new();
    for cmsg in msg.cmsgs()? {
        if let ControlMessageOwned::ScmRights(received) = cmsg {
            for fd in received {
                fds.push(unsafe { OwnedFd::from_raw_fd(fd) });
            }
        }
    }
    Ok(fds)
}

/// Non-blocking drain of the pipe/pty buffer into `out`. The dup'd FD shares
/// the same pipe as the I/O thread, so both consumers write to the caller's
/// stdout/stderr — no data is lost.
fn drain_buffer(fd: &OwnedFd, out: &mut impl std::io::Write) {
    use std::os::fd::AsFd;
    let flags = nix::fcntl::fcntl(fd.as_fd(), nix::fcntl::FcntlArg::F_GETFL).unwrap_or(0);
    let _ = nix::fcntl::fcntl(
        fd.as_fd(),
        nix::fcntl::FcntlArg::F_SETFL(
            nix::fcntl::OFlag::from_bits_truncate(flags) | nix::fcntl::OFlag::O_NONBLOCK,
        ),
    );
    let mut buf = [0u8; CAP_1_KiB];
    loop {
        match nix::unistd::read(fd.as_fd(), &mut buf) {
            Ok(0) | Err(Errno::EAGAIN) | Err(_) => break,
            Ok(n) => {
                out.write_all(&buf[..n]).ok();
            }
        }
    }
    out.flush().ok();
    let _ = nix::fcntl::fcntl(
        fd.as_fd(),
        nix::fcntl::FcntlArg::F_SETFL(nix::fcntl::OFlag::from_bits_truncate(flags)),
    );
}

/// Start a Unix socket listener that accepts FD handoffs from `exec` processes.
/// Received FDs are drained to stderr (which flows to container-runtime's journald).
fn start_drain_listener(chroot: &Path) {
    use std::io::{Read, Write};
    use std::os::unix::net::UnixListener;

    let sock_path = log_drain_sock_path(chroot);
    let _ = std::fs::remove_file(&sock_path);
    let listener = match UnixListener::bind(&sock_path) {
        Ok(l) => l,
        Err(e) => {
            tracing::warn!("failed to bind log drain socket: {e}");
            return;
        }
    };

    std::thread::spawn(move || {
        for stream in listener.incoming() {
            let stream = match stream {
                Ok(s) => s,
                Err(_) => break,
            };
            match recv_drain_fds(&stream) {
                Ok(fds) => {
                    for fd in fds {
                        std::thread::spawn(move || {
                            let mut reader = File::from(fd);
                            let mut buf = [0u8; CAP_1_KiB];
                            loop {
                                match reader.read(&mut buf) {
                                    Ok(0) | Err(_) => break,
                                    Ok(n) => {
                                        let stderr = std::io::stderr();
                                        let mut out = stderr.lock();
                                        out.write_all(&buf[..n]).ok();
                                    }
                                }
                            }
                        });
                    }
                }
                Err(e) => {
                    tracing::warn!("failed to receive drain fds: {e}");
                }
            }
        }
    });
}

#[derive(Debug, Clone, Serialize, Deserialize, Parser)]
#[group(skip)]
pub struct ExecParams {
    #[arg(long, help = "help.arg.force-tty")]
    force_tty: bool,
    #[arg(long, help = "help.arg.force-stderr-tty")]
    force_stderr_tty: bool,
    #[arg(long, help = "help.arg.pty-size")]
    pty_size: Option<TermSize>,
    #[arg(short, long, help = "help.arg.env-variable")]
    env: Vec<String>,
    #[arg(long, help = "help.arg.env-file-path")]
    env_file: Option<PathBuf>,
    #[arg(short, long, help = "help.arg.workdir-path")]
    workdir: Option<PathBuf>,
    #[arg(short, long, help = "help.arg.user-name")]
    user: Option<String>,
    #[arg(help = "help.arg.chroot-path")]
    chroot: PathBuf,
    #[arg(trailing_var_arg = true, help = "help.arg.command-to-execute")]
    command: Vec<OsString>,
}
impl ExecParams {
    fn exec(&self) -> Result<(), Error> {
        let ExecParams {
            env,
            env_file,
            workdir,
            user,
            chroot,
            command,
            ..
        } = self;
        let Some(([command], args)) = command.split_at_checked(1) else {
            return Err(Error::new(
                eyre!("command cannot be empty"),
                ErrorKind::InvalidRequest,
            ));
        };

        let mut cmd = StdCommand::new(command);

        let mut uid = Err(None);
        let mut gid = Err(None);
        let mut needs_home = true;

        if let Some(user) = user {
            if let Some((u, g)) = user.split_once(":") {
                uid = Err(Some(u));
                gid = Err(Some(g));
            } else {
                uid = Err(Some(user));
            }
        }

        if let Some(u) = uid.err().flatten().and_then(|u| u.parse::<u32>().ok()) {
            uid = Ok(u);
        }
        if let Some(g) = gid.err().flatten().and_then(|g| g.parse::<u32>().ok()) {
            gid = Ok(g);
        }

        let mut update_env = |line: &str| {
            if let Some((k, v)) = line.split_once("=") {
                needs_home &= k != "HOME";
                cmd.env(k, v);
            } else {
                tracing::warn!("Invalid line in env: {line}");
            }
        };
        if let Some(f) = env_file {
            let mut lines = BufReader::new(
                File::open(&f).with_ctx(|_| (ErrorKind::Filesystem, format!("open r {f:?}")))?,
            )
            .lines();
            while let Some(line) = lines.next().transpose()? {
                update_env(&line);
            }
        }

        for line in env {
            update_env(&line);
        }

        let needs_gid = Err(None) == gid;
        let mut username = InternedString::intern("root");
        let mut handle_passwd_line = |line: &str| -> Option<()> {
            let l = line.trim();
            let mut split = l.split(":");
            let user = split.next()?;
            match uid {
                Err(Some(u)) if u != user => return None,
                _ => (),
            }
            split.next(); // throw away x
            let u: u32 = split.next()?.parse().ok()?;
            match uid {
                Err(Some(_)) => uid = Ok(u),
                Err(None) if u == 0 => uid = Ok(u),
                Ok(uid) if uid != u => return None,
                _ => (),
            }

            username = user.into();

            if !needs_gid && !needs_home {
                return Some(());
            }
            let g = split.next()?;
            if needs_gid {
                gid = Ok(g.parse().ok()?);
            }

            if needs_home {
                split.next(); // throw away group name

                let home = split.next()?;

                cmd.env("HOME", home);
            }

            Some(())
        };

        let mut lines = BufReader::new(
            File::open(chroot.join("etc/passwd"))
                .with_ctx(|_| (ErrorKind::Filesystem, format!("open r /etc/passwd")))?,
        )
        .lines();
        while let Some(line) = lines.next().transpose()? {
            if handle_passwd_line(&line).is_some() {
                break;
            }
        }

        let mut groups = Vec::new();
        let mut handle_group_line = |line: &str| -> Option<()> {
            let l = line.trim();
            let mut split = l.split(":");
            let name = split.next()?;
            split.next()?; // throw away x
            let g = split.next()?.parse::<u32>().ok()?;
            match gid {
                Err(Some(n)) if n == name => gid = Ok(g),
                _ => (),
            }
            let users = split.next()?;
            if users.split(",").any(|u| u == &*username) {
                groups.push(nix::unistd::Gid::from_raw(g));
            }
            Some(())
        };
        let mut lines = BufReader::new(
            File::open(chroot.join("etc/group"))
                .with_ctx(|_| (ErrorKind::Filesystem, format!("open r /etc/group")))?,
        )
        .lines();
        while let Some(line) = lines.next().transpose()? {
            if handle_group_line(&line).is_none() {
                tracing::warn!("Invalid /etc/group line: {line}");
            }
        }

        std::os::unix::fs::chroot(chroot)
            .with_ctx(|_| (ErrorKind::Filesystem, lazy_format!("chroot {chroot:?}")))?;
        if let Ok(uid) = uid {
            if uid != 0 {
                std::os::unix::fs::chown("/proc/self/fd/0", Some(uid), gid.ok()).ok();
                std::os::unix::fs::chown("/proc/self/fd/1", Some(uid), gid.ok()).ok();
                std::os::unix::fs::chown("/proc/self/fd/2", Some(uid), gid.ok()).ok();
            }
        }
        // Handle credential changes in pre_exec to control the order:
        // setgroups must happen before setgid/setuid (requires CAP_SETGID)
        {
            let set_uid = uid.ok();
            let set_gid = gid.ok();
            unsafe {
                cmd.pre_exec(move || {
                    // Create a new process group so entrypoint scripts that do
                    // kill(0, SIGTERM) don't cascade to other subcontainers.
                    nix::unistd::setsid()
                        .map_err(|e| std::io::Error::from_raw_os_error(e as i32))?;
                    if !groups.is_empty() {
                        nix::unistd::setgroups(&groups)
                            .map_err(|e| std::io::Error::from_raw_os_error(e as i32))?;
                    }
                    if let Some(gid) = set_gid {
                        nix::unistd::setgid(nix::unistd::Gid::from_raw(gid))
                            .map_err(|e| std::io::Error::from_raw_os_error(e as i32))?;
                    }
                    if let Some(uid) = set_uid {
                        nix::unistd::setuid(nix::unistd::Uid::from_raw(uid))
                            .map_err(|e| std::io::Error::from_raw_os_error(e as i32))?;
                    }
                    Ok(())
                });
            }
        }
        cmd.args(args);

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
        force_tty,
        force_stderr_tty,
        pty_size,
        env,
        env_file,
        workdir,
        user,
        chroot,
        command,
    }: ExecParams,
) -> Result<(), Error> {
    use std::io::Write;

    kill_init(Path::new("/proc"), &chroot)?;
    start_drain_listener(&chroot);
    let mut sig = signal_hook::iterator::Signals::new(FWD_SIGNALS)?;
    let (send_pid, recv_pid) = oneshot::channel();
    std::thread::spawn(move || {
        if let Ok(pid) = recv_pid.blocking_recv() {
            for sig in sig.forever() {
                match nix::sys::signal::kill(
                    Pid::from_raw(pid),
                    Some(nix::sys::signal::Signal::try_from(sig).unwrap()),
                ) {
                    Err(Errno::ESRCH) => Ok(()),
                    a => a,
                }
                .unwrap()
            }
        }
    });

    let mut stdin = std::io::stdin();
    let stdout = std::io::stdout();
    let stderr = std::io::stderr();
    let stderr_tty = force_stderr_tty || stderr.is_terminal();

    let tty = force_tty || (stdin.is_terminal() && stdout.is_terminal());

    let raw = if stdin.is_terminal() && stdout.is_terminal() {
        Some(termion::get_tty()?.into_raw_mode()?)
    } else {
        None
    };

    let (stdin_send, stdin_recv) = oneshot::channel::<Box<dyn Write + Send>>();
    std::thread::spawn(move || {
        if let Ok(mut cstdin) = stdin_recv.blocking_recv() {
            if tty {
                let mut buf = [0_u8; CAP_1_KiB];
                while let Ok(n) = stdin.read(&mut buf) {
                    if n == 0 {
                        break;
                    }
                    cstdin.write_all(&buf[..n]).ok();
                    cstdin.flush().ok();
                }
            } else {
                std::io::copy(&mut stdin, &mut cstdin).unwrap();
            }
        }
    });
    let (stdout_send, stdout_recv) = oneshot::channel::<Box<dyn std::io::Read + Send>>();
    let stdout_thread = std::thread::spawn(move || {
        if let Ok(mut cstdout) = stdout_recv.blocking_recv() {
            if tty {
                let mut stdout = stdout.lock();
                let mut buf = [0_u8; CAP_1_KiB];
                while let Ok(n) = cstdout.read(&mut buf) {
                    if n == 0 {
                        break;
                    }
                    stdout.write_all(&buf[..n]).ok();
                    stdout.flush().ok();
                }
            } else {
                std::io::copy(&mut cstdout, &mut stdout.lock()).unwrap();
            }
        }
    });
    let (stderr_send, stderr_recv) = oneshot::channel::<Box<dyn std::io::Read + Send>>();
    let stderr_thread = if !stderr_tty {
        Some(std::thread::spawn(move || {
            if let Ok(mut cstderr) = stderr_recv.blocking_recv() {
                std::io::copy(&mut cstderr, &mut stderr.lock()).unwrap();
            }
        }))
    } else {
        None
    };
    nix::sched::unshare(CloneFlags::CLONE_NEWPID)
        .with_ctx(|_| (ErrorKind::Filesystem, "unshare pid ns"))?;
    nix::sched::unshare(CloneFlags::CLONE_NEWCGROUP)
        .with_ctx(|_| (ErrorKind::Filesystem, "unshare cgroup ns"))?;
    nix::sched::unshare(CloneFlags::CLONE_NEWIPC)
        .with_ctx(|_| (ErrorKind::Filesystem, "unshare ipc ns"))?;

    if tty {
        use pty_process::blocking as pty_process;
        let (pty, pts) = pty_process::open().with_kind(ErrorKind::Filesystem)?;
        let mut cmd = pty_process::Command::new("/usr/bin/start-container");
        cmd = cmd.arg("subcontainer").arg("launch-init");
        for env in env {
            cmd = cmd.arg("-e").arg(env)
        }
        if let Some(env_file) = env_file {
            cmd = cmd.arg("--env-file").arg(env_file);
        }
        if let Some(workdir) = workdir {
            cmd = cmd.arg("--workdir").arg(workdir);
        }
        if let Some(user) = user {
            cmd = cmd.arg("--user").arg(user);
        }
        cmd = cmd.arg(&chroot).args(&command);
        if !stderr_tty {
            cmd = cmd.stderr(Stdio::piped());
        }
        let mut child = cmd
            .spawn(pts)
            .map_err(color_eyre::eyre::Report::msg)
            .with_ctx(|_| (ErrorKind::Filesystem, "spawning child process"))?;
        send_pid.send(child.id() as i32).unwrap_or_default();
        if let Some(pty_size) = pty_size.or_else(|| TermSize::get_current()) {
            let size = if let Some((x, y)) = pty_size.pixels {
                ::pty_process::Size::new_with_pixel(pty_size.rows, pty_size.cols, x, y)
            } else {
                ::pty_process::Size::new(pty_size.rows, pty_size.cols)
            };
            pty.resize(size).with_kind(ErrorKind::Filesystem)?;
        }
        let shared = ArcPty(Arc::new(pty));
        stdin_send
            .send(Box::new(shared.clone()))
            .unwrap_or_default();
        stdout_send
            .send(Box::new(shared.clone()))
            .unwrap_or_default();
        if let Some(stderr) = child.stderr.take() {
            stderr_send.send(Box::new(stderr)).unwrap_or_default();
        }
        let exit = child
            .wait()
            .with_ctx(|_| (ErrorKind::Filesystem, "waiting on child process"))?;
        stdout_thread.join().unwrap();
        stderr_thread.map(|t| t.join().unwrap());
        if let Some(code) = exit.code() {
            drop(raw);
            std::process::exit(code);
        } else if exit.success() || exit.signal() == Some(15) {
            Ok(())
        } else {
            Err(Error::new(
                color_eyre::eyre::Report::msg(exit),
                ErrorKind::Unknown,
            ))
        }
    } else {
        let mut cmd = StdCommand::new("/usr/bin/start-container");
        cmd.arg("subcontainer").arg("launch-init");
        for env in env {
            cmd.arg("-e").arg(env);
        }
        if let Some(env_file) = env_file {
            cmd.arg("--env-file").arg(env_file);
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
        let mut child = cmd
            .spawn()
            .map_err(color_eyre::eyre::Report::msg)
            .with_ctx(|_| (ErrorKind::Filesystem, "spawning child process"))?;
        send_pid.send(child.id() as i32).unwrap_or_default();
        stdin_send
            .send(Box::new(child.stdin.take().unwrap()))
            .unwrap_or_default();
        stdout_send
            .send(Box::new(child.stdout.take().unwrap()))
            .unwrap_or_default();
        stderr_send
            .send(Box::new(child.stderr.take().unwrap()))
            .unwrap_or_default();
        let exit = child
            .wait()
            .with_ctx(|_| (ErrorKind::Filesystem, "waiting on child process"))?;
        stdout_thread.join().unwrap();
        stderr_thread.map(|t| t.join().unwrap());
        if let Some(code) = exit.code() {
            nix::mount::umount(&chroot.join("proc"))
                .with_ctx(|_| (ErrorKind::Filesystem, "umount procfs"))?;
            std::process::exit(code);
        } else if exit.success() || exit.signal() == Some(15) {
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

#[derive(Clone)]
struct ArcPty(Arc<pty_process::blocking::Pty>);
impl std::io::Write for ArcPty {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        (&*self.0).write(buf)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        (&*self.0).flush()
    }
}
impl std::io::Read for ArcPty {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        (&*self.0).read(buf)
    }
}

pub fn exec(
    _: ContainerCliContext,
    ExecParams {
        force_tty,
        force_stderr_tty,
        pty_size,
        env,
        env_file,
        workdir,
        user,
        chroot,
        command,
    }: ExecParams,
) -> Result<(), Error> {
    use std::io::Write;

    let mut sig = signal_hook::iterator::Signals::new(FWD_SIGNALS)?;
    let (send_pid, recv_pid) = oneshot::channel();
    std::thread::spawn(move || {
        if let Ok(pid) = recv_pid.blocking_recv() {
            for sig in sig.forever() {
                match nix::sys::signal::kill(
                    Pid::from_raw(pid),
                    Some(nix::sys::signal::Signal::try_from(sig).unwrap()),
                ) {
                    Err(Errno::ESRCH) => Ok(()),
                    a => a,
                }
                .unwrap();
            }
        }
    });

    let mut stdin = std::io::stdin();
    let stdout = std::io::stdout();
    let stderr = std::io::stderr();
    let stderr_tty = force_stderr_tty || stderr.is_terminal();

    let tty = force_tty || (stdin.is_terminal() && stdout.is_terminal());

    let raw = if stdin.is_terminal() && stdout.is_terminal() {
        Some(termion::get_tty()?.into_raw_mode()?)
    } else {
        None
    };

    let (stdin_send, stdin_recv) = oneshot::channel::<Box<dyn Write + Send>>();
    std::thread::spawn(move || {
        if let Ok(mut cstdin) = stdin_recv.blocking_recv() {
            if tty {
                let mut buf = [0_u8; CAP_1_KiB];
                while let Ok(n) = stdin.read(&mut buf) {
                    if n == 0 {
                        break;
                    }
                    cstdin.write_all(&buf[..n]).ok();
                    cstdin.flush().ok();
                }
            } else {
                std::io::copy(&mut stdin, &mut cstdin).unwrap();
            }
        }
    });
    let (stdout_send, stdout_recv) = oneshot::channel::<Box<dyn std::io::Read + Send>>();
    let _stdout_thread = std::thread::spawn(move || {
        if let Ok(mut cstdout) = stdout_recv.blocking_recv() {
            if tty {
                let mut stdout = stdout.lock();
                let mut buf = [0_u8; CAP_1_KiB];
                while let Ok(n) = cstdout.read(&mut buf) {
                    if n == 0 {
                        break;
                    }
                    stdout.write_all(&buf[..n]).ok();
                    stdout.flush().ok();
                }
            } else {
                std::io::copy(&mut cstdout, &mut stdout.lock()).unwrap();
            }
        }
    });
    let (stderr_send, stderr_recv) = oneshot::channel::<Box<dyn std::io::Read + Send>>();
    let _stderr_thread = if !stderr_tty {
        Some(std::thread::spawn(move || {
            if let Ok(mut cstderr) = stderr_recv.blocking_recv() {
                std::io::copy(&mut cstderr, &mut stderr.lock()).unwrap();
            }
        }))
    } else {
        None
    };
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

    if tty {
        use pty_process::blocking as pty_process;
        let (pty, pts) = pty_process::open().with_kind(ErrorKind::Filesystem)?;
        let mut cmd = pty_process::Command::new("/usr/bin/start-container");
        cmd = cmd.arg("subcontainer").arg("exec-command");
        for env in env {
            cmd = cmd.arg("-e").arg(env);
        }
        if let Some(env_file) = env_file {
            cmd = cmd.arg("--env-file").arg(env_file);
        }
        if let Some(workdir) = workdir {
            cmd = cmd.arg("--workdir").arg(workdir);
        }
        if let Some(user) = user {
            cmd = cmd.arg("--user").arg(user);
        }
        cmd = cmd.arg(&chroot).args(&command);
        if !stderr_tty {
            cmd = cmd.stderr(Stdio::piped());
        }
        let mut child = cmd
            .spawn(pts)
            .map_err(color_eyre::eyre::Report::msg)
            .with_ctx(|_| (ErrorKind::Filesystem, "spawning child process"))?;
        send_pid.send(child.id() as i32).unwrap_or_default();
        if let Some(pty_size) = pty_size.or_else(|| TermSize::get_current()) {
            let size = if let Some((x, y)) = pty_size.pixels {
                ::pty_process::Size::new_with_pixel(pty_size.rows, pty_size.cols, x, y)
            } else {
                ::pty_process::Size::new(pty_size.rows, pty_size.cols)
            };
            pty.resize(size).with_kind(ErrorKind::Filesystem)?;
        }

        // Dup the PTY master (and separate stderr if piped) for drain handoff
        let pty_drain = nix::unistd::dup(&pty)
            .with_ctx(|_| (ErrorKind::Filesystem, "dup pty for drain"))?;
        let mut drain_fds = vec![pty_drain];
        if let Some(stderr) = child.stderr.take() {
            let stderr_drain = nix::unistd::dup(&stderr)
                .with_ctx(|_| (ErrorKind::Filesystem, "dup stderr for drain"))?;
            drain_fds.push(stderr_drain);
            stderr_send.send(Box::new(stderr)).unwrap_or_default();
        }

        let shared = ArcPty(Arc::new(pty));
        stdin_send
            .send(Box::new(shared.clone()))
            .unwrap_or_default();
        stdout_send
            .send(Box::new(shared.clone()))
            .unwrap_or_default();

        let exit = child
            .wait()
            .with_ctx(|_| (ErrorKind::Filesystem, "waiting on child process"))?;

        // Flush any remaining pipe buffer data to the caller, then hand off
        // the dup'd FDs to launch for grandchild log capture.
        // We cannot join the I/O threads because grandchildren may hold the
        // pipe write ends open indefinitely.
        drain_buffer(&drain_fds[0], &mut std::io::stdout().lock());
        if drain_fds.len() > 1 {
            drain_buffer(&drain_fds[1], &mut std::io::stderr().lock());
        }
        let raw_fds: Vec<RawFd> = drain_fds.iter().map(|fd| fd.as_raw_fd()).collect();
        send_drain_fds(&chroot, &raw_fds);
        drop(drain_fds);

        if let Some(code) = exit.code() {
            drop(raw);
            std::process::exit(code);
        } else if exit.success() {
            Ok(())
        } else {
            Err(Error::new(
                color_eyre::eyre::Report::msg(exit),
                ErrorKind::Unknown,
            ))
        }
    } else {
        let mut cmd = StdCommand::new("/usr/bin/start-container");
        cmd.arg("subcontainer").arg("exec-command");
        for env in env {
            cmd.arg("-e").arg(env);
        }
        if let Some(env_file) = env_file {
            cmd.arg("--env-file").arg(env_file);
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
        let mut child = cmd
            .spawn()
            .map_err(color_eyre::eyre::Report::msg)
            .with_ctx(|_| (ErrorKind::Filesystem, "spawning child process"))?;
        send_pid.send(child.id() as i32).unwrap_or_default();

        // Take pipe handles and dup them for later handoff to the drain listener.
        // We dup before sending the originals to I/O threads.
        let child_stdout = child.stdout.take().unwrap();
        let child_stderr = child.stderr.take().unwrap();
        let stdout_drain = nix::unistd::dup(&child_stdout)
            .with_ctx(|_| (ErrorKind::Filesystem, "dup stdout for drain"))?;
        let stderr_drain = nix::unistd::dup(&child_stderr)
            .with_ctx(|_| (ErrorKind::Filesystem, "dup stderr for drain"))?;

        stdin_send
            .send(Box::new(child.stdin.take().unwrap()))
            .unwrap_or_default();
        stdout_send
            .send(Box::new(child_stdout))
            .unwrap_or_default();
        stderr_send
            .send(Box::new(child_stderr))
            .unwrap_or_default();

        let exit = child
            .wait()
            .with_ctx(|_| (ErrorKind::Filesystem, "waiting on child process"))?;

        // Flush any remaining pipe buffer data to the caller, then hand off
        // the dup'd FDs to launch for grandchild log capture.
        // We cannot join the I/O threads because grandchildren may hold the
        // pipe write ends open indefinitely.
        drain_buffer(&stdout_drain, &mut std::io::stdout().lock());
        drain_buffer(&stderr_drain, &mut std::io::stderr().lock());
        send_drain_fds(&chroot, &[stdout_drain.as_raw_fd(), stderr_drain.as_raw_fd()]);
        drop(stdout_drain);
        drop(stderr_drain);

        if let Some(code) = exit.code() {
            std::process::exit(code);
        } else if exit.success() || exit.signal() == Some(15) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use std::os::fd::AsFd;
    use std::os::unix::net::UnixListener;
    use std::sync::atomic::{AtomicU32, Ordering};

    static TEST_COUNTER: AtomicU32 = AtomicU32::new(0);

    fn test_dir() -> PathBuf {
        let n = TEST_COUNTER.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!(
            "startos-test-drain-{}-{}",
            std::process::id(),
            n,
        ));
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    /// Create an anonymous pipe, returning (read_end, write_end) as OwnedFds.
    fn pipe() -> (OwnedFd, OwnedFd) {
        let (r, w) = nix::unistd::pipe().unwrap();
        (r, w)
    }

    #[test]
    fn test_send_recv_drain_fds() {
        let dir = test_dir();
        let sock_path = dir.join(LOG_DRAIN_SOCK);
        let listener = UnixListener::bind(&sock_path).unwrap();

        let (pipe_r, pipe_w) = pipe();
        let raw_fd = pipe_r.as_raw_fd();

        // Send the read end of the pipe
        let send_thread = std::thread::spawn(move || {
            let stream = std::os::unix::net::UnixStream::connect(&sock_path).unwrap();
            let cmsg = [ControlMessage::ScmRights(&[raw_fd])];
            let iov = [std::io::IoSlice::new(&[0u8])];
            sendmsg::<()>(stream.as_raw_fd(), &iov, &cmsg, MsgFlags::empty(), None).unwrap();
            drop(pipe_r); // close our copy after sending
        });

        // Receive on the listener side
        let (stream, _) = listener.accept().unwrap();
        let fds = recv_drain_fds(&stream).unwrap();
        send_thread.join().unwrap();

        assert_eq!(fds.len(), 1, "should receive exactly one FD");

        // Write through the pipe and read from the received FD
        let mut writer = File::from(pipe_w);
        writer.write_all(b"hello from pipe").unwrap();
        drop(writer);

        let mut reader = File::from(fds.into_iter().next().unwrap());
        let mut buf = String::new();
        std::io::Read::read_to_string(&mut reader, &mut buf).unwrap();
        assert_eq!(buf, "hello from pipe");
    }

    #[test]
    fn test_drain_buffer_reads_available_data() {
        let (pipe_r, pipe_w) = pipe();

        let mut writer = File::from(pipe_w);
        writer.write_all(b"buffered data").unwrap();
        drop(writer);

        let mut output = Vec::new();
        drain_buffer(&pipe_r, &mut output);

        assert_eq!(output, b"buffered data");
    }

    #[test]
    fn test_drain_buffer_returns_immediately_on_empty_pipe() {
        let (pipe_r, _pipe_w) = pipe();

        // Pipe is open but empty — should return immediately (EAGAIN)
        let mut output = Vec::new();
        drain_buffer(&pipe_r, &mut output);

        assert!(output.is_empty(), "empty pipe should yield no data");
    }

    #[test]
    fn test_drain_buffer_restores_blocking_mode() {
        let (pipe_r, _pipe_w) = pipe();

        let flags_before =
            nix::fcntl::fcntl(pipe_r.as_fd(), nix::fcntl::FcntlArg::F_GETFL).unwrap();
        drain_buffer(&pipe_r, &mut Vec::new());
        let flags_after =
            nix::fcntl::fcntl(pipe_r.as_fd(), nix::fcntl::FcntlArg::F_GETFL).unwrap();

        assert_eq!(flags_before, flags_after, "flags should be restored");
    }

    #[test]
    fn test_dup_preserves_pipe_after_original_closed() {
        // Simulates the real flow: dup the pipe, close the original,
        // verify data is still readable from the dup'd FD.
        let (pipe_r, pipe_w) = pipe();

        let drain_fd = nix::unistd::dup(pipe_r.as_fd()).unwrap();

        let mut writer = File::from(pipe_w);
        writer.write_all(b"child output").unwrap();
        drop(writer);

        // Close original — pipe stays alive through drain_fd
        drop(pipe_r);

        let mut reader = File::from(drain_fd);
        let mut buf = String::new();
        std::io::Read::read_to_string(&mut reader, &mut buf).unwrap();
        assert_eq!(buf, "child output");
    }

    #[test]
    fn test_start_drain_listener_end_to_end() {
        // Full integration: start the listener, send a pipe FD, write data,
        // verify the listener drains it.
        let chroot = test_dir();

        start_drain_listener(&chroot);

        // Give the listener thread a moment to bind
        std::thread::sleep(std::time::Duration::from_millis(50));

        let (pipe_r, pipe_w) = pipe();

        // Send the pipe read end to the drain listener
        send_drain_fds(&chroot, &[pipe_r.as_raw_fd()]);
        drop(pipe_r); // close our copy; listener has its own

        // Write data — the listener should drain it (to stderr in production,
        // but we just verify the pipe doesn't block/hang)
        let mut writer = File::from(pipe_w);
        for _ in 0..100 {
            writer.write_all(b"log line\n").unwrap();
        }
        drop(writer);

        // Give the drain thread time to process
        std::thread::sleep(std::time::Duration::from_millis(100));
        // If we get here without hanging, the test passes.
    }

    #[test]
    fn test_send_drain_fds_no_socket_does_not_panic() {
        // When no drain socket exists, send_drain_fds should silently return
        let dir = test_dir();
        let (pipe_r, _pipe_w) = pipe();
        send_drain_fds(&dir, &[pipe_r.as_raw_fd()]);
        // No panic, no error — best-effort
    }
}
