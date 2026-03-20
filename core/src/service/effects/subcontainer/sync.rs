use std::ffi::{OsStr, OsString, c_int};
use std::fs::File;
use std::io::{BufRead, BufReader, IsTerminal, Read};
use std::os::unix::process::{CommandExt, ExitStatusExt};
use std::path::{Path, PathBuf};
use std::process::{Command as StdCommand, Stdio};
use std::sync::Arc;

use nix::errno::Errno;
use nix::sched::CloneFlags;
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

        // TODO: subreaping, signal handling
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
