use std::ffi::OsString;
use std::os::unix::process::CommandExt;
use std::path::{Path, PathBuf};
use std::process::Command as StdCommand;
use std::sync::Arc;
use std::time::Duration;

use rpc_toolkit::Context;
use unshare::Command as NSCommand;

use crate::service::effects::prelude::*;

#[derive(Debug, Clone, Serialize, Deserialize, Parser)]
pub struct ExecParams {
    #[arg(short = 'e', long = "env")]
    env: Option<PathBuf>,
    #[arg(short = 'w', long = "workdir")]
    workdir: Option<PathBuf>,
    #[arg(short = 'u', long = "user")]
    user: Option<String>,
    path: PathBuf,
    command: Vec<OsString>,
}
pub fn launch<C: Context>(
    _: C,
    ExecParams {
        env,
        workdir,
        user,
        path,
        command,
    }: ExecParams,
) -> Result<(), Error> {
    use unshare::{Namespace, Stdio};
    let tmpdir = TmpDir::new()?;
    let stdin_fifo = tmpdir.join("stdin");
    unix_named_pipe::create(&stdin_fifo, Some(0o644))?;
    let stdout_fifo = tmpdir.join("stdout");
    unix_named_pipe::create(&stdout_fifo, Some(0o666))?;
    let stderr_fifo = tmpdir.join("stderr");
    unix_named_pipe::create(&stderr_fifo, Some(0o666))?;
    let mut cmd = NSCommand::new("start-cli");
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
    cmd.arg(path);
    cmd.args(&command);
    cmd.unshare(&[Namespace::Pid, Namespace::Cgroup, Namespace::Ipc]);
    cmd.stdin(Stdio::from_file(unix_named_pipe::open_read(&stdin_fifo)?));
    cmd.stdout(Stdio::from_file(unix_named_pipe::open_write(&stdout_fifo)?));
    cmd.stderr(Stdio::from_file(unix_named_pipe::open_write(&stderr_fifo)?));
    let mut stdin = unix_named_pipe::open_write(&stdin_fifo)?;
    let mut stdout = unix_named_pipe::open_read(&stdout_fifo)?;
    let mut stderr = unix_named_pipe::open_read(&stderr_fifo)?;
    std::thread::spawn(move || {
        std::io::copy(&mut std::io::stdin(), &mut stdin).unwrap();
    });
    std::thread::spawn(move || {
        std::io::copy(&mut stdout, &mut std::io::stdout()).unwrap();
    });
    std::thread::spawn(move || {
        std::io::copy(&mut stderr, &mut std::io::stderr()).unwrap();
    });
    let mut child = cmd
        .spawn()
        .map_err(|e| Error::new(color_eyre::eyre::Report::msg(e), ErrorKind::Filesystem))?;
    // TODO: subreaping, signal handling
    let exit = child.wait()?;
    tmpdir.delete().unwrap();
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
pub fn exec<C: Context>(
    _: C,
    ExecParams {
        env,
        workdir,
        user,
        path,
        command,
    }: ExecParams,
) -> Result<(), Error> {
    use unshare::{Namespace, Stdio};
    let tmpdir = TmpDir::new()?;
    let stdin_fifo = tmpdir.join("stdin");
    unix_named_pipe::create(&stdin_fifo, Some(0o644))?;
    let stdout_fifo = tmpdir.join("stdout");
    unix_named_pipe::create(&stdout_fifo, Some(0o666))?;
    let stderr_fifo = tmpdir.join("stderr");
    unix_named_pipe::create(&stderr_fifo, Some(0o666))?;
    let mut cmd = NSCommand::new("start-cli");
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
    cmd.arg(path);
    cmd.args(&command);
    cmd.unshare(&[Namespace::Pid, Namespace::Cgroup, Namespace::Ipc]);
    cmd.stdin(Stdio::from_file(unix_named_pipe::open_read(&stdin_fifo)?));
    cmd.stdout(Stdio::from_file(unix_named_pipe::open_write(&stdout_fifo)?));
    cmd.stderr(Stdio::from_file(unix_named_pipe::open_write(&stderr_fifo)?));
    let mut stdin = unix_named_pipe::open_write(&stdin_fifo)?;
    let mut stdout = unix_named_pipe::open_read(&stdout_fifo)?;
    let mut stderr = unix_named_pipe::open_read(&stderr_fifo)?;
    std::thread::spawn(move || {
        std::io::copy(&mut std::io::stdin(), &mut stdin).unwrap();
    });
    std::thread::spawn(move || {
        std::io::copy(&mut stdout, &mut std::io::stdout()).unwrap();
    });
    std::thread::spawn(move || {
        std::io::copy(&mut stderr, &mut std::io::stderr()).unwrap();
    });
    let mut child = cmd
        .spawn()
        .map_err(|e| Error::new(color_eyre::eyre::Report::msg(e), ErrorKind::Filesystem))?;
    // TODO: subreaping, signal handling
    let exit = child.wait()?;
    tmpdir.delete().unwrap();
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

pub fn launch_init<C: Context>(
    _: C,
    ExecParams {
        env,
        workdir,
        user,
        path,
        command,
    }: ExecParams,
) -> Result<(), Error> {
    if Path::new("/proc/1").exists() {
        nix::mount::umount(target)
    }
    StdCommand::new("mount").arg("-t").arg("proc").arg("proc").arg("/proc")
    if let Some(cmd) = command.get(0) {
        let mut cmd = StdCommand::new(cmd);
        if let Some(env) = env {
            for (k, v) in std::fs::read_to_string(env)?
                .lines()
                .map(|l| l.trim())
                .filter_map(|l| l.split_once("="))
            {
                cmd.env(k, v);
            }
        }
        std::os::unix::fs::chroot(path)?;
        if let Some(uid) = user.as_deref().and_then(|u| u.parse::<u32>().ok()) {
            cmd.uid(uid);
        } else if let Some(user) = user {
            let (uid, gid) = std::fs::read_to_string("/etc/passwd")?
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
        }
        cmd.args(&command[1..]);
        Err(cmd.exec().into())
    } else {
        loop {
            std::thread::park();
        }
    }
}

#[derive(Debug)]
struct TmpDir {
    path: PathBuf,
}
impl TmpDir {
    pub fn new() -> Result<Self, Error> {
        let path = Path::new("/var/tmp/startos").join(base32::encode(
            base32::Alphabet::Rfc4648 { padding: false },
            &rand::random::<[u8; 8]>(),
        ));
        if path.exists() {
            return Err(Error::new(
                eyre!("{path:?} already exists"),
                ErrorKind::Filesystem,
            ));
        }
        std::fs::create_dir_all(&path)?;
        Ok(Self { path })
    }

    pub fn delete(self) -> Result<(), Error> {
        std::fs::remove_dir_all(&self.path)?;
        Ok(())
    }

    pub fn gc(self: Arc<Self>) -> Result<(), Error> {
        if let Ok(dir) = Arc::try_unwrap(self) {
            dir.delete()
        } else {
            Ok(())
        }
    }
}
impl std::ops::Deref for TmpDir {
    type Target = Path;
    fn deref(&self) -> &Self::Target {
        &self.path
    }
}
impl AsRef<Path> for TmpDir {
    fn as_ref(&self) -> &Path {
        &*self
    }
}
impl Drop for TmpDir {
    fn drop(&mut self) {
        if self.path.exists() {
            std::fs::remove_dir_all(&self.path).log_err();
        }
    }
}
