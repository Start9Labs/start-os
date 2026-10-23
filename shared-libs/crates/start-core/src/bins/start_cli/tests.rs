use std::ffi::OsString;
use std::os::unix::process::ExitStatusExt;
use std::path::Path;
use std::process::Command;

const CHILD_ARGS: &str = "START_CLI_SIGPIPE_ARGS";
const BROKEN_STREAM: &str = "START_CLI_BROKEN_STREAM";

fn child(directory: &Path, args: &[&str]) -> Command {
    let mut command = Command::new(std::env::current_exe().unwrap());
    command
        .args([
            "--exact",
            "bins::start_cli::tests::run_cli_child",
            "--nocapture",
        ])
        .current_dir(directory)
        .env("HOME", directory)
        .env(CHILD_ARGS, serde_json::to_string(args).unwrap());
    command
}

#[test]
fn run_cli_child() {
    let Ok(args) = std::env::var(CHILD_ARGS) else {
        return;
    };
    let args: Vec<String> = serde_json::from_str(&args).unwrap();
    tokio_rustls::rustls::crypto::ring::default_provider()
        .install_default()
        .ok();
    if let Ok(stream) = std::env::var(BROKEN_STREAM) {
        let (reader, writer) = nix::unistd::pipe().unwrap();
        drop(reader);
        match stream.as_str() {
            "stdout" => nix::unistd::dup2_stdout(&writer).unwrap(),
            "stderr" => nix::unistd::dup2_stderr(&writer).unwrap(),
            _ => panic!("unknown stream"),
        }
    }
    super::main(
        std::iter::once(OsString::from("start-cli")).chain(args.into_iter().map(OsString::from)),
    );
    std::process::exit(0);
}

#[test]
fn stdout_uses_sigpipe_for_all_display_paths() {
    let directory = tempfile::tempdir().unwrap();
    for args in [
        vec!["net", "dns", "query", "localhost"],
        vec!["net", "dns", "query", "localhost", "--format", "json"],
        vec![
            "net",
            "dns",
            "query",
            "localhost",
            "--format",
            "json-pretty",
        ],
        vec!["net", "dns", "query", "localhost", "--format", "yaml"],
        vec!["net", "dns", "query", "localhost", "--format", "cbor"],
        vec!["net", "dns", "query", "localhost", "--format", "toml"],
        vec![
            "net",
            "dns",
            "query",
            "localhost",
            "--format",
            "toml-pretty",
        ],
        vec!["git-info"],
        vec!["completions", "bash"],
        vec!["--help"],
    ] {
        let normal = child(directory.path(), &args).output().unwrap();
        assert!(normal.status.success(), "{args:?}: {normal:?}");
        assert!(!normal.stdout.is_empty(), "{args:?}");
        assert!(normal.stderr.is_empty(), "{args:?}: {normal:?}");

        let broken = child(directory.path(), &args)
            .env(BROKEN_STREAM, "stdout")
            .output()
            .unwrap();
        assert_eq!(
            broken.status.signal(),
            Some(libc::SIGPIPE),
            "{args:?}: {broken:?}"
        );
        assert!(broken.stderr.is_empty(), "{args:?}: {broken:?}");
    }
}

#[test]
fn stderr_uses_sigpipe_and_normal_errors_keep_their_exit_code() {
    let directory = tempfile::tempdir().unwrap();
    std::fs::write(directory.path().join("invalid.json"), b"{").unwrap();
    let args = ["-c", "invalid.json", "git-info"];
    let normal = child(directory.path(), &args).output().unwrap();
    assert_eq!(
        normal.status.code(),
        Some(crate::ErrorKind::Deserialization as i32)
    );
    assert!(!normal.stderr.is_empty(), "{normal:?}");

    let broken = child(directory.path(), &args)
        .env(BROKEN_STREAM, "stderr")
        .output()
        .unwrap();
    assert_eq!(broken.status.signal(), Some(libc::SIGPIPE), "{broken:?}");
}
