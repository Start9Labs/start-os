use std::{path::Path, process::Stdio};

use embassy::disk::main::DEFAULT_PASSWORD;

pub fn create_backup(
    mountpoint: impl AsRef<Path>,
    data_path: impl AsRef<Path>,
) -> Result<(), anyhow::Error> {
    let mountpoint = std::fs::canonicalize(mountpoint)?;
    let data_path = std::fs::canonicalize(data_path)?;

    let ignore_path = data_path.join(".backupignore");
    let exclude = if ignore_path.is_file() {
        std::fs::read_to_string(ignore_path)?
    } else {
        String::new()
    };

    let mut data_cmd = std::process::Command::new("duplicity");
    for exclude in exclude.lines().map(|s| s.trim()).filter(|s| !s.is_empty()) {
        if exclude.to_string().starts_with('!') {
            data_cmd.arg(format!(
                "--include={}",
                data_path
                    .join(exclude.to_string().trim_start_matches('!'))
                    .display()
            )).arg("--allow-source-mismatch");
        } else {
            data_cmd.arg(format!(
                "--exclude={}",
                data_path.join(exclude.to_string()).display()
            )).arg("--allow-source-mismatch");
        }
    }
    let data_output = data_cmd
        .env("PASSPHRASE", DEFAULT_PASSWORD)
        .arg(data_path)
        .arg(format!("file://{}", mountpoint.display().to_string()))
        .arg("--allow-source-mismatch")
        .stderr(Stdio::piped())
        .output()?;
    if !data_output.status.success() {
        return Err(anyhow::anyhow!(
            "duplicity error: {}",
            String::from_utf8(data_output.stderr).unwrap()
        ));
    }

    Ok(())
}

pub fn restore_backup(
    mountpoint: impl AsRef<Path>,
    data_path: impl AsRef<Path>,
) -> Result<(), anyhow::Error> {
    let mountpoint = std::fs::canonicalize(mountpoint)?;
    let data_path = std::fs::canonicalize(data_path)?;

    let data_output = std::process::Command::new("duplicity")
        .arg("--allow-source-mismatch")
        .env("PASSPHRASE", DEFAULT_PASSWORD)
        .arg("--force")
        .arg(format!("file://{}", mountpoint.display().to_string()))
        .arg(&data_path)
        .stderr(Stdio::piped())
        .output()?;
    if !data_output.status.success() {
        return Err(anyhow::anyhow!(
            "duplicity error: {}",
            String::from_utf8(data_output.stderr).unwrap()
        ));
    }

    Ok(())
}
