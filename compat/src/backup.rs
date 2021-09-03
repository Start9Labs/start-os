use std::path::Path;

pub fn create_backup<P: AsRef<Path>>(
    mountpoint: P,
    data_path: P,
    app_id: &str,
) -> Result<(), anyhow::Error> {
    let path = std::fs::canonicalize(mountpoint)?;
    let volume_path = Path::new(embassy::VOLUMES).join(app_id);

    let exclude = if volume_path.is_dir() {
        let ignore_path = volume_path.join(".backupignore");
        if ignore_path.is_file() {
            std::fs::read(ignore_path)?
        } else {
            Vec::new()
        }
    } else {
        return Err(anyhow::anyhow!("Volume For {} Does Not Exist", app_id))
    };

    let mut data_cmd = std::process::Command::new("duplicity");
    for exclude in exclude {
        if exclude.to_string().starts_with('!') {
            data_cmd.arg(format!(
                "--include={}",
                volume_path.join(exclude.to_string().trim_start_matches('!')).display()
            ));
        } else {
            data_cmd.arg(format!("--exclude={}", volume_path.join(exclude.to_string()).display()));
        }
    }
    let data_res = data_cmd
        .arg(volume_path)
        .arg(format!("file://{}", data_path.as_ref().display().to_string()))
        .output();
    data_res?;

    Ok(())
}

pub fn restore_backup<P: AsRef<Path>>(
    path: P,
    data_path: P,
    app_id: &str,
) -> Result<(), anyhow::Error> {
    let path = std::fs::canonicalize(path)?;
    if !path.is_dir() {
        anyhow::anyhow!("Backup Path Must Be Directory");
    }
    let metadata_path = path.join("metadata.yaml");
    let volume_path = Path::new(embassy::VOLUMES).join(app_id);

    let mut data_cmd = std::process::Command::new("duplicity");
    data_cmd
        .arg("--force")
        .arg(format!("file://{:#?}", data_path.as_ref().display().to_string()))
        .arg(&volume_path);

    let data_output = data_cmd.status()?;
    if !data_output.success() {
        return Err(anyhow::anyhow!("duplicity error for {}", app_id))
    }

    std::fs::copy(
        metadata_path,
        Path::new(embassy::VOLUMES)
            .join(app_id)
            .join("start9")
            .join("restore.yaml"),
    )?;

    Ok(())
}