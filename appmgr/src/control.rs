use std::path::Path;

use futures::future::{BoxFuture, FutureExt};
use linear_map::{set::LinearSet, LinearMap};

use crate::dependencies::{DependencyError, TaggedDependencyError};
use crate::util::{from_yaml_async_reader, PersistencePath, YamlUpdateHandle};
use crate::Error;

pub async fn start_app(name: &str, update_metadata: bool) -> Result<(), Error> {
    let lock = crate::util::lock_file(
        format!(
            "{}",
            Path::new(crate::PERSISTENCE_DIR)
                .join("apps")
                .join(name)
                .join("control.lock")
                .display()
        ),
        true,
    )
    .await?;
    let status = crate::apps::status(name, false).await?.status;
    if status == crate::apps::DockerStatus::Stopped {
        if update_metadata {
            crate::config::configure(name, None, None, false).await?;
            crate::dependencies::update_shared(name).await?;
            crate::dependencies::update_binds(name).await?;
        }
        crate::apps::set_needs_restart(name, false).await?;
        let output = tokio::process::Command::new("docker")
            .args(&["start", name])
            .stdout(std::process::Stdio::null())
            .output()
            .await?;
        crate::ensure_code!(
            output.status.success(),
            crate::error::DOCKER_ERROR,
            "Failed to Start Application: {}",
            std::str::from_utf8(&output.stderr).unwrap_or("Unknown Error")
        );
        let mut running = YamlUpdateHandle::<LinearSet<String>>::new_or_default(
            PersistencePath::from_ref("running.yaml"),
        )
        .await?;
        running.insert(name.to_owned());
        running.commit().await?;
    } else if status == crate::apps::DockerStatus::Paused {
        resume_app(name).await?;
    }
    crate::util::unlock(lock).await?;
    Ok(())
}

pub async fn stop_app(
    name: &str,
    cascade: bool,
    dry_run: bool,
) -> Result<LinearMap<String, TaggedDependencyError>, Error> {
    let mut res = LinearMap::new();
    if cascade {
        stop_dependents(name, dry_run, DependencyError::NotRunning, &mut res).await?;
    }
    if !dry_run {
        let lock = crate::util::lock_file(
            format!(
                "{}",
                Path::new(crate::PERSISTENCE_DIR)
                    .join("apps")
                    .join(name)
                    .join("control.lock")
                    .display()
            ),
            true,
        )
        .await?;
        log::info!("Stopping {}", name);
        let output = tokio::process::Command::new("docker")
            .args(&["stop", "-t", "25", name])
            .stdout(std::process::Stdio::null())
            .output()
            .await?;
        crate::ensure_code!(
            output.status.success(),
            crate::error::DOCKER_ERROR,
            "Failed to Stop Application: {}",
            std::str::from_utf8(&output.stderr).unwrap_or("Unknown Error")
        );
        let mut running = YamlUpdateHandle::<LinearSet<String>>::new_or_default(
            PersistencePath::from_ref("running.yaml"),
        )
        .await?;
        running.remove(name);
        running.commit().await?;
        crate::util::unlock(lock).await?;
    }
    Ok(res)
}

pub async fn stop_dependents(
    name: &str,
    dry_run: bool,
    err: DependencyError,
    res: &mut LinearMap<String, TaggedDependencyError>,
) -> Result<(), Error> {
    fn stop_dependents_rec<'a>(
        name: &'a str,
        dry_run: bool,
        err: DependencyError,
        res: &'a mut LinearMap<String, TaggedDependencyError>,
    ) -> BoxFuture<'a, Result<(), Error>> {
        async move {
            for dependent in crate::apps::dependents(name, false).await? {
                if crate::apps::status(&dependent, false).await?.status
                    != crate::apps::DockerStatus::Stopped
                {
                    stop_dependents_rec(&dependent, dry_run, DependencyError::NotRunning, res)
                        .await?;
                    stop_app(&dependent, false, dry_run).await?;
                    res.insert(
                        dependent,
                        TaggedDependencyError {
                            dependency: name.to_owned(),
                            error: err.clone(),
                        },
                    );
                }
            }
            Ok(())
        }
        .boxed()
    }
    stop_dependents_rec(name, dry_run, err, res).await
}

pub async fn restart_app(name: &str) -> Result<(), Error> {
    stop_app(name, false, false).await?;
    if let Err(e) = start_app(name, true).await {
        log::warn!("Stopping dependents");
        stop_dependents(
            name,
            false,
            crate::dependencies::DependencyError::NotRunning,
            &mut linear_map::LinearMap::new(),
        )
        .await?;
        return Err(e);
    }
    Ok(())
}

pub async fn pause_app(name: &str) -> Result<(), Error> {
    let lock = crate::util::lock_file(
        format!(
            "{}",
            Path::new(crate::PERSISTENCE_DIR)
                .join("apps")
                .join(name)
                .join("control.lock")
                .display()
        ),
        true,
    )
    .await?;
    let output = tokio::process::Command::new("docker")
        .args(&["pause", name])
        .stdout(std::process::Stdio::null())
        .output()
        .await?;
    crate::ensure_code!(
        output.status.success(),
        crate::error::DOCKER_ERROR,
        "Failed to Pause Application: {}",
        std::str::from_utf8(&output.stderr).unwrap_or("Unknown Error")
    );

    crate::util::unlock(lock).await?;
    Ok(())
}

pub async fn resume_app(name: &str) -> Result<(), Error> {
    let lock = crate::util::lock_file(
        format!(
            "{}",
            Path::new(crate::PERSISTENCE_DIR)
                .join("apps")
                .join(name)
                .join("control.lock")
                .display()
        ),
        true,
    )
    .await?;
    let output = tokio::process::Command::new("docker")
        .args(&["unpause", name])
        .stdout(std::process::Stdio::null())
        .output()
        .await?;
    crate::ensure_code!(
        output.status.success(),
        crate::error::DOCKER_ERROR,
        "Failed to Resume Application: {}",
        std::str::from_utf8(&output.stderr).unwrap_or("Unknown Error")
    );
    crate::util::unlock(lock).await?;
    Ok(())
}

pub async fn repair_app_status() -> Result<(), Error> {
    let lock = crate::util::lock_file(
        format!(
            "{}",
            Path::new(crate::PERSISTENCE_DIR)
                .join("apps")
                .join(name)
                .join("control.lock")
                .display()
        ),
        true,
    )
    .await?;
    let running: Vec<String> = if let Some(mut f) = PersistencePath::from_ref("running.yaml")
        .maybe_read(false)
        .await
        .transpose()?
    {
        from_yaml_async_reader(&mut *f).await?
    } else {
        Vec::new()
    };
    for name in running {
        if crate::apps::status(&name, false).await?.status == crate::apps::DockerStatus::Stopped {
            start_app(&name, true).await?;
        }
    }
    crate::util::unlock(lock).await?;
    Ok(())
}
