use linear_map::LinearMap;

use crate::dependencies::{DependencyError, TaggedDependencyError};
use crate::Error;
use crate::ResultExt as _;

pub async fn update(
    name_version: &str,
    dry_run: bool,
) -> Result<LinearMap<String, TaggedDependencyError>, Error> {
    let mut name_version_iter = name_version.split("@");
    let name = name_version_iter.next().unwrap();
    let version_req = name_version_iter
        .next()
        .map(|v| v.parse())
        .transpose()
        .no_code()?
        .unwrap_or_else(emver::VersionRange::any);
    let version = crate::registry::version(name, &version_req).await?;
    let mut res = LinearMap::new();
    for dependent in crate::apps::dependents(name, false).await? {
        if crate::apps::status(&dependent, false).await?.status
            != crate::apps::DockerStatus::Stopped
        {
            let manifest = crate::apps::manifest(&dependent).await?;
            match manifest.dependencies.0.get(name) {
                Some(dep) if !version.satisfies(&dep.version) => {
                    crate::control::stop_dependents(
                        &dependent,
                        dry_run,
                        DependencyError::NotRunning,
                        &mut res,
                    )
                    .await?;
                    if crate::apps::status(name, false).await?.status
                        != crate::apps::DockerStatus::Stopped
                    {
                        crate::control::stop_app(&dependent, false, dry_run).await?;
                        res.insert(
                            dependent,
                            TaggedDependencyError {
                                dependency: name.to_owned(),
                                error: DependencyError::IncorrectVersion {
                                    expected: version_req.clone(),
                                    received: version.clone(),
                                },
                            },
                        );
                    }
                }
                _ => {
                    crate::control::stop_dependents(
                        &dependent,
                        dry_run,
                        DependencyError::NotRunning,
                        &mut res,
                    )
                    .await?;
                    if crate::apps::status(name, false).await?.status
                        != crate::apps::DockerStatus::Stopped
                    {
                        crate::control::stop_app(&dependent, false, dry_run).await?;
                        res.insert(
                            dependent,
                            TaggedDependencyError {
                                dependency: name.to_owned(),
                                error: DependencyError::NotRunning,
                            },
                        );
                    }
                }
            }
        }
    }
    if dry_run {
        return Ok(res);
    }
    let download_path = crate::install::download_name(name_version).await?;
    crate::remove::remove(name, false, false).await?;
    crate::install::install_path(download_path, Some(name)).await?;
    crate::apps::set_recoverable(name, false).await?;

    Ok(res)
}
