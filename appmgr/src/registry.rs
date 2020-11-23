use emver::VersionRange;

use crate::apps::AppConfig;
use crate::manifest::ManifestLatest;
use crate::Error;
use crate::ResultExt as _;

pub async fn manifest(id: &str, version: &VersionRange) -> Result<ManifestLatest, Error> {
    let manifest: ManifestLatest = reqwest::get(&format!(
        "{}/manifest/{}?spec={}",
        &*crate::APP_REGISTRY_URL,
        id,
        version
    ))
    .await
    .with_code(crate::error::NETWORK_ERROR)?
    .error_for_status()
    .with_code(crate::error::REGISTRY_ERROR)?
    .json()
    .await
    .with_code(crate::error::SERDE_ERROR)?;
    Ok(manifest)
}

pub async fn version(id: &str, version: &VersionRange) -> Result<emver::Version, Error> {
    #[derive(serde::Deserialize)]
    struct VersionRes {
        version: emver::Version,
    }

    let version: VersionRes = reqwest::get(&format!(
        "{}/version/{}?spec={}",
        &*crate::APP_REGISTRY_URL,
        id,
        version
    ))
    .await
    .with_code(crate::error::NETWORK_ERROR)?
    .error_for_status()
    .with_code(crate::error::REGISTRY_ERROR)?
    .json()
    .await
    .with_code(crate::error::SERDE_ERROR)?;
    Ok(version.version)
}

pub async fn config(id: &str, version: &VersionRange) -> Result<AppConfig, Error> {
    let config: crate::inspect::AppConfig = reqwest::get(&format!(
        "{}/config/{}?spec={}",
        &*crate::APP_REGISTRY_URL,
        id,
        version
    ))
    .await
    .with_code(crate::error::NETWORK_ERROR)?
    .error_for_status()
    .with_code(crate::error::REGISTRY_ERROR)?
    .json()
    .await
    .with_code(crate::error::SERDE_ERROR)?;
    Ok(AppConfig {
        config: None,
        spec: config.spec,
        rules: config.rules,
    })
}
