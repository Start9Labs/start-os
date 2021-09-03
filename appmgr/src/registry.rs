use emver::VersionRange;
use tokio_compat_02::FutureExt;

use crate::s9pk::manifest::Manifest;
use crate::{Error, ResultExt as _};

pub async fn manifest(id: &str, version: &VersionRange) -> Result<Manifest, Error> {
    let manifest: Manifest = reqwest::get(&format!(
        "{}/manifest/{}?spec={}",
        &*crate::APP_REGISTRY_URL,
        id,
        version
    ))
    .compat()
    .await
    .with_kind(crate::ErrorKind::Network)?
    .error_for_status()
    .with_kind(crate::ErrorKind::Registry)?
    .json()
    .await
    .with_kind(crate::ErrorKind::Deserialization)?;
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
    .compat()
    .await
    .with_kind(crate::ErrorKind::Network)?
    .error_for_status()
    .with_kind(crate::ErrorKind::Registry)?
    .json()
    .await
    .with_kind(crate::ErrorKind::Deserialization)?;
    Ok(version.version)
}
