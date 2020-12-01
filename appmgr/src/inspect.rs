use std::path::Path;

use failure::ResultExt as _;
use futures::stream::StreamExt;
use tokio_compat_02::IoCompat;
use tokio_tar as tar;

use crate::config::{ConfigRuleEntry, ConfigSpec};
use crate::manifest::{Manifest, ManifestLatest};
use crate::util::from_cbor_async_reader;
use crate::version::VersionT;
use crate::Error;
use crate::ResultExt as _;

#[derive(Debug, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct AppInfoFull {
    #[serde(flatten)]
    pub info: AppInfo,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub manifest: Option<ManifestLatest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub config: Option<AppConfig>,
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct AppInfo {
    pub title: String,
    pub version: emver::Version,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct AppConfig {
    pub spec: ConfigSpec,
    pub rules: Vec<ConfigRuleEntry>,
}

pub async fn info_full<P: AsRef<Path>>(
    path: P,
    with_manifest: bool,
    with_config: bool,
) -> Result<AppInfoFull, Error> {
    let p = path.as_ref();
    log::info!("Opening file.");
    let r = tokio::fs::File::open(p)
        .await
        .with_context(|e| format!("{}: {}", p.display(), e))
        .with_code(crate::error::FILESYSTEM_ERROR)?;
    log::info!("Extracting archive.");
    let mut pkg = tar::Archive::new(IoCompat::new(r));
    let mut entries = pkg.entries()?;
    log::info!("Opening manifest from archive.");
    let manifest = entries
        .next()
        .await
        .ok_or(crate::install::Error::CorruptedPkgFile("missing manifest"))
        .no_code()??;
    crate::ensure_code!(
        manifest.path()?.to_str() == Some("manifest.cbor"),
        crate::error::GENERAL_ERROR,
        "Package File Invalid or Corrupted"
    );
    log::trace!("Deserializing manifest.");
    let manifest: Manifest = from_cbor_async_reader(IoCompat::new(manifest)).await?;
    let manifest = manifest.into_latest();
    crate::ensure_code!(
        crate::version::Current::new()
            .semver()
            .satisfies(&manifest.os_version_required),
        crate::error::VERSION_INCOMPATIBLE,
        "AppMgr Version Not Compatible: needs {}",
        manifest.os_version_required
    );
    Ok(AppInfoFull {
        info: AppInfo {
            title: manifest.title.clone(),
            version: manifest.version.clone(),
        },
        manifest: if with_manifest { Some(manifest) } else { None },
        config: if with_config {
            log::info!("Opening config spec from archive.");
            let spec = entries
                .next()
                .await
                .ok_or(crate::install::Error::CorruptedPkgFile(
                    "missing config spec",
                ))
                .no_code()??;
            crate::ensure_code!(
                spec.path()?.to_str() == Some("config_spec.cbor"),
                crate::error::GENERAL_ERROR,
                "Package File Invalid or Corrupted"
            );
            log::trace!("Deserializing config spec.");
            let spec = from_cbor_async_reader(IoCompat::new(spec)).await?;
            log::info!("Opening config rules from archive.");
            let rules = entries
                .next()
                .await
                .ok_or(crate::install::Error::CorruptedPkgFile(
                    "missing config rules",
                ))
                .no_code()??;
            crate::ensure_code!(
                rules.path()?.to_str() == Some("config_rules.cbor"),
                crate::error::GENERAL_ERROR,
                "Package File Invalid or Corrupted"
            );
            log::trace!("Deserializing config rules.");
            let rules = from_cbor_async_reader(IoCompat::new(rules)).await?;
            Some(AppConfig { spec, rules })
        } else {
            None
        },
    })
}

pub async fn print_instructions<P: AsRef<Path>>(path: P) -> Result<(), Error> {
    let p = path.as_ref();
    log::info!("Opening file.");
    let r = tokio::fs::File::open(p)
        .await
        .with_context(|e| format!("{}: {}", p.display(), e))
        .with_code(crate::error::FILESYSTEM_ERROR)?;
    log::info!("Extracting archive.");
    let mut pkg = tar::Archive::new(IoCompat::new(r));
    let mut entries = pkg.entries()?;
    log::info!("Opening manifest from archive.");
    let manifest = entries
        .next()
        .await
        .ok_or(crate::install::Error::CorruptedPkgFile("missing manifest"))
        .no_code()??;
    crate::ensure_code!(
        manifest.path()?.to_str() == Some("manifest.cbor"),
        crate::error::GENERAL_ERROR,
        "Package File Invalid or Corrupted"
    );
    log::trace!("Deserializing manifest.");
    let manifest: Manifest = from_cbor_async_reader(IoCompat::new(manifest)).await?;
    let manifest = manifest.into_latest();
    crate::ensure_code!(
        crate::version::Current::new()
            .semver()
            .satisfies(&manifest.os_version_required),
        crate::error::VERSION_INCOMPATIBLE,
        "AppMgr Version Not Compatible: needs {}",
        manifest.os_version_required
    );
    entries
        .next()
        .await
        .ok_or(crate::install::Error::CorruptedPkgFile(
            "missing config spec",
        ))
        .no_code()??;
    entries
        .next()
        .await
        .ok_or(crate::install::Error::CorruptedPkgFile(
            "missing config rules",
        ))
        .no_code()??;

    if manifest.has_instructions {
        use tokio::io::AsyncWriteExt;

        let instructions = entries
            .next()
            .await
            .ok_or(crate::install::Error::CorruptedPkgFile(
                "missing instructions",
            ))
            .no_code()??;

        let mut stdout = tokio::io::stdout();
        tokio::io::copy(&mut IoCompat::new(instructions), &mut stdout)
            .await
            .with_code(crate::error::FILESYSTEM_ERROR)?;
        stdout
            .flush()
            .await
            .with_code(crate::error::FILESYSTEM_ERROR)?;
        stdout
            .shutdown()
            .await
            .with_code(crate::error::FILESYSTEM_ERROR)?;
    } else {
        return Err(failure::format_err!("No instructions for {}", p.display()))
            .with_code(crate::error::NOT_FOUND);
    }

    Ok(())
}
