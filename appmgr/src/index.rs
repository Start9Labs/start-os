use std::cmp::Ord;
use std::ffi::OsStr;
use std::iter::FromIterator;
use std::path::Path;

use emver::{Version, VersionRange};
use futures::future::{BoxFuture, FutureExt};
use linear_map::LinearMap;

use crate::inspect::info_full;
use crate::manifest::{Description, ManifestLatest};
use crate::{Error, ResultExt};

#[derive(Clone, Debug, Default, serde::Deserialize, serde::Serialize)]
pub struct AppIndex(pub LinearMap<String, IndexInfo>);

#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct IndexInfo {
    pub title: String,
    pub description: Description,
    pub version_info: Vec<VersionInfo>,
    pub icon_type: String,
}

#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct VersionInfo {
    pub version: Version,
    pub release_notes: String,
    pub os_version_required: VersionRange,
    pub os_version_recommended: VersionRange,
    pub install_alert: Option<String>,
}

const NULL_VERSION: Version = Version::new(0, 0, 0, 0);

impl AppIndex {
    fn add(&mut self, manifest: ManifestLatest) {
        if let Some(ref mut entry) = self.0.get_mut(&manifest.id) {
            if entry
                .version_info
                .get(0)
                .map(|i| &i.version)
                .unwrap_or(&NULL_VERSION)
                <= &manifest.version
            {
                entry.title = manifest.title;
                entry.description = manifest.description;
            }
            entry.version_info.push(VersionInfo {
                version: manifest.version,
                release_notes: manifest.release_notes,
                os_version_required: manifest.os_version_required,
                os_version_recommended: manifest.os_version_recommended,
                install_alert: manifest.install_alert,
            });
            entry
                .version_info
                .sort_unstable_by(|a, b| b.version.cmp(&a.version));
            entry.version_info.dedup_by(|a, b| a.version == b.version);
        } else {
            self.0.insert(
                manifest.id,
                IndexInfo {
                    title: manifest.title,
                    description: manifest.description,
                    version_info: vec![VersionInfo {
                        version: manifest.version,
                        release_notes: manifest.release_notes,
                        os_version_required: manifest.os_version_required,
                        os_version_recommended: manifest.os_version_recommended,
                        install_alert: manifest.install_alert,
                    }],
                    icon_type: "png".to_owned(), // TODO
                },
            );
        }
    }
}

impl Extend<ManifestLatest> for AppIndex {
    fn extend<I: IntoIterator<Item = ManifestLatest>>(&mut self, iter: I) {
        for manifest in iter {
            self.add(manifest);
        }
    }
}

impl FromIterator<ManifestLatest> for AppIndex {
    fn from_iter<I: IntoIterator<Item = ManifestLatest>>(iter: I) -> Self {
        let mut res = Self::default();
        res.extend(iter);
        res
    }
}

pub async fn index<P: AsRef<Path>>(dir: P) -> Result<AppIndex, Error> {
    let dir_path = dir.as_ref();
    let mut idx = AppIndex::default();
    fn index_rec<'a, P: AsRef<Path> + Send + Sync + 'a>(
        idx: &'a mut AppIndex,
        dir: P,
    ) -> BoxFuture<'a, Result<(), Error>> {
        async move {
            let dir_path = dir.as_ref();
            if let Ok(_) = tokio::fs::metadata(dir_path.join(".ignore")).await {
                log::info!("Skipping {}", dir_path.display());
                return Ok(());
            }
            let mut entry_stream = tokio::fs::read_dir(dir_path).await?;
            while let Some(entry) = entry_stream.next_entry().await? {
                let path = entry.path();
                let metadata = entry.metadata().await?;
                if metadata.is_file() {
                    let ext = path.extension();
                    if ext == Some(OsStr::new("s9pk")) {
                        let info = info_full(&path, true, false)
                            .await
                            .with_ctx(|e| (e.code.clone(), format!("{}: {}", path.display(), e)))?;
                        idx.add(info.manifest.unwrap());
                    }
                } else if metadata.is_dir() {
                    index_rec(idx, &path).await?;
                }
            }
            Ok(())
        }
        .boxed()
    }
    index_rec(&mut idx, dir_path).await?;
    Ok(idx)
}
