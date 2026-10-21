use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use axum::body::Body;
use axum::extract::Request;
use axum::response::Response;
use http::header::{CACHE_CONTROL, CONTENT_TYPE};

use crate::PackageId;
use crate::middleware::auth::signature::{AUTH_SIG_HEADER, verify_request_signature};
use crate::net::static_server::{bad_request, not_found, server_error};
use crate::prelude::*;
use crate::registry::context::RegistryContext;
use crate::registry::package::index::Reader;
use crate::rpc_continuations::Guid;
use crate::s9pk::S9pk;
use crate::s9pk::merkle_archive::source::FileSource;
use crate::util::mime::mime;
use crate::util::{DataUrl, VersionString};

/// Serves the icon of a package version, or of one of its dependencies as that version packs it.
/// A private package's icons require a signed request from a signer who may see it.
pub async fn icon(
    ctx: &RegistryContext,
    id: &str,
    version: &str,
    dependency: Option<&str>,
    mut request: Request,
) -> Response {
    let (Ok(id), Ok(version), Ok(dependency)) = (
        id.parse::<PackageId>(),
        version.parse::<VersionString>(),
        dependency.map(str::parse::<PackageId>).transpose(),
    ) else {
        return bad_request();
    };
    let signer = if request.headers().contains_key(AUTH_SIG_HEADER) {
        verify_request_signature(ctx, &mut request).await.ok()
    } else {
        None
    };
    let private = async {
        let peek = ctx.db.peek().await;
        let reader = Reader::resolve(&peek, signer.as_ref())?;
        let Some(package) = peek
            .as_index()
            .as_package()
            .visible_package(&id, reader.as_ref())?
        else {
            return Ok(None);
        };
        if package.as_versions().as_idx(&version).is_none() {
            return Ok(None);
        }
        package.as_private().de().map(Some)
    }
    .await;
    let icon = match private {
        Ok(Some(private)) => ctx
            .icons
            .read(&id, &version, dependency.as_ref())
            .await
            .map(|icon| icon.map(|icon| (icon, private))),
        Ok(None) => Ok(None),
        Err(e) => Err(e),
    };
    match icon {
        Ok(Some((icon, private))) => Response::builder()
            .header(CONTENT_TYPE, &*icon.mime)
            .header(
                CACHE_CONTROL,
                if private {
                    "private, max-age=3600"
                } else {
                    "public, max-age=3600"
                },
            )
            .body(Body::from(icon.data.into_owned()))
            .unwrap(),
        Ok(None) => not_found(),
        Err(e) => server_error(e),
    }
}

/// A package version's icon and its dependencies' icons as its s9pk packs them.
#[derive(Debug, Clone, PartialEq)]
pub struct PackageIcons {
    pub icon: DataUrl<'static>,
    pub dependencies: BTreeMap<PackageId, DataUrl<'static>>,
}

impl PackageIcons {
    pub async fn from_s9pk<S: FileSource + Clone>(s9pk: &S9pk<S>) -> Result<Self, Error> {
        let mut dependencies = BTreeMap::new();
        for id in s9pk.as_manifest().dependencies.0.keys() {
            if let Some(icon) = s9pk.dependency_icon_data_url(id).await? {
                dependencies.insert(id.clone(), icon);
            }
        }
        Ok(Self {
            icon: s9pk.icon_data_url().await?,
            dependencies,
        })
    }
}

/// Package icons under the registry's data directory, one directory per package version.
#[derive(Clone)]
pub struct IconStore(PathBuf);

const ICON_STEM: &str = "icon";
const DEPENDENCIES_DIR: &str = "dependencies";

impl IconStore {
    pub fn new(datadir: &Path) -> Self {
        Self(datadir.join("icons"))
    }

    fn package_dir(&self, id: &PackageId) -> PathBuf {
        self.0.join(&**id)
    }

    fn version_dir(&self, id: &PackageId, version: &VersionString) -> PathBuf {
        self.package_dir(id).join(AsRef::<str>::as_ref(version))
    }

    pub async fn read(
        &self,
        id: &PackageId,
        version: &VersionString,
        dependency: Option<&PackageId>,
    ) -> Result<Option<DataUrl<'static>>, Error> {
        let dir = self.version_dir(id, version);
        match dependency {
            None => read_icon(&dir, ICON_STEM).await,
            Some(dep) => read_icon(&dir.join(DEPENDENCIES_DIR), dep).await,
        }
    }

    /// None when no icon is stored for the version.
    pub async fn read_all(
        &self,
        id: &PackageId,
        version: &VersionString,
    ) -> Result<Option<PackageIcons>, Error> {
        let dir = self.version_dir(id, version);
        let Some(icon) = read_icon(&dir, ICON_STEM).await? else {
            return Ok(None);
        };
        let mut dependencies = BTreeMap::new();
        let deps = dir.join(DEPENDENCIES_DIR);
        if tokio::fs::try_exists(&deps).await? {
            let mut entries = tokio::fs::read_dir(&deps).await?;
            while let Some(entry) = entries.next_entry().await? {
                let path = entry.path();
                let Some(dep) = path
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .and_then(|s| s.parse::<PackageId>().ok())
                else {
                    continue;
                };
                dependencies.insert(dep, data_url(&path).await?);
            }
        }
        Ok(Some(PackageIcons { icon, dependencies }))
    }

    /// Fails with `icon-mismatch` when the version already stores other icons.
    pub async fn ensure_matches(
        &self,
        id: &PackageId,
        version: &VersionString,
        icons: &PackageIcons,
    ) -> Result<(), Error> {
        match self.read_all(id, version).await? {
            Some(stored) => check_icons_match(&stored, icons),
            None => Ok(()),
        }
    }

    /// Publishes a complete version without replacing stored icons.
    pub async fn write(
        &self,
        id: &PackageId,
        version: &VersionString,
        icons: &PackageIcons,
    ) -> Result<(), Error> {
        if let Some(stored) = self.read_all(id, version).await? {
            return check_icons_match(&stored, icons);
        }
        let staging = self
            .package_dir(id)
            .join(format!(".icons-{}", Guid::new().as_ref()));
        let published = async {
            let deps = staging.join(DEPENDENCIES_DIR);
            tokio::fs::create_dir_all(&deps).await?;
            write_icon(&staging, ICON_STEM, &icons.icon).await?;
            for (dep, icon) in &icons.dependencies {
                write_icon(&deps, dep, icon).await?;
            }
            tokio::fs::rename(&staging, self.version_dir(id, version)).await?;
            Ok(())
        }
        .await;
        if published.is_err() {
            remove_dir(&staging).await?;
        }
        published
    }

    /// Removes one version's icons, or the whole package's.
    pub async fn remove(
        &self,
        id: &PackageId,
        version: Option<&VersionString>,
    ) -> Result<(), Error> {
        match version {
            Some(version) => remove_dir(&self.version_dir(id, version)).await,
            None => remove_dir(&self.package_dir(id)).await,
        }
    }

    /// Writes the icons a registry database still holds inline.
    pub async fn import_inline(&self, db: &Value) -> Result<(), Error> {
        for (id, icons) in inline_icons(db)? {
            for (version, icons) in icons {
                self.write(&id, &version, &icons).await?;
            }
        }
        Ok(())
    }
}

/// The icons held inline in a registry database, by package and version.
pub(crate) fn inline_icons(
    db: &Value,
) -> Result<BTreeMap<PackageId, BTreeMap<VersionString, PackageIcons>>, Error> {
    let mut res = BTreeMap::new();
    let Some(packages) = db["index"]["package"]["packages"].as_object() else {
        return Ok(res);
    };
    for (id, package) in packages {
        let id: PackageId = id.parse()?;
        for (version, info) in package["versions"].as_object().into_iter().flatten() {
            let Some(icon) = info["icon"].as_str() else {
                continue;
            };
            let mut dependencies = BTreeMap::new();
            for (dep, meta) in info["dependencyMetadata"].as_object().into_iter().flatten() {
                if let Some(icon) = meta["icon"].as_str() {
                    dependencies.insert(dep.parse()?, icon.parse()?);
                }
            }
            res.entry(id.clone()).or_insert_with(BTreeMap::new).insert(
                version.parse()?,
                PackageIcons {
                    icon: icon.parse()?,
                    dependencies,
                },
            );
        }
    }
    Ok(res)
}

fn check_icons_match(stored: &PackageIcons, incoming: &PackageIcons) -> Result<(), Error> {
    ensure_code!(
        stored == incoming,
        ErrorKind::InvalidRequest,
        "{}",
        t!("registry.package.index.icon-mismatch")
    );
    Ok(())
}

async fn remove_dir(dir: &Path) -> Result<(), Error> {
    match tokio::fs::remove_dir_all(dir).await {
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
        res => Ok(res?),
    }
}

async fn write_icon(dir: &Path, stem: &str, icon: &DataUrl<'_>) -> Result<(), Error> {
    let ext = icon.canonical_ext().unwrap_or("bin");
    tokio::fs::write(dir.join(format!("{stem}.{ext}")), &icon.data).await?;
    Ok(())
}

async fn read_icon(dir: &Path, stem: &str) -> Result<Option<DataUrl<'static>>, Error> {
    let mut entries = match tokio::fs::read_dir(dir).await {
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        res => res?,
    };
    while let Some(entry) = entries.next_entry().await? {
        let path = entry.path();
        if path.is_file() && path.file_stem().and_then(|s| s.to_str()) == Some(stem) {
            return Ok(Some(data_url(&path).await?));
        }
    }
    Ok(None)
}

async fn data_url(path: &Path) -> Result<DataUrl<'static>, Error> {
    let mime = path
        .extension()
        .and_then(|e| e.to_str())
        .and_then(mime)
        .unwrap_or(DataUrl::DEFAULT_MIME);
    Ok(DataUrl::from_vec(mime, tokio::fs::read(path).await?))
}

#[cfg(test)]
mod tests {
    use imbl_value::json;

    use super::*;

    fn icons(data: &[u8]) -> PackageIcons {
        PackageIcons {
            icon: DataUrl::from_vec("image/png", data.to_vec()),
            dependencies: [(
                "dep".parse().unwrap(),
                DataUrl::from_vec("image/svg+xml", b"<svg/>".to_vec()),
            )]
            .into_iter()
            .collect(),
        }
    }

    #[tokio::test]
    async fn icon_store_round_trips_and_removes() {
        let dir = tempfile::tempdir().unwrap();
        let store = IconStore::new(dir.path());
        let id: PackageId = "pkg".parse().unwrap();
        let version: VersionString = "1.0.0:0".parse().unwrap();
        let dep: PackageId = "dep".parse().unwrap();
        assert!(store.read_all(&id, &version).await.unwrap().is_none());
        store
            .ensure_matches(&id, &version, &icons(b"a"))
            .await
            .unwrap();

        store.write(&id, &version, &icons(b"a")).await.unwrap();
        assert_eq!(
            store.read_all(&id, &version).await.unwrap(),
            Some(icons(b"a"))
        );
        assert_eq!(
            store
                .read(&id, &version, Some(&dep))
                .await
                .unwrap()
                .unwrap()
                .mime,
            "image/svg+xml"
        );
        assert!(
            store
                .read(&id, &version, Some(&id))
                .await
                .unwrap()
                .is_none()
        );
        store
            .ensure_matches(&id, &version, &icons(b"a"))
            .await
            .unwrap();
        store
            .ensure_matches(&id, &version, &icons(b"b"))
            .await
            .unwrap_err();

        store.write(&id, &version, &icons(b"a")).await.unwrap();
        store.write(&id, &version, &icons(b"b")).await.unwrap_err();
        assert_eq!(
            store.read_all(&id, &version).await.unwrap(),
            Some(icons(b"a"))
        );

        store.remove(&id, Some(&version)).await.unwrap();
        assert!(store.read(&id, &version, None).await.unwrap().is_none());
        store.write(&id, &version, &icons(b"a")).await.unwrap();
        store.remove(&id, None).await.unwrap();
        assert!(!dir.path().join("icons/pkg").exists());
    }

    #[tokio::test]
    async fn concurrent_icon_publication_keeps_one_complete_version() {
        let dir = tempfile::tempdir().unwrap();
        let store = IconStore::new(dir.path());
        let id = "pkg".parse().unwrap();
        let version = "1.0.0:0".parse().unwrap();
        let first = icons(b"a");
        let second = icons(b"b");
        let (a, b) = tokio::join!(
            store.write(&id, &version, &first),
            store.write(&id, &version, &second),
        );
        assert_ne!(a.is_ok(), b.is_ok());
        assert_eq!(
            store.read_all(&id, &version).await.unwrap(),
            Some(if a.is_ok() { first } else { second })
        );
    }

    #[tokio::test]
    async fn inline_icons_move_to_the_store() {
        let dir = tempfile::tempdir().unwrap();
        let store = IconStore::new(dir.path());
        let expected = icons(b"a");
        let db = json!({
            "index": { "package": { "packages": {
                "pkg": { "versions": {
                    "1.0.0:0": {
                        "icon": expected.icon.to_string(),
                        "dependencyMetadata": {
                            "dep": { "icon": expected.dependencies.values().next().unwrap().to_string() },
                            "other": { "icon": null },
                        },
                    },
                    "2.0.0:0": { "dependencyMetadata": {} },
                } },
            } } },
        });
        store.import_inline(&db).await.unwrap();
        let id = "pkg".parse().unwrap();
        assert_eq!(
            store
                .read_all(&id, &"1.0.0:0".parse().unwrap())
                .await
                .unwrap(),
            Some(expected)
        );
        assert!(
            store
                .read_all(&id, &"2.0.0:0".parse().unwrap())
                .await
                .unwrap()
                .is_none()
        );
    }
}
