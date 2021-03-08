use std::cmp::Ordering;

use async_trait::async_trait;
use failure::ResultExt as _;
use futures::stream::TryStreamExt;
use tokio_compat_02::FutureExt;

use crate::util::{to_yaml_async_writer, AsyncCompat, PersistencePath};
use crate::Error;
use crate::ResultExt as _;

mod v0_1_0;
mod v0_1_1;
mod v0_1_2;
mod v0_1_3;
mod v0_1_4;
mod v0_1_5;
mod v0_2_0;
mod v0_2_1;
mod v0_2_10;
mod v0_2_2;
mod v0_2_3;
mod v0_2_4;
mod v0_2_5;
mod v0_2_6;
mod v0_2_7;
mod v0_2_8;
mod v0_2_9;

pub use v0_2_10::Version as Current;

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
enum Version {
    V0_0_0(Wrapper<()>),
    V0_1_0(Wrapper<v0_1_0::Version>),
    V0_1_1(Wrapper<v0_1_1::Version>),
    V0_1_2(Wrapper<v0_1_2::Version>),
    V0_1_3(Wrapper<v0_1_3::Version>),
    V0_1_4(Wrapper<v0_1_4::Version>),
    V0_1_5(Wrapper<v0_1_5::Version>),
    V0_2_0(Wrapper<v0_2_0::Version>),
    V0_2_1(Wrapper<v0_2_1::Version>),
    V0_2_2(Wrapper<v0_2_2::Version>),
    V0_2_3(Wrapper<v0_2_3::Version>),
    V0_2_4(Wrapper<v0_2_4::Version>),
    V0_2_5(Wrapper<v0_2_5::Version>),
    V0_2_6(Wrapper<v0_2_6::Version>),
    V0_2_7(Wrapper<v0_2_7::Version>),
    V0_2_8(Wrapper<v0_2_8::Version>),
    V0_2_9(Wrapper<v0_2_9::Version>),
    V0_2_10(Wrapper<v0_2_10::Version>),
    Other(emver::Version),
}

#[async_trait]
pub trait VersionT
where
    Self: Sized + Send + Sync,
{
    type Previous: VersionT;
    fn new() -> Self;
    fn semver(&self) -> &'static emver::Version;
    async fn up(&self) -> Result<(), Error>;
    async fn down(&self) -> Result<(), Error>;
    async fn commit(&self) -> Result<(), Error> {
        let mut out = PersistencePath::from_ref("version").write(None).await?;
        to_yaml_async_writer(out.as_mut(), &self.semver()).await?;
        out.commit().await?;
        Ok(())
    }
    async fn migrate_to<V: VersionT>(&self, version: &V) -> Result<(), Error> {
        match self.semver().cmp(version.semver()) {
            Ordering::Greater => self.rollback_to_unchecked(version).await,
            Ordering::Less => version.migrate_from_unchecked(self).await,
            Ordering::Equal => Ok(()),
        }
    }
    async fn migrate_from_unchecked<V: VersionT>(&self, version: &V) -> Result<(), Error> {
        let previous = Self::Previous::new();
        if version.semver() != previous.semver() {
            previous.migrate_from_unchecked(version).await?;
        }
        log::info!("{} -> {}", previous.semver(), self.semver());
        self.up().await?;
        self.commit().await?;
        Ok(())
    }
    async fn rollback_to_unchecked<V: VersionT>(&self, version: &V) -> Result<(), Error> {
        let previous = Self::Previous::new();
        log::info!("{} -> {}", self.semver(), previous.semver());
        self.down().await?;
        previous.commit().await?;
        if version.semver() != previous.semver() {
            previous.rollback_to_unchecked(version).await?;
        }
        Ok(())
    }
}
struct Wrapper<T>(T);
impl<T> serde::Serialize for Wrapper<T>
where
    T: VersionT,
{
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.0.semver().serialize(serializer)
    }
}
impl<'de, T> serde::Deserialize<'de> for Wrapper<T>
where
    T: VersionT,
{
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let v = emver::Version::deserialize(deserializer)?;
        let version = T::new();
        if &v == version.semver() {
            Ok(Wrapper(version))
        } else {
            Err(serde::de::Error::custom("Mismatched Version"))
        }
    }
}
const V0_0_0: emver::Version = emver::Version::new(0, 0, 0, 0);
#[async_trait]
impl VersionT for () {
    type Previous = ();
    fn new() -> Self {
        ()
    }
    fn semver(&self) -> &'static emver::Version {
        &V0_0_0
    }
    async fn up(&self) -> Result<(), Error> {
        Ok(())
    }
    async fn down(&self) -> Result<(), Error> {
        Ok(())
    }
}

pub async fn init() -> Result<(), failure::Error> {
    let _lock = PersistencePath::from_ref("").lock(true).await?;
    let vpath = PersistencePath::from_ref("version");
    if let Some(mut f) = vpath.maybe_read(false).await.transpose()? {
        let v: Version = crate::util::from_yaml_async_reader(&mut *f).await?;
        match v {
            Version::V0_0_0(v) => v.0.migrate_to(&Current::new()).await?,
            Version::V0_1_0(v) => v.0.migrate_to(&Current::new()).await?,
            Version::V0_1_1(v) => v.0.migrate_to(&Current::new()).await?,
            Version::V0_1_2(v) => v.0.migrate_to(&Current::new()).await?,
            Version::V0_1_3(v) => v.0.migrate_to(&Current::new()).await?,
            Version::V0_1_4(v) => v.0.migrate_to(&Current::new()).await?,
            Version::V0_1_5(v) => v.0.migrate_to(&Current::new()).await?,
            Version::V0_2_0(v) => v.0.migrate_to(&Current::new()).await?,
            Version::V0_2_1(v) => v.0.migrate_to(&Current::new()).await?,
            Version::V0_2_2(v) => v.0.migrate_to(&Current::new()).await?,
            Version::V0_2_3(v) => v.0.migrate_to(&Current::new()).await?,
            Version::V0_2_4(v) => v.0.migrate_to(&Current::new()).await?,
            Version::V0_2_5(v) => v.0.migrate_to(&Current::new()).await?,
            Version::V0_2_6(v) => v.0.migrate_to(&Current::new()).await?,
            Version::V0_2_7(v) => v.0.migrate_to(&Current::new()).await?,
            Version::V0_2_8(v) => v.0.migrate_to(&Current::new()).await?,
            Version::V0_2_9(v) => v.0.migrate_to(&Current::new()).await?,
            Version::V0_2_10(v) => v.0.migrate_to(&Current::new()).await?,
            Version::Other(_) => (),
            // TODO find some way to automate this?
        }
    } else {
        ().migrate_to(&Current::new()).await?;
    }
    Ok(())
}

pub async fn self_update(requirement: emver::VersionRange) -> Result<(), Error> {
    let req_str: String = format!("{}", requirement)
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();
    let url = format!("{}/appmgr?spec={}", &*crate::SYS_REGISTRY_URL, req_str);
    log::info!("Fetching new version from {}", url);
    let response = reqwest::get(&url)
        .compat()
        .await
        .with_code(crate::error::NETWORK_ERROR)?
        .error_for_status()
        .with_code(crate::error::REGISTRY_ERROR)?;
    let tmp_appmgr_path = PersistencePath::from_ref("appmgr").tmp();
    if let Some(parent) = tmp_appmgr_path.parent() {
        if !parent.exists() {
            tokio::fs::create_dir_all(parent)
                .await
                .with_code(crate::error::FILESYSTEM_ERROR)?;
        }
    }
    let mut f = tokio::fs::OpenOptions::new()
        .create(true)
        .write(true)
        .open(&tmp_appmgr_path)
        .await
        .with_context(|e| format!("{}: {}", tmp_appmgr_path.display(), e))
        .with_code(crate::error::FILESYSTEM_ERROR)?;
    tokio::io::copy(
        &mut AsyncCompat(
            response
                .bytes_stream()
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
                .into_async_read(),
        ),
        &mut f,
    )
    .await
    .no_code()?;
    drop(f);
    crate::ensure_code!(
        tokio::process::Command::new("chmod")
            .arg("700")
            .arg(&tmp_appmgr_path)
            .output()
            .await?
            .status
            .success(),
        crate::error::FILESYSTEM_ERROR,
        "chmod failed"
    );
    let out = std::process::Command::new(&tmp_appmgr_path)
        .arg("semver")
        .stdout(std::process::Stdio::piped())
        .spawn()?
        .wait_with_output()
        .with_context(|e| format!("{} semver: {}", tmp_appmgr_path.display(), e))
        .no_code()?;
    let out_str = std::str::from_utf8(&out.stdout).no_code()?;
    log::info!("Migrating to version {}", out_str);
    let v: Version = serde_yaml::from_str(out_str)
        .with_context(|e| format!("{}: {:?}", e, out_str))
        .with_code(crate::error::SERDE_ERROR)?;
    match v {
        Version::V0_0_0(v) => Current::new().migrate_to(&v.0).await?,
        Version::V0_1_0(v) => Current::new().migrate_to(&v.0).await?,
        Version::V0_1_1(v) => Current::new().migrate_to(&v.0).await?,
        Version::V0_1_2(v) => Current::new().migrate_to(&v.0).await?,
        Version::V0_1_3(v) => Current::new().migrate_to(&v.0).await?,
        Version::V0_1_4(v) => Current::new().migrate_to(&v.0).await?,
        Version::V0_1_5(v) => Current::new().migrate_to(&v.0).await?,
        Version::V0_2_0(v) => Current::new().migrate_to(&v.0).await?,
        Version::V0_2_1(v) => Current::new().migrate_to(&v.0).await?,
        Version::V0_2_2(v) => Current::new().migrate_to(&v.0).await?,
        Version::V0_2_3(v) => Current::new().migrate_to(&v.0).await?,
        Version::V0_2_4(v) => Current::new().migrate_to(&v.0).await?,
        Version::V0_2_5(v) => Current::new().migrate_to(&v.0).await?,
        Version::V0_2_6(v) => Current::new().migrate_to(&v.0).await?,
        Version::V0_2_7(v) => Current::new().migrate_to(&v.0).await?,
        Version::V0_2_8(v) => Current::new().migrate_to(&v.0).await?,
        Version::V0_2_9(v) => Current::new().migrate_to(&v.0).await?,
        Version::V0_2_10(v) => Current::new().migrate_to(&v.0).await?,
        Version::Other(_) => (),
        // TODO find some way to automate this?
    };
    let cur_path = std::path::Path::new("/usr/local/bin/appmgr");
    tokio::fs::rename(&tmp_appmgr_path, &cur_path)
        .await
        .with_context(|e| {
            format!(
                "{} -> {}: {}",
                tmp_appmgr_path.display(),
                cur_path.display(),
                e
            )
        })
        .with_code(crate::error::FILESYSTEM_ERROR)?;

    Ok(())
}
