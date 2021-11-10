use std::cmp::Ordering;

use async_trait::async_trait;
use color_eyre::eyre::eyre;
use patch_db::json_ptr::JsonPointer;
use patch_db::{DbHandle, LockType};
use rpc_toolkit::command;

use crate::{Error, ResultExt};

mod v0_3_0;

pub type Current = v0_3_0::Version;

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
enum Version {
    V0_3_0(Wrapper<v0_3_0::Version>),
    Other(emver::Version),
}

#[async_trait]
pub trait VersionT
where
    Self: Sized + Send + Sync,
{
    type Previous: VersionT;
    fn new() -> Self;
    fn semver(&self) -> emver::Version;
    fn compat(&self) -> &'static emver::VersionRange;
    async fn up<Db: DbHandle>(&self, db: &mut Db) -> Result<(), Error>;
    async fn down<Db: DbHandle>(&self, db: &mut Db) -> Result<(), Error>;
    async fn commit<Db: DbHandle>(&self, db: &mut Db) -> Result<(), Error> {
        crate::db::DatabaseModel::new()
            .server_info()
            .version()
            .put(db, &self.semver().into())
            .await?;

        Ok(())
    }
    async fn migrate_to<V: VersionT, Db: DbHandle>(
        &self,
        version: &V,
        db: &mut Db,
    ) -> Result<(), Error> {
        match self.semver().cmp(&version.semver()) {
            Ordering::Greater => self.rollback_to_unchecked(version, db).await,
            Ordering::Less => version.migrate_from_unchecked(self, db).await,
            Ordering::Equal => Ok(()),
        }
    }
    async fn migrate_from_unchecked<V: VersionT, Db: DbHandle>(
        &self,
        version: &V,
        db: &mut Db,
    ) -> Result<(), Error> {
        let previous = Self::Previous::new();
        if version.semver() != previous.semver() {
            previous.migrate_from_unchecked(version, db).await?;
        }
        tracing::info!("{} -> {}", previous.semver(), self.semver(),);
        self.up(db).await?;
        self.commit(db).await?;
        Ok(())
    }
    async fn rollback_to_unchecked<V: VersionT, Db: DbHandle>(
        &self,
        version: &V,
        db: &mut Db,
    ) -> Result<(), Error> {
        let previous = Self::Previous::new();
        tracing::info!("{} -> {}", self.semver(), previous.semver(),);
        self.down(db).await?;
        previous.commit(db).await?;
        if version.semver() != previous.semver() {
            previous.rollback_to_unchecked(version, db).await?;
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
        let v = crate::util::Version::deserialize(deserializer)?;
        let version = T::new();
        if &*v == &version.semver() {
            Ok(Wrapper(version))
        } else {
            Err(serde::de::Error::custom("Mismatched Version"))
        }
    }
}

pub async fn init<Db: DbHandle>(db: &mut Db) -> Result<(), Error> {
    let ptr: JsonPointer = "/server-info/version"
        .parse()
        .with_kind(crate::ErrorKind::Database)?;
    db.lock(ptr.clone(), LockType::Write).await;
    let version: Version = db.get(&ptr).await?;
    match version {
        Version::V0_3_0(v) => v.0.migrate_to(&Current::new(), db).await?,
        Version::Other(_) => {
            return Err(Error::new(
                eyre!("Cannot downgrade"),
                crate::ErrorKind::InvalidRequest,
            ))
        }
    }
    Ok(())
}

pub const COMMIT_HASH: &'static str =
    git_version::git_version!(args = ["--always", "--abbrev=40", "--dirty=-modified"]);

#[command(rename = "git-info", local, metadata(authenticated = false))]
pub fn git_info() -> Result<&'static str, Error> {
    Ok(COMMIT_HASH)
}
