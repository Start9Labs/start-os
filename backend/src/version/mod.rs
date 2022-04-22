use std::cmp::Ordering;

use async_trait::async_trait;
use color_eyre::eyre::eyre;
use patch_db::{json_ptr::JsonPointer, LockReceipt};
use patch_db::{DbHandle, LockType};
use rpc_toolkit::command;

use crate::{db::receipts, init::InitReceipts, Error, ResultExt};

mod v0_3_0;
mod v0_3_0_1;
mod v0_3_0_2;

pub type Current = v0_3_0_2::Version;

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
enum Version {
    V0_3_0(Wrapper<v0_3_0::Version>),
    V0_3_0_1(Wrapper<v0_3_0_1::Version>),
    V0_3_0_2(Wrapper<v0_3_0_2::Version>),
    Other(emver::Version),
}

pub trait VersionCommitReceipt:
    receipts::VersionRangeReceipt + receipts::ServerVersionReceipt
{
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
    async fn commit<Db: DbHandle>(
        &self,
        db: &mut Db,
        receipts: &InitReceipts,
    ) -> Result<(), Error> {
        receipts
            .version_range
            .set(db, self.compat().clone())
            .await?;
        receipts
            .server_version
            .set(db, self.semver().into())
            .await?;

        Ok(())
    }
    async fn migrate_to<V: VersionT, Db: DbHandle>(
        &self,
        version: &V,
        db: &mut Db,
        receipts: &InitReceipts,
    ) -> Result<(), Error> {
        match self.semver().cmp(&version.semver()) {
            Ordering::Greater => self.rollback_to_unchecked(version, db, receipts).await,
            Ordering::Less => version.migrate_from_unchecked(self, db, receipts).await,
            Ordering::Equal => Ok(()),
        }
    }
    async fn migrate_from_unchecked<V: VersionT, Db: DbHandle>(
        &self,
        version: &V,
        db: &mut Db,
        receipts: &InitReceipts,
    ) -> Result<(), Error> {
        let previous = Self::Previous::new();
        if version.semver() != previous.semver() {
            previous
                .migrate_from_unchecked(version, db, receipts)
                .await?;
        }
        tracing::info!("{} -> {}", previous.semver(), self.semver(),);
        self.up(db).await?;
        self.commit(db, receipts).await?;
        Ok(())
    }
    async fn rollback_to_unchecked<V: VersionT, Db: DbHandle>(
        &self,
        version: &V,
        db: &mut Db,
        receipts: &InitReceipts,
    ) -> Result<(), Error> {
        let previous = Self::Previous::new();
        tracing::info!("{} -> {}", self.semver(), previous.semver(),);
        self.down(db).await?;
        previous.commit(db, receipts).await?;
        if version.semver() != previous.semver() {
            previous
                .rollback_to_unchecked(version, db, receipts)
                .await?;
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

fn get_version(version: crate::util::Version) -> Version {
    let wrapper = v0_3_0::Version::new();
    if *version == wrapper.semver() {
        return Version::V0_3_0(Wrapper(wrapper));
    }
    let wrapper = v0_3_0_1::Version::new();
    if *version == wrapper.semver() {
        return Version::V0_3_0_1(Wrapper(wrapper));
    }
    let wrapper = v0_3_0_2::Version::new();
    if *version == wrapper.semver() {
        return Version::V0_3_0_2(Wrapper(wrapper));
    }
    return Version::Other(version.into_version());
}

pub async fn init<Db: DbHandle>(
    db: &mut Db,
    receipts: &crate::init::InitReceipts,
) -> Result<(), Error> {
    let version: Version = get_version(receipts.server_version.get(db).await?);
    match version {
        Version::V0_3_0(v) => v.0.migrate_to(&Current::new(), db, receipts).await?,
        Version::V0_3_0_1(v) => v.0.migrate_to(&Current::new(), db, receipts).await?,
        Version::V0_3_0_2(v) => v.0.migrate_to(&Current::new(), db, receipts).await?,
        Version::Other(_) => {
            return Err(Error::new(
                eyre!("Cannot downgrade"),
                crate::ErrorKind::InvalidRequest,
            ))
        }
    }
    Ok(())
}

pub const COMMIT_HASH: &str =
    git_version::git_version!(args = ["--always", "--abbrev=40", "--dirty=-modified"]);

#[command(rename = "git-info", local, metadata(authenticated = false))]
pub fn git_info() -> Result<&'static str, Error> {
    Ok(COMMIT_HASH)
}
