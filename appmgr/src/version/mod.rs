use std::cmp::Ordering;

use async_trait::async_trait;
use futures::stream::TryStreamExt;
use lazy_static::lazy_static;
use patch_db::DbHandle;
use rpc_toolkit::command;
use tokio_compat_02::FutureExt;

// mod v0_1_0;
// mod v0_1_1;
// mod v0_1_2;
// mod v0_1_3;
// mod v0_1_4;
// mod v0_1_5;
// mod v0_2_0;
// mod v0_2_1;
// mod v0_2_2;
// mod v0_2_3;
// mod v0_2_4;
// mod v0_2_5;
// mod v0_2_6;
// mod v0_2_7;
// mod v0_2_8;
// mod v0_2_9;

// mod v0_2_10;
// mod v0_2_11;
// mod v0_2_12;

// pub use v0_2_12::Version as Current;
pub type Current = ();

use crate::context::{CliContext, EitherContext, RpcContext};
use crate::util::{to_yaml_async_writer, AsyncCompat};
use crate::{Error, ResultExt as _};

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
enum Version {
    V0_0_0(Wrapper<()>),
    // V0_1_0(Wrapper<v0_1_0::Version>),
    // V0_1_1(Wrapper<v0_1_1::Version>),
    // V0_1_2(Wrapper<v0_1_2::Version>),
    // V0_1_3(Wrapper<v0_1_3::Version>),
    // V0_1_4(Wrapper<v0_1_4::Version>),
    // V0_1_5(Wrapper<v0_1_5::Version>),
    // V0_2_0(Wrapper<v0_2_0::Version>),
    // V0_2_1(Wrapper<v0_2_1::Version>),
    // V0_2_2(Wrapper<v0_2_2::Version>),
    // V0_2_3(Wrapper<v0_2_3::Version>),
    // V0_2_4(Wrapper<v0_2_4::Version>),
    // V0_2_5(Wrapper<v0_2_5::Version>),
    // V0_2_6(Wrapper<v0_2_6::Version>),
    // V0_2_7(Wrapper<v0_2_7::Version>),
    // V0_2_8(Wrapper<v0_2_8::Version>),
    // V0_2_9(Wrapper<v0_2_9::Version>),
    // V0_2_10(Wrapper<v0_2_10::Version>),
    // V0_2_11(Wrapper<v0_2_11::Version>),
    // V0_2_12(Wrapper<v0_2_12::Version>),
    Other(emver::Version),
}

#[async_trait]
pub trait VersionT
where
    Self: Sized + Send + Sync,
{
    type Previous: VersionT;
    fn new() -> Self;
    fn semver(&self) -> &'static crate::util::Version;
    async fn up<Db: DbHandle>(&self, db: &mut Db) -> Result<(), Error>;
    async fn down<Db: DbHandle>(&self, db: &mut Db) -> Result<(), Error>;
    async fn commit<Db: DbHandle>(&self, db: &mut Db) -> Result<(), Error> {
        crate::db::DatabaseModel::new()
            .server_info()
            .version()
            .put(db, self.semver())
            .await?;

        Ok(())
    }
    async fn migrate_to<V: VersionT, Db: DbHandle>(
        &self,
        version: &V,
        db: &mut Db,
    ) -> Result<(), Error> {
        match self.semver().cmp(version.semver()) {
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
        log::info!(
            "{} -> {}",
            previous.semver().as_str(),
            self.semver().as_str()
        );
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
        log::info!(
            "{} -> {}",
            self.semver().as_str(),
            previous.semver().as_str()
        );
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
        if &v == version.semver() {
            Ok(Wrapper(version))
        } else {
            Err(serde::de::Error::custom("Mismatched Version"))
        }
    }
}
lazy_static! {
    static ref V0_0_0: crate::util::Version = emver::Version::new(0, 0, 0, 0).into();
}
#[async_trait]
impl VersionT for () {
    type Previous = ();
    fn new() -> Self {
        ()
    }
    fn semver(&self) -> &'static crate::util::Version {
        &*V0_0_0
    }
    async fn up<Db: DbHandle>(&self, db: &mut Db) -> Result<(), Error> {
        Ok(())
    }
    async fn down<Db: DbHandle>(&self, db: &mut Db) -> Result<(), Error> {
        Ok(())
    }
}

pub async fn init() -> Result<(), Error> {
    todo!()
}

pub async fn self_update(requirement: emver::VersionRange) -> Result<(), Error> {
    todo!()
}

#[command(rename = "git-info", local)]
pub fn git_info(#[context] _ctx: EitherContext) -> Result<String, Error> {
    Ok(
        git_version::git_version!(args = ["--always", "--abbrev=40", "--dirty=-modified"])
            .to_owned(),
    )
}
