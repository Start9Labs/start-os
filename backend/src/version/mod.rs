use std::cmp::Ordering;

use async_trait::async_trait;
use color_eyre::eyre::eyre;
use rpc_toolkit::command;
use sqlx::PgPool;

use crate::prelude::*;
use crate::Error;

mod v0_3_5;
mod v0_4_0;

pub type Current = v0_4_0::Version;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
#[serde(untagged)]
enum Version {
    LT0_3_4_3(LTWrapper<v0_3_5::Version>),
    V0_3_4_3(Wrapper<v0_3_5::Version>),
    V0_4_0(Wrapper<v0_4_0::Version>),
    Other(emver::Version),
}

impl Version {
    fn from_util_version(version: crate::util::Version) -> Self {
        serde_json::to_value(version.clone())
            .and_then(serde_json::from_value)
            .unwrap_or_else(|_e| {
                tracing::warn!("Can't deserialize: {:?} and falling back to other", version);
                Version::Other(version.into_version())
            })
    }
    #[cfg(test)]
    fn as_sem_ver(&self) -> emver::Version {
        match self {
            Version::LT0_3_4_3(LTWrapper(_, x)) => x.clone(),
            Version::V0_3_4_3(Wrapper(x)) => x.semver(),
            Version::V0_4_0(Wrapper(x)) => x.semver(),
            Version::Other(x) => x.clone(),
        }
    }
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
    async fn up(&self, db: &PatchDb, secrets: &PgPool) -> Result<(), Error>;
    async fn down(&self, db: &PatchDb, secrets: &PgPool) -> Result<(), Error>;
    async fn commit(&self, db: &PatchDb) -> Result<(), Error> {
        let semver = self.semver().into();
        let compat = self.compat().clone();
        db.mutate(|d| {
            d.as_server_info_mut().as_version_mut().ser(&semver)?;
            d.as_server_info_mut()
                .as_eos_version_compat_mut()
                .ser(&compat)?;
            Ok(())
        })
        .await?;
        Ok(())
    }
    async fn migrate_to<V: VersionT>(
        &self,
        version: &V,
        db: &PatchDb,
        secrets: &PgPool,
    ) -> Result<(), Error> {
        match self.semver().cmp(&version.semver()) {
            Ordering::Greater => self.rollback_to_unchecked(version, db, secrets).await,
            Ordering::Less => version.migrate_from_unchecked(self, db, secrets).await,
            Ordering::Equal => Ok(()),
        }
    }
    async fn migrate_from_unchecked<V: VersionT>(
        &self,
        version: &V,
        db: &PatchDb,
        secrets: &PgPool,
    ) -> Result<(), Error> {
        let previous = Self::Previous::new();
        if version.semver() < previous.semver() {
            previous
                .migrate_from_unchecked(version, db, secrets)
                .await?;
        } else if version.semver() > previous.semver() {
            return Err(Error::new(
                eyre!(
                    "NO PATH FROM {}, THIS IS LIKELY A MISTAKE IN THE VERSION DEFINITION",
                    version.semver()
                ),
                crate::ErrorKind::MigrationFailed,
            ));
        }
        tracing::info!("{} -> {}", previous.semver(), self.semver(),);
        self.up(db, secrets).await?;
        self.commit(db).await?;
        Ok(())
    }
    async fn rollback_to_unchecked<V: VersionT>(
        &self,
        version: &V,
        db: &PatchDb,
        secrets: &PgPool,
    ) -> Result<(), Error> {
        let previous = Self::Previous::new();
        tracing::info!("{} -> {}", self.semver(), previous.semver(),);
        self.down(db, secrets).await?;
        previous.commit(db).await?;
        if version.semver() < previous.semver() {
            previous.rollback_to_unchecked(version, db, secrets).await?;
        } else if version.semver() > previous.semver() {
            return Err(Error::new(
                eyre!(
                    "NO PATH TO {}, THIS IS LIKELY A MISTAKE IN THE VERSION DEFINITION",
                    version.semver()
                ),
                crate::ErrorKind::MigrationFailed,
            ));
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct LTWrapper<T>(T, emver::Version);
impl<T> serde::Serialize for LTWrapper<T>
where
    T: VersionT,
{
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.0.semver().serialize(serializer)
    }
}
impl<'de, T> serde::Deserialize<'de> for LTWrapper<T>
where
    T: VersionT,
{
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let v = crate::util::Version::deserialize(deserializer)?;
        let version = T::new();
        if *v < version.semver() {
            Ok(Self(version, v.into_version()))
        } else {
            Err(serde::de::Error::custom("Mismatched Version"))
        }
    }
}

#[derive(Debug, Clone)]
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
        if *v == version.semver() {
            Ok(Self(version))
        } else {
            Err(serde::de::Error::custom("Mismatched Version"))
        }
    }
}

pub async fn init(db: &PatchDb, secrets: &PgPool) -> Result<(), Error> {
    let version = Version::from_util_version(db.peek().await?.as_server_info().as_version().de()?);

    match version {
        Version::LT0_3_4_3(_) => {
            return Err(Error::new(
                eyre!("Cannot migrate from pre-0.3.4. Please update to v0.3.4 first."),
                crate::ErrorKind::MigrationFailed,
            ));
        }
        Version::V0_3_4_3(v) => v.0.migrate_to(&Current::new(), db, secrets).await?,
        Version::V0_4_0(v) => v.0.migrate_to(&Current::new(), db, secrets).await?,
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

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use super::*;

    fn em_version() -> impl Strategy<Value = emver::Version> {
        any::<(usize, usize, usize, usize)>().prop_map(|(major, minor, patch, super_minor)| {
            emver::Version::new(major, minor, patch, super_minor)
        })
    }

    fn versions() -> impl Strategy<Value = Version> {
        prop_oneof![
            em_version().prop_map(|v| if v < v0_3_5::Version::new().semver() {
                Version::LT0_3_4_3(LTWrapper(v0_3_5::Version::new(), v))
            } else {
                Version::LT0_3_4_3(LTWrapper(
                    v0_3_5::Version::new(),
                    emver::Version::new(0, 3, 0, 0),
                ))
            }),
            Just(Version::V0_3_4_3(Wrapper(v0_3_5::Version::new()))),
            Just(Version::V0_4_0(Wrapper(v0_4_0::Version::new()))),
            em_version().prop_map(Version::Other),
        ]
    }

    proptest! {
        #[test]
        fn emversion_isomorphic_version(original in em_version()) {
            let version = Version::from_util_version(original.clone().into());
            let back = version.as_sem_ver();
            prop_assert_eq!(original, back, "All versions should round trip");
        }
        #[test]
        fn version_isomorphic_em_version(version in versions()) {
            let sem_ver = version.as_sem_ver();
            let back = Version::from_util_version(sem_ver.into());
            prop_assert_eq!(format!("{:?}",version), format!("{:?}", back), "All versions should round trip");
        }
    }
}
