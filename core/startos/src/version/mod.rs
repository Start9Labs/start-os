use std::cmp::Ordering;

use color_eyre::eyre::eyre;
use futures::future::BoxFuture;
use futures::{Future, FutureExt};
use imbl_value::{to_value, InternedString};
use patch_db::json_ptr::JsonPointer;

use crate::db::model::Database;
use crate::prelude::*;
use crate::progress::PhaseProgressTrackerHandle;
use crate::Error;

mod v0_3_5;
mod v0_3_5_1;
mod v0_3_5_2;
mod v0_3_6_alpha_0;
mod v0_3_6_alpha_1;
mod v0_3_6_alpha_2;
mod v0_3_6_alpha_3;
mod v0_3_6_alpha_4;
mod v0_3_6_alpha_5;
mod v0_3_6_alpha_6;
mod v0_3_6_alpha_7;

#[derive(Debug, serde::Deserialize, serde::Serialize, HasModel)]
#[model = "Model<Self>"]
pub struct AnyDb(pub Value);

pub type Current = v0_3_6_alpha_5::Version; // VERSION_BUMP

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
#[serde(untagged)]
#[allow(non_camel_case_types)]
enum Version {
    LT0_3_5(LTWrapper<v0_3_5::Version>),
    V0_3_5(Wrapper<v0_3_5::Version>),
    V0_3_5_1(Wrapper<v0_3_5_1::Version>),
    V0_3_5_2(Wrapper<v0_3_5_2::Version>),
    V0_3_6_alpha_0(Wrapper<v0_3_6_alpha_0::Version>),
    V0_3_6_alpha_1(Wrapper<v0_3_6_alpha_1::Version>),
    V0_3_6_alpha_2(Wrapper<v0_3_6_alpha_2::Version>),
    V0_3_6_alpha_3(Wrapper<v0_3_6_alpha_3::Version>),
    V0_3_6_alpha_4(Wrapper<v0_3_6_alpha_4::Version>),
    V0_3_6_alpha_5(Wrapper<v0_3_6_alpha_5::Version>),
    V0_3_6_alpha_6(Wrapper<v0_3_6_alpha_6::Version>),
    V0_3_6_alpha_7(Wrapper<v0_3_6_alpha_7::Version>),
    Other(exver::Version),
}

impl Version {
    fn from_exver_version(version: exver::Version) -> Self {
        serde_json::to_value(version.clone())
            .and_then(serde_json::from_value)
            .unwrap_or_else(|_e| {
                tracing::warn!("Can't deserialize: {:?} and falling back to other", version);
                Version::Other(version)
            })
    }
    #[cfg(test)]
    fn as_exver(&self) -> exver::Version {
        match self {
            Version::LT0_3_5(LTWrapper(_, x)) => x.clone(),
            Version::V0_3_5(Wrapper(x)) => x.semver(),
            Version::V0_3_5_1(Wrapper(x)) => x.semver(),
            Version::V0_3_5_2(Wrapper(x)) => x.semver(),
            Version::V0_3_6_alpha_0(Wrapper(x)) => x.semver(),
            Version::V0_3_6_alpha_1(Wrapper(x)) => x.semver(),
            Version::V0_3_6_alpha_2(Wrapper(x)) => x.semver(),
            Version::V0_3_6_alpha_3(Wrapper(x)) => x.semver(),
            Version::V0_3_6_alpha_4(Wrapper(x)) => x.semver(),
            Version::V0_3_6_alpha_5(Wrapper(x)) => x.semver(),
            Version::V0_3_6_alpha_6(Wrapper(x)) => x.semver(),
            Version::V0_3_6_alpha_7(Wrapper(x)) => x.semver(),
            Version::Other(x) => x.clone(),
        }
    }
}

fn current_version(db: &Value) -> Result<Version, Error> {
    if let Some(public) = db.get("public") {
        from_value(
            public
                .get("serverInfo")
                .or_not_found("`server-info` in .public")?
                .get("version")
                .or_not_found("`version` in .public.serverInfo")?
                .clone(),
        )
    } else {
        from_value(
            db.get("server-info")
                .or_not_found("`server-info` in .")?
                .get("version")
                .or_not_found("`version` in .server-info")?
                .clone(),
        )
    }
}

fn migrate_to<VFrom: VersionT, VTo: VersionT>(
    from: VFrom,
    to: VTo,
    db: &mut Value,
    progress: &mut PhaseProgressTrackerHandle,
) -> Result<impl Future<Output = Result<(), Error>> + Send + 'static, Error> {
    match from.semver().cmp(&to.semver()) {
        Ordering::Greater => rollback_to_unchecked(from, to, db, progress),
        Ordering::Less => migrate_from_unchecked(from, to, db, progress),
        Ordering::Equal => Ok(async { Ok(()) }.boxed()),
    }
}

fn migrate_from_unchecked<VFrom: VersionT, VTo: VersionT>(
    from: VFrom,
    to: VTo,
    db: &mut Value,
    progress: &mut PhaseProgressTrackerHandle,
) -> Result<BoxFuture<'static, Result<(), Error>>, Error> {
    progress.add_total(1);
    let previous = VTo::Previous::default();
    let ff_post_init = if from.semver() < previous.semver() {
        migrate_from_unchecked(from, previous, db, progress)?
    } else if from.semver() > previous.semver() {
        return Err(Error::new(
            eyre!(
                "NO PATH FROM {}, THIS IS LIKELY A MISTAKE IN THE VERSION DEFINITION",
                from.semver()
            ),
            crate::ErrorKind::MigrationFailed,
        ));
    } else {
        async { Ok::<_, Error>(()) }.boxed()
    };
    let post_init = to.up(db)?.boxed();
    to.commit(db)?;
    Ok(async {
        ff_post_init.await?;
        post_init.await?;
        Ok(())
    }
    .boxed())
    // tracing::info!("{} -> {}", previous.semver(), self.semver(),);
    // *progress += 1;
    // Ok(())
}

fn rollback_to_unchecked<VFrom: VersionT, VTo: VersionT>(
    from: VFrom,
    to: VTo,
    db: &mut Value,
    progress: &mut PhaseProgressTrackerHandle,
) -> Result<BoxFuture<'static, Result<(), Error>>, Error> {
    let previous = VFrom::Previous::default();
    let post_init = from.down(db)?.boxed();
    previous.commit(db)?;
    // tracing::info!("{} -> {}", self.semver(), previous.semver(),);
    // self.down(db).await?;
    // previous.commit(db).await?;
    // *progress += 1;
    let ff_post_init = if to.semver() < previous.semver() {
        rollback_to_unchecked(previous, to, db, progress)?
    } else if to.semver() > previous.semver() {
        return Err(Error::new(
            eyre!(
                "NO PATH TO {}, THIS IS LIKELY A MISTAKE IN THE VERSION DEFINITION",
                to.semver()
            ),
            crate::ErrorKind::MigrationFailed,
        ));
    } else {
        async { Ok::<_, Error>(()) }.boxed()
    };
    Ok(async move {
        post_init.await?;
        ff_post_init.await?;
        Ok(())
    }
    .boxed())
}

pub trait VersionT
where
    Self: Default + Copy + Sized + Send + Sync + 'static,
{
    type Previous: VersionT;
    type PreUpRes;
    fn semver(self) -> exver::Version;
    fn compat(self) -> &'static exver::VersionRange;
    fn pre_up(self) -> impl Future<Output = Result<Self::PreUpRes, Error>> + Send + 'static;
    fn up(
        self,
        db: &mut Value,
        input: Self::PreUpRes,
    ) -> Result<impl Future<Output = Result<(), Error>> + Send + 'static, Error>;
    fn down(
        self,
        db: &mut Value,
    ) -> Result<impl Future<Output = Result<(), Error>> + Send + 'static, Error>;
    fn commit(self, db: &mut Value) -> Result<(), Error>;
}

#[derive(Debug, Clone)]
struct LTWrapper<T>(T, exver::Version);
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
        let v = exver::Version::deserialize(deserializer)?;
        let version = T::default();
        if v < version.semver() {
            Ok(Self(version, v))
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
        let v = exver::Version::deserialize(deserializer)?;
        let version = T::default();
        if v == version.semver() {
            Ok(Wrapper(version))
        } else {
            Err(serde::de::Error::custom("Mismatched Version"))
        }
    }
}

pub async fn init(
    db: &TypedPatchDb<Database>,
    mut progress: PhaseProgressTrackerHandle,
) -> Result<(), Error> {
    progress.start();
    db.mutate(|db| {
        db.as_public_mut()
            .as_server_info_mut()
            .as_version_mut()
            .map_mutate(|v| {
                Ok(if v == exver::Version::new([0, 3, 6], []) {
                    v0_3_6_alpha_0::Version::default().semver()
                } else {
                    v
                })
            })
    })
    .await?; // TODO: remove before releasing 0.3.6
    let version = Version::from_exver_version(
        db.peek()
            .await
            .as_public()
            .as_server_info()
            .as_version()
            .de()?,
    );
    let current_version = Current::new();

    db.mutate(|db| {
        db.as_public_mut()
            .as_server_info_mut()
            .as_version_mut()
            .ser(&current_version.semver().into())?;

        db.as_public_mut()
            .as_server_info_mut()
            .as_eos_version_compat_mut()
            .ser(&current_version.compat())?;
        Ok(())
    })
    .await?;
    let mut value: Value = db.get(&JsonPointer::<&'static str>::default()).await?;
    match version {
        Version::LT0_3_5(_) => {
            return Err(Error::new(
                eyre!("Cannot migrate from pre-0.3.5. Please update to v0.3.5 first."),
                ErrorKind::MigrationFailed,
            ));
        }
        Version::V0_3_5(v) => {
            v.0.migrate_to(&current_version, &mut value, &mut progress)?
        }
        Version::V0_3_5_1(v) => {
            v.0.migrate_to(&current_version, &mut value, &mut progress)?
        }
        Version::V0_3_5_2(v) => {
            v.0.migrate_to(&current_version, &mut value, &mut progress)?
        }
        Version::V0_3_6_alpha_0(v) => {
            v.0.migrate_to(&current_version, &mut value, &mut progress)?
        }
        Version::V0_3_6_alpha_1(v) => {
            v.0.migrate_to(&current_version, &mut value, &mut progress)?
        }
        Version::V0_3_6_alpha_2(v) => {
            v.0.migrate_to(&current_version, &mut value, &mut progress)?
        }
        Version::V0_3_6_alpha_3(v) => {
            v.0.migrate_to(&current_version, &mut value, &mut progress)?
        }
        Version::V0_3_6_alpha_4(v) => {
            v.0.migrate_to(&current_version, &mut value, &mut progress)?
        }
        Version::V0_3_6_alpha_5(v) => {
            v.0.migrate_to(&current_version, &mut value, &mut progress)?
        }
        Version::V0_3_6_alpha_6(v) => {
            v.0.migrate_to(&current_version, &mut value, &mut progress)?
        }
        Version::V0_3_6_alpha_7(v) => {
            v.0.migrate_to(&current_version, &mut value, &mut progress)?
        }
        Version::Other(_) => {
            return Err(Error::new(
                eyre!("Cannot downgrade"),
                crate::ErrorKind::InvalidRequest,
            ))
        }
    };
    db.mutate(|db| db.ser(&imbl_value::from_value(value)?))
        .await?;

    progress.complete();
    Ok(())
}

pub const COMMIT_HASH: &str =
    include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../../GIT_HASH.txt"));

pub fn git_info() -> Result<InternedString, Error> {
    Ok(InternedString::intern(COMMIT_HASH))
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use super::*;

    fn em_version() -> impl Strategy<Value = exver::Version> {
        any::<(usize, usize, usize, bool)>().prop_map(|(major, minor, patch, alpha)| {
            if alpha {
                exver::Version::new(
                    [0, major, minor]
                        .into_iter()
                        .chain(Some(patch).filter(|n| *n != 0)),
                    [],
                )
            } else {
                exver::Version::new([major, minor, patch], [])
            }
        })
    }

    fn versions() -> impl Strategy<Value = Version> {
        prop_oneof![
            Just(Version::V0_3_5(Wrapper(v0_3_5::Version::new()))),
            Just(Version::V0_3_5_1(Wrapper(v0_3_5_1::Version::new()))),
            Just(Version::V0_3_5_2(Wrapper(v0_3_5_2::Version::new()))),
            Just(Version::V0_3_6_alpha_0(Wrapper(
                v0_3_6_alpha_0::Version::new()
            ))),
            Just(Version::V0_3_6_alpha_1(Wrapper(
                v0_3_6_alpha_1::Version::new()
            ))),
            Just(Version::V0_3_6_alpha_2(Wrapper(
                v0_3_6_alpha_2::Version::new()
            ))),
            em_version().prop_map(Version::Other),
        ]
    }

    proptest! {
        #[test]
        fn exversion_isomorphic_version(original in em_version()) {
            let version = Version::from_exver_version(original.clone().into());
            let back = version.as_exver();
            prop_assert_eq!(original, back, "All versions should round trip");
        }
        #[test]
        fn version_isomorphic_em_version(version in versions()) {
            let sem_ver = version.as_exver();
            let back = Version::from_exver_version(sem_ver.into());
            prop_assert_eq!(format!("{:?}",version), format!("{:?}", back), "All versions should round trip");
        }
    }
}
