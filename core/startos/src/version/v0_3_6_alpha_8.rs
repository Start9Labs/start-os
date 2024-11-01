use exver::{PreReleaseSegment, VersionRange};
use imbl_value::{json, InOMap};
use tokio::process::Command;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_6_alpha_7, VersionT};
use crate::{
    install::PKG_ARCHIVE_DIR,
    s9pk::{
        manifest::{DeviceFilter, Manifest},
        merkle_archive::{Entry, MerkleArchive},
        v2::SIG_CONTEXT,
        S9pk,
    },
    util::{io::create_file, Invoke},
};
use crate::{prelude::*, s9pk::manifest};

lazy_static::lazy_static! {
    static ref V0_3_6_alpha_8: exver::Version = exver::Version::new(
        [0, 3, 6],
        [PreReleaseSegment::String("alpha".into()), 8.into()]
    );
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_3_6_alpha_7::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_3_6_alpha_8.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    fn up(self, _: &mut Value, _: Self::PreUpRes) -> Result<(), Error> {
        Ok(())
    }
    async fn post_up(self, ctx: &crate::context::RpcContext) -> Result<(), Error> {
        let s9pk_dir = ctx.datadir.join(PKG_ARCHIVE_DIR).join("installed");

        for s9pk_path in s9pk_dir.read_dir()? {
            let s9pk_path = s9pk_path?.path();
            if s9pk_path.extension().map(|x| x == "s9pk").unwrap_or(false) {
                continue;
            }

            let original_pack = match S9pk::open(&s9pk_path, None).await {
                Ok(a) => a,
                Err(e) => {
                    tracing::error!("Error opening s9pk for install: {e}");
                    tracing::debug!("{e:?}");
                    continue;
                }
            };
            let archive = original_pack.as_archive();

            let previous_manifest: Value = serde_json::from_slice::<serde_json::Value>(
                &archive
                    .contents()
                    .get_path("manifest.json")
                    .or_not_found("manifest.json")?
                    .read_file_to_vec()
                    .await?,
            )
            .with_kind(ErrorKind::Deserialization)?
            .into();

            let mut manifest = previous_manifest.clone();

            if let Some(device) = previous_manifest["hardwareRequirements"]["device"].as_object() {
                manifest["hardwareRequirements"]["device"] = to_value(
                    &device
                        .into_iter()
                        .map(|(class, product)| {
                            Ok::<_, Error>(DeviceFilter {
                                pattern_description: format!(
                                    "a {class} device matching the expression {}",
                                    &product
                                ),
                                class: class.clone(),
                                pattern: from_value(product.clone())?,
                            })
                        })
                        .fold(Ok::<_, Error>(Vec::new()), |acc, value| {
                            let mut acc = acc?;
                            acc.push(value?);
                            Ok(acc)
                        })?,
                )?;
            }
            if previous_manifest != manifest {
                let tmp_path = s9pk_path.with_extension("s9pk.tmp");
                let mut tmp_file = create_file(&tmp_path).await?;
                // TODO, wouldn't this break in the later versions of the manifest that would need changes, this doesn't seem to be a good way to handle this
                let manifest: Manifest = from_value(manifest.clone())?;
                let mut s9pk: S9pk<_> = S9pk::new_with_manifest(archive.clone(), None, manifest);
                let s9pk_compat_key = todo!("There is a key used in the compat that we could use ctx.developer_key()?.clone()");
                s9pk.as_archive_mut()
                    .set_signer(s9pk_compat_key, SIG_CONTEXT);
                s9pk.serialize(&mut tmp_file, true).await?;
                tmp_file.sync_all().await?;
                tokio::fs::rename(&tmp_path, &s9pk_path).await?;
            }
        }

        Ok(())
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
