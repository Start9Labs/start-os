use exver::{PreReleaseSegment, VersionRange};
use tokio::fs::File;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_3_6_alpha_7, VersionT};
use crate::s9pk::manifest::{DeviceFilter, Manifest};
use crate::s9pk::merkle_archive::MerkleArchive;
use crate::s9pk::v2::SIG_CONTEXT;
use crate::s9pk::S9pk;
use crate::util::io::create_file;
use crate::{
    install::PKG_ARCHIVE_DIR, s9pk::merkle_archive::source::multi_cursor_file::MultiCursorFile,
};
use crate::{prelude::*, service::LoadDisposition};

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
            let matches_s9pk = s9pk_path.extension().map(|x| x == "s9pk").unwrap_or(false);
            if !matches_s9pk {
                continue;
            }

            let get_archive = async {
                let multi_cursor = MultiCursorFile::from(File::open(&s9pk_path).await?);
                Ok::<_, Error>(S9pk::archive(&multi_cursor, None).await?)
            };

            let archive: MerkleArchive<
                crate::s9pk::merkle_archive::source::Section<MultiCursorFile>,
            > = match get_archive.await {
                Ok(a) => a,
                Err(e) => {
                    tracing::error!("Error opening s9pk for install: {e}");
                    tracing::debug!("{e:?}");
                    continue;
                }
            };

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
                let id = manifest.id.clone();
                let mut s9pk: S9pk<_> = S9pk::new_with_manifest(archive, None, manifest);
                let s9pk_compat_key = ctx.account.read().await.compat_s9pk_key.clone();
                s9pk.as_archive_mut()
                    .set_signer(s9pk_compat_key, SIG_CONTEXT);
                s9pk.serialize(&mut tmp_file, true).await?;
                tmp_file.sync_all().await?;
                tokio::fs::rename(&tmp_path, &s9pk_path).await?;
                ctx.services.load(ctx, &id, LoadDisposition::Retry).await?;
            }
        }

        Ok(())
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
