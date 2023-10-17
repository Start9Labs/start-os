use std::collections::BTreeMap;
use std::path::Path;

use async_trait::async_trait;
use emver::VersionRange;
use models::DataUrl;
use sqlx::PgPool;

use super::v0_3_4::V0_3_0_COMPAT;
use super::{v0_3_4_4, VersionT};
use crate::prelude::*;

const V0_3_5: emver::Version = emver::Version::new(0, 3, 5, 0);

#[derive(Clone, Debug)]
pub struct Version;

#[async_trait]
impl VersionT for Version {
    type Previous = v0_3_4_4::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_5
    }
    fn compat(&self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    async fn up(&self, db: PatchDb, _secrets: &PgPool) -> Result<(), Error> {
        let peek = db.peek().await;
        let mut url_replacements = BTreeMap::new();
        for (_, pde) in peek.as_package_data().as_entries()? {
            for (dependency, info) in pde
                .as_installed()
                .map(|i| i.as_dependency_info().as_entries())
                .transpose()?
                .into_iter()
                .flatten()
            {
                if !url_replacements.contains_key(&dependency) {
                    url_replacements.insert(
                        dependency,
                        DataUrl::from_path(
                            <&Value>::from(info.as_icon())
                                .as_str()
                                .and_then(|i| i.strip_prefix("/public/package-data/"))
                                .map(|path| {
                                    Path::new("/embassy-data/package-data/public").join(path)
                                })
                                .unwrap_or_default(),
                        )
                        .await
                        .unwrap_or_else(|_| {
                            DataUrl::from_slice(
                                "image/png",
                                include_bytes!("../install/package-icon.png"),
                            )
                        }),
                    );
                }
            }
        }
        db.mutate(|v| {
            for (_, pde) in v.as_package_data_mut().as_entries_mut()? {
                for (dependency, info) in pde
                    .as_installed_mut()
                    .map(|i| i.as_dependency_info_mut().as_entries_mut())
                    .transpose()?
                    .into_iter()
                    .flatten()
                {
                    if let Some(url) = url_replacements.get(&dependency) {
                        info.as_icon_mut().ser(url)?;
                    } else {
                        info.as_icon_mut().ser(&DataUrl::from_slice(
                            "image/png",
                            include_bytes!("../install/package-icon.png"),
                        ))?;
                    }
                    let manifest = <&mut Value>::from(&mut *info)
                        .as_object_mut()
                        .and_then(|o| o.remove("manifest"));
                    if let Some(title) = manifest
                        .as_ref()
                        .and_then(|m| m.as_object())
                        .and_then(|m| m.get("title"))
                        .and_then(|t| t.as_str())
                        .map(|s| s.to_owned())
                    {
                        info.as_title_mut().ser(&title)?;
                    } else {
                        info.as_title_mut().ser(&dependency.to_string())?;
                    }
                }
            }
            Ok(())
        })
        .await?;
        Ok(())
    }
    async fn down(&self, _db: PatchDb, _secrets: &PgPool) -> Result<(), Error> {
        Ok(())
    }
}
