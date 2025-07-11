use std::collections::BTreeMap;
use std::path::Path;
use std::sync::Arc;

use exver::{PreReleaseSegment, VersionRange};
use imbl_value::{InOMap, InternedString};
use models::PackageId;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_4_0_alpha_8, VersionT};
use crate::context::RpcContext;
use crate::install::PKG_ARCHIVE_DIR;
use crate::prelude::*;
use crate::util::io::write_file_atomic;
use crate::volume::PKG_VOLUME_DIR;
use crate::DATA_DIR;

lazy_static::lazy_static! {
    static ref V0_4_0_alpha_9: exver::Version = exver::Version::new(
        [0, 4, 0],
        [PreReleaseSegment::String("alpha".into()), 9.into()]
    );
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_4_0_alpha_8::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_4_0_alpha_9.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    #[instrument]
    fn up(self, db: &mut Value, _: Self::PreUpRes) -> Result<Value, Error> {
        let mut res = InOMap::new();
        let todos = db
            .get_mut("public")
            .and_then(|p| p.get_mut("serverInfo"))
            .and_then(|si| si.get_mut("postInitMigrationTodos"))
            .or_not_found("`public.serverInfo.postInitMigrationTodos` in db")?;
        if let Some(prev) = todos.take().as_array() {
            *todos = Value::Object(
                prev.into_iter()
                    .filter_map(|version| version.as_str())
                    .map(InternedString::intern)
                    .map(|v| (v, Value::Null))
                    .collect(),
            );
        }
        for (id, pde) in db
            .get_mut("public")
            .and_then(|si| si.get_mut("packageData"))
            .and_then(|pd| pd.as_object_mut())
            .into_iter()
            .flat_map(|m| m.iter_mut())
        {
            let Some(pde) = pde.as_object_mut() else {
                continue;
            };
            res.insert(id.clone(), pde.remove("dataVersion").unwrap_or_default());
            pde.insert(
                "s9pk".into(),
                Value::String(Arc::new(
                    Path::new(DATA_DIR)
                        .join(PKG_ARCHIVE_DIR)
                        .join("installed")
                        .join(id)
                        .with_extension("s9pk")
                        .into_os_string()
                        .into_string()
                        .map_or_else(|o| o.to_string_lossy().into_owned(), |a| a),
                )),
            );
        }

        Ok(Value::Object(res))
    }
    async fn post_up(self, _ctx: &RpcContext, input: Value) -> Result<(), Error> {
        for (id, data_version) in from_value::<BTreeMap<PackageId, Option<String>>>(input)? {
            if let Some(data_version) = data_version {
                write_file_atomic(
                    Path::new(DATA_DIR)
                        .join(PKG_VOLUME_DIR)
                        .join(&id)
                        .join("data")
                        .join(".version"),
                    data_version.as_bytes(),
                )
                .await?;
            }
        }

        Ok(())
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
