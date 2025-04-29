use exver::{PreReleaseSegment, VersionRange};
use imbl_value::json;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_4_0_alpha_0, VersionT};
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_4_0_alpha_1: exver::Version = exver::Version::new(
        [0, 4, 0],
        [PreReleaseSegment::String("alpha".into()), 1.into()]
    );
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_4_0_alpha_0::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_4_0_alpha_1.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    fn up(self, db: &mut Value, _: Self::PreUpRes) -> Result<(), Error> {
        let Some(ui) = db["public"]["ui"].as_object_mut() else {
            return Err(Error::new(
                eyre!("db.public.ui is not an object"),
                ErrorKind::Database,
            ));
        };
        ui.insert(
            "registries".into(),
            Value::Object(
                ui.get("marketplace")
                    .and_then(|m| m.get("knownHosts"))
                    .and_then(|kh| kh.as_object())
                    .into_iter()
                    .flatten()
                    .map(|(k, v)| (k.clone(), v["name"].clone()))
                    .collect(),
            ),
        );
        if let Some(highscore) = ui
            .get_mut("gaming")
            .and_then(|g| g.get_mut("snake"))
            .and_then(|s| s.get_mut("highScore"))
            .map(|hs| hs.take())
            .filter(|s| s.is_number())
        {
            ui.insert("snakeHighScore".into(), highscore);
        }
        ui.insert(
            "startosRegistry".into(),
            json!("https://registry.start9.com/"),
        );
        ui.remove("marketplace");
        ui.remove("gaming");
        ui.remove("theme");

        Ok(())
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
