use linear_map::LinearMap;

use super::*;
use crate::util::{to_yaml_async_writer, PersistencePath};

const V0_2_0: emver::Version = emver::Version::new(0, 2, 0, 0);

pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_1_5::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> &'static emver::Version {
        &V0_2_0
    }
    async fn up(&self) -> Result<(), Error> {
        let app_info: LinearMap<String, crate::apps::AppInfo> = legacy::apps::list_info()
            .await?
            .into_iter()
            .map(|(id, ai)| {
                (
                    id,
                    crate::apps::AppInfo {
                        title: ai.title,
                        version: ai.version,
                        tor_address: ai.tor_address,
                        configured: ai.configured,
                        recoverable: ai.recoverable,
                        needs_restart: false,
                    },
                )
            })
            .collect();
        let mut apps_file = PersistencePath::from_ref("apps.yaml").write(None).await?;
        to_yaml_async_writer(&mut *apps_file, &app_info).await?;
        apps_file.commit().await?;

        Ok(())
    }
    async fn down(&self) -> Result<(), Error> {
        let app_info: LinearMap<String, legacy::apps::AppInfo> = crate::apps::list_info()
            .await?
            .into_iter()
            .map(|(id, ai)| {
                (
                    id,
                    legacy::apps::AppInfo {
                        title: ai.title,
                        version: ai.version,
                        tor_address: ai.tor_address,
                        configured: ai.configured,
                        recoverable: ai.recoverable,
                    },
                )
            })
            .collect();
        let mut apps_file = PersistencePath::from_ref("apps.yaml").write(None).await?;
        to_yaml_async_writer(&mut *apps_file, &app_info).await?;
        apps_file.commit().await?;

        Ok(())
    }
}

mod legacy {
    pub mod apps {
        use linear_map::LinearMap;

        use crate::util::{from_yaml_async_reader, PersistencePath};
        use crate::Error;

        fn not(b: &bool) -> bool {
            !b
        }

        #[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
        pub struct AppInfo {
            pub title: String,
            pub version: emver::Version,
            pub tor_address: Option<String>,
            pub configured: bool,
            #[serde(default)]
            #[serde(skip_serializing_if = "not")]
            pub recoverable: bool,
        }

        pub async fn list_info() -> Result<LinearMap<String, AppInfo>, Error> {
            let apps_path = PersistencePath::from_ref("apps.yaml");
            let mut f = match apps_path.maybe_read(false).await.transpose()? {
                Some(a) => a,
                None => return Ok(LinearMap::new()),
            };
            from_yaml_async_reader(&mut *f).await
        }
    }
}
