use futures::StreamExt;
use futures::TryStreamExt;
use linear_map::LinearMap;

use super::*;

const V0_1_2: emver::Version = emver::Version::new(0, 1, 2, 0);

pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_1_1::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> &'static emver::Version {
        &V0_1_2
    }
    async fn up(&self) -> Result<(), Error> {
        let app_info = legacy::apps::list_info().await?;
        for (name, _) in &app_info {
            let p = PersistencePath::from_ref("apps")
                .join(name)
                .join("manifest.yaml");
            let mut f = p.for_update().await?;
            let manifest: crate::manifest::ManifestV0 = crate::util::from_yaml_async_reader(&mut f)
                .await
                .no_code()?;
            let mut f = f.into_writer().await?;
            crate::util::to_yaml_async_writer(&mut f, &crate::manifest::Manifest::V0(manifest))
                .await
                .no_code()?;
            f.commit().await?;
        }

        let p = PersistencePath::from_ref("apps.yaml");
        let exists = p.path().exists();
        let mut f = p.for_update().await?;
        let info: LinearMap<String, legacy::apps::AppInfo> = if exists {
            crate::util::from_yaml_async_reader(&mut f)
                .await
                .no_code()?
        } else {
            LinearMap::new()
        };
        let new_info: LinearMap<String, crate::apps::AppInfo> = futures::stream::iter(info)
            .then(|(name, i)| async move {
                let title = crate::apps::manifest(&name).await?.title;
                Ok::<_, Error>((
                    name,
                    crate::apps::AppInfo {
                        title,
                        version: i.version,
                        tor_address: i.tor_address,
                        configured: i.configured,
                        recoverable: false,
                        needs_restart: false,
                    },
                ))
            })
            .try_collect()
            .await?;
        let mut f = f.into_writer().await?;
        crate::util::to_yaml_async_writer(&mut f, &new_info)
            .await
            .no_code()?;
        f.commit().await?;
        Ok(())
    }
    async fn down(&self) -> Result<(), Error> {
        Ok(())
    }
}

mod legacy {
    pub mod apps {
        use linear_map::LinearMap;

        use crate::util::from_yaml_async_reader;
        use crate::util::Apply;
        use crate::util::PersistencePath;
        use crate::Error;

        #[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
        pub struct AppInfo {
            pub version: emver::Version,
            pub tor_address: Option<String>,
            pub configured: bool,
        }

        pub async fn list_info() -> Result<LinearMap<String, AppInfo>, Error> {
            let apps_path = PersistencePath::from_ref("apps.yaml");
            Ok(apps_path
                .maybe_read(false)
                .await
                .transpose()?
                .map(|mut f| async move { from_yaml_async_reader(&mut *f).await })
                .apply(futures::future::OptionFuture::from)
                .await
                .transpose()?
                .unwrap_or_else(LinearMap::new))
        }
    }
}
