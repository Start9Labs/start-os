use super::*;
use crate::util::Invoke;

const V0_2_7: emver::Version = emver::Version::new(0, 2, 7, 0);

pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_2_6::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> &'static emver::Version {
        &V0_2_7
    }
    async fn up(&self) -> Result<(), Error> {
        for (app_id, _) in crate::apps::list_info().await? {
            tokio::process::Command::new("docker")
                .arg("stop")
                .arg(&app_id)
                .invoke("Docker")
                .await?;
            tokio::process::Command::new("docker")
                .arg("update")
                .arg("--restart")
                .arg("no")
                .arg(&app_id)
                .invoke("Docker")
                .await?;
        }
        Ok(())
    }
    async fn down(&self) -> Result<(), Error> {
        Ok(())
    }
}
