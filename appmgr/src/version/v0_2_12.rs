use super::*;
use std::os::unix::process::ExitStatusExt;

const V0_2_12: emver::Version = emver::Version::new(0, 2, 12, 0);

pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_2_11::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> &'static emver::Version {
        &V0_2_12
    }
    async fn up(&self) -> Result<(), Error> {
        crate::tor::write_lan_services(
            &crate::tor::services_map(&PersistencePath::from_ref(crate::SERVICES_YAML)).await?,
        )
        .await?;
        let svc_exit = std::process::Command::new("service")
            .args(&["nginx", "reload"])
            .status()?;
        crate::ensure_code!(
            svc_exit.success(),
            crate::error::GENERAL_ERROR,
            "Failed to Reload Nginx: {}",
            svc_exit
                .code()
                .or_else(|| { svc_exit.signal().map(|a| 128 + a) })
                .unwrap_or(0)
        );
        Ok(())
    }
    async fn down(&self) -> Result<(), Error> {
        Ok(())
    }
}
