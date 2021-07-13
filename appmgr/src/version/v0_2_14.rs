use super::*;

const V0_2_14: emver::Version = emver::Version::new(0, 2, 14, 0);

pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_2_13::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> &'static emver::Version {
        &V0_2_14
    }
    async fn up(&self) -> Result<(), Error> {
        Ok(())
    }
    async fn down(&self) -> Result<(), Error> {
        Ok(())
    }
}
