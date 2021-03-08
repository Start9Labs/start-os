use super::*;

const V0_2_10: emver::Version = emver::Version::new(0, 2, 10, 0);

pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_2_9::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> &'static emver::Version {
        &V0_2_10
    }
    async fn up(&self) -> Result<(), Error> {
        Ok(())
    }
    async fn down(&self) -> Result<(), Error> {
        Ok(())
    }
}
