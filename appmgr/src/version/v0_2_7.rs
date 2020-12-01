use super::*;

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
        Ok(())
    }
    async fn down(&self) -> Result<(), Error> {
        Ok(())
    }
}
