use super::*;

const V0_1_5: emver::Version = emver::Version::new(0, 1, 5, 0);

pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_1_4::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> &'static emver::Version {
        &V0_1_5
    }
    async fn up(&self) -> Result<(), Error> {
        Ok(())
    }
    async fn down(&self) -> Result<(), Error> {
        Ok(())
    }
}
