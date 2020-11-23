use super::*;

const V0_2_2: emver::Version = emver::Version::new(0, 2, 2, 0);

pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_2_1::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> &'static emver::Version {
        &V0_2_2
    }
    async fn up(&self) -> Result<(), Error> {
        Ok(())
    }
    async fn down(&self) -> Result<(), Error> {
        Ok(())
    }
}
