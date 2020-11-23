use super::*;

const V0_1_3: emver::Version = emver::Version::new(0, 1, 3, 0);

pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_1_2::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> &'static emver::Version {
        &V0_1_3
    }
    async fn up(&self) -> Result<(), Error> {
        Ok(())
    }
    async fn down(&self) -> Result<(), Error> {
        Ok(())
    }
}
