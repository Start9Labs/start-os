use super::*;
use crate::util::Invoke;

const V0_2_8: emver::Version = emver::Version::new(0, 2, 8, 0);

pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_2_8::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> &'static emver::Version {
        &V0_2_4
    }
    async fn up(&self) -> Result<(), Error> {
        Ok(())
    }
    async fn down(&self) -> Result<(), Error> {
        Ok(())
    }
}
