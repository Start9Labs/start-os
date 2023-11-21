use models::ErrorKind;

use crate::context::RpcContext;
use crate::s9pk::S9pk;
use crate::util::docker::stop_container;
use crate::Error;

/// This is helper structure for a service, the seed of the data that is needed for the manager_container
pub struct ManagerSeed {
    pub ctx: RpcContext,
    pub s9pk: S9pk,
}

impl ManagerSeed {
    pub async fn stop_container(&self) -> Result<(), Error> {
        match stop_container(
            todo!(),
            None, // TODO
            None,
        )
        .await
        {
            Err(e) if e.kind == ErrorKind::NotFound => (), // Already stopped
            a => a?,
        }
        Ok(())
    }
}
