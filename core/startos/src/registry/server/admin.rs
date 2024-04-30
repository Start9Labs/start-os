use rpc_toolkit::ParentHandler;

use crate::context::RegistryContext;

pub fn admin_api() -> ParentHandler {
    ParentHandler::new()
}
