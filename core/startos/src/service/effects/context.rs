use std::sync::{Arc, Weak};

use rpc_toolkit::Context;

use crate::prelude::*;
use crate::service::Service;

#[derive(Clone)]
pub(in crate::service) struct EffectContext(Weak<Service>);
impl EffectContext {
    pub fn new(service: Weak<Service>) -> Self {
        Self(service)
    }
}
impl Context for EffectContext {}
impl EffectContext {
    pub(super) fn deref(&self) -> Result<Arc<Service>, Error> {
        if let Some(seed) = Weak::upgrade(&self.0) {
            Ok(seed)
        } else {
            Err(Error::new(
                eyre!("Service has already been destroyed"),
                ErrorKind::InvalidRequest,
            ))
        }
    }
}
