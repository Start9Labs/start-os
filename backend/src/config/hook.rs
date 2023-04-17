use helpers::Callback;
use itertools::Itertools;
use jsonpath_lib::PathCompiled;
use models::PackageId;

use crate::context::RpcContext;
use crate::prelude::*;

pub struct ConfigHook {
    pub path: PathCompiled<'static>,
    pub prev: Vec<Value>,
    pub callback: Callback,
}

impl RpcContext {
    pub async fn add_config_hook(&self, id: PackageId, hook: ConfigHook) {
        let mut hooks = self.config_hooks.lock().await;
        let prev = hooks.remove(&id).unwrap_or_default();
        hooks.insert(
            id,
            prev.into_iter()
                .filter(|h| h.callback.is_listening())
                .chain(std::iter::once(hook))
                .collect(),
        );
    }

    pub async fn call_config_hooks(&self, id: PackageId, config: &Value) {
        let mut hooks = self.config_hooks.lock().await;
        let mut prev = hooks.remove(&id).unwrap_or_default();
        for hook in &mut prev {
            let new = hook
                .path
                .select(config)
                .unwrap_or_default()
                .into_iter()
                .cloned()
                .collect_vec();
            if new != hook.prev {
                hook.callback
                    .call(imbl::vector![Value::Array(new.clone())])
                    .unwrap_or_default();
                hook.prev = new;
            }
        }
        hooks.insert(
            id,
            prev.into_iter()
                .filter(|h| h.callback.is_listening())
                .collect(),
        );
    }
}
