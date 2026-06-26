use std::sync::Arc;

use tokio::runtime::Runtime;

pub trait Context: Send + Sync + 'static {
    fn runtime(&self) -> Option<Arc<Runtime>> {
        None
    }
}
