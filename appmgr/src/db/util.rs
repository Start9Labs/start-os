use std::sync::Arc;

use patch_db::Revision;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct WithRevision<T> {
    pub response: T,
    pub revision: Arc<Revision>,
}
