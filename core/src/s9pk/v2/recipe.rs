use std::collections::BTreeMap;
use std::path::PathBuf;

use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
pub struct DirRecipe(BTreeMap<PathBuf, Recipe>);

#[derive(Debug, Clone, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub enum Recipe {
    Make(PathBuf),
    Wget {
        #[ts(type = "string")]
        url: Url,
        checksum: String,
    },
    Recipe(DirRecipe),
}
