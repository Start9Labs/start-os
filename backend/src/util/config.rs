use std::fs::File;
use std::path::{Path, PathBuf};

use serde::Deserialize;
use serde_json::Value;

use crate::util::serde::IoFormat;
use crate::{Config, Error, ResultExt};

pub const DEVICE_CONFIG_PATH: &str = "/media/embassy/config/config.yaml";
pub const CONFIG_PATH: &str = "/etc/embassy/config.yaml";
pub const CONFIG_PATH_LOCAL: &str = ".embassy/config.yaml";

pub fn local_config_path() -> Option<PathBuf> {
    if let Ok(home) = std::env::var("HOME") {
        Some(Path::new(&home).join(CONFIG_PATH_LOCAL))
    } else {
        None
    }
}

/// BLOCKING
pub fn load_config_from_paths<'a, T: for<'de> Deserialize<'de>>(
    paths: impl IntoIterator<Item = impl AsRef<Path>>,
) -> Result<T, Error> {
    let mut config = Default::default();
    for path in paths {
        if path.as_ref().exists() {
            let format: IoFormat = path
                .as_ref()
                .extension()
                .and_then(|s| s.to_str())
                .map(|f| f.parse())
                .transpose()?
                .unwrap_or_default();
            let new = format.from_reader(File::open(path)?)?;
            config = merge_configs(config, new);
        }
    }
    serde_json::from_value(Value::Object(config)).with_kind(crate::ErrorKind::Deserialization)
}

pub fn merge_configs(mut first: Config, second: Config) -> Config {
    for (k, v) in second.into_iter() {
        let new = match first.remove(&k) {
            None => v,
            Some(old) => match (old, v) {
                (Value::Object(first), Value::Object(second)) => {
                    Value::Object(merge_configs(first, second))
                }
                (first, _) => first,
            },
        };
        first.insert(k, new);
    }
    first
}
