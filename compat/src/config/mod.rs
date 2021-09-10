use std::borrow::Cow;
use std::path::Path;

use beau_collector::BeauCollector;
use embassy::config::action::SetResult;
use embassy::config::spec;
use itertools::Itertools;
use linear_map::set::LinearSet;
use linear_map::LinearMap;
use regex::Regex;

pub mod rules;
pub mod util;
pub mod value;

pub use rules::{ConfigRuleEntry, ConfigRuleEntryWithSuggestions};
pub use value::Config;

pub fn validate_configuration(
    name: &str,
    config: Config,
    rules_path: &Path,
    config_path: &Path,
) -> Result<SetResult, anyhow::Error> {
    let rules: Vec<ConfigRuleEntry> = serde_yaml::from_reader(std::fs::File::open(rules_path)?)?;
    let mut cfgs = LinearMap::new();
    cfgs.insert(name, Cow::Borrowed(&config));
    let rule_check = rules
        .into_iter()
        .map(|r| r.check(&config, &cfgs))
        .bcollect::<Vec<_>>();
    match rule_check {
        Ok(_) => {
            // create temp config file
            let temp = std::fs::File::create("config_temp.yaml")?;
            // copy new config that pass rule check into temp file
            serde_yaml::to_writer(temp, &config)?;
            std::fs::copy("config_temp.yaml", config_path)?;
            std::fs::remove_file("config_temp.yaml")?;
            // return set result
            Ok(SetResult {
                depends_on: indexmap::IndexMap::new(),
                // sending sigterm so service is restarted - in 0.3.x services, this is whatever signal is needed to send to the process to pick up the configuration
                signal: Some(nix::sys::signal::SIGTERM),
            })
        }
        Err(e) => Err(e),
    }
}