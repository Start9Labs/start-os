use std::borrow::Cow;
use std::collections::BTreeMap;
use std::path::Path;

use beau_collector::BeauCollector;
use embassy::config::action::SetResult;
use embassy::config::{Config, spec};
use linear_map::LinearMap;

pub mod rules;

use anyhow::anyhow;
pub use rules::{ConfigRuleEntry, ConfigRuleEntryWithSuggestions};

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
            serde_yaml::to_writer(std::fs::File::create(config_path.with_extension("tmp"))?, &config)?;
            std::fs::rename(config_path.with_extension("tmp"), config_path)?;
            // return set result
            Ok(SetResult {
                depends_on: BTreeMap::new(),
                // sending sigterm so service is restarted - in 0.3.x services, this is whatever signal is needed to send to the process to pick up the configuration
                signal: Some(nix::sys::signal::SIGTERM),
            })
        }
        Err(e) => Err(anyhow!("{}", e))
    }
}

pub fn validate_dependency_configuration(
    name: &str,
    config: Config,
    parent_name: &str,
    parent_config: Config,
    rules_path: &Path,
) -> Result<(), anyhow::Error> {
    let rules: Vec<ConfigRuleEntry> = serde_yaml::from_reader(std::fs::File::open(rules_path)?)?;
    let mut cfgs = LinearMap::new();
    cfgs.insert(parent_name, Cow::Borrowed(&parent_config));
    cfgs.insert(name, Cow::Borrowed(&config));
    let rule_check = rules
        .into_iter()
        .map(|r| r.check(&parent_config, &cfgs))
        .bcollect::<Vec<_>>();
    match rule_check {
        Ok(_) => Ok(()),
        Err(e) => Err(anyhow!("{}", e))
    }
}

pub fn apply_dependency_configuration(
    name: &str,
    config: Config,
    parent_name: &str,
    mut parent_config: Config,
    rules_path: &Path,
) -> Result<Config, anyhow::Error> {
    let rules: Vec<ConfigRuleEntryWithSuggestions> =
        serde_yaml::from_reader(std::fs::File::open(rules_path)?)?;
    let mut cfgs = LinearMap::new();
    cfgs.insert(parent_name, Cow::Owned(parent_config.clone()));
    cfgs.insert(name, Cow::Owned(config.clone()));
    let rule_check = rules
        .into_iter()
        .map(|r| r.apply(parent_name, &mut parent_config, &mut cfgs))
        .bcollect::<Vec<_>>();
    match rule_check {
        Ok(_) => Ok(parent_config),
        Err(e) => Err(anyhow!("{}", e)),
    }
}
