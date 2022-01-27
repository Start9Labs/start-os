use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::path::Path;

use beau_collector::BeauCollector;
use embassy::config::action::SetResult;
use embassy::config::{spec, Config};
use embassy::s9pk::manifest::PackageId;
use embassy::status::health_check::HealthCheckId;
use linear_map::LinearMap;

pub mod rules;

use anyhow::anyhow;
pub use rules::{ConfigRuleEntry, ConfigRuleEntryWithSuggestions};

use self::rules::ConfigRule;
pub type DepInfo = HashMap<PackageId, DepRuleInfo>;
#[derive(Clone, Debug, serde::Deserialize)]
pub struct DepRuleInfo {
    condition: ConfigRule,
    health_checks: BTreeSet<HealthCheckId>,
}

pub fn validate_configuration(
    name: &str,
    config: Config,
    rules_path: &Path,
    config_path: &Path,
    maybe_deps_path: Option<&str>,
) -> Result<SetResult, anyhow::Error> {
    let rules: Vec<ConfigRuleEntry> = serde_yaml::from_reader(std::fs::File::open(rules_path)?)?;
    let mut cfgs = LinearMap::new();
    cfgs.insert(name, Cow::Borrowed(&config));

    let mut depends_on = BTreeMap::new();
    if let Some(deps_path) = maybe_deps_path.map(Path::new) {
        if deps_path.exists() {
            let deps: DepInfo = serde_yaml::from_reader(std::fs::File::open(deps_path)?)?;
            // check if new config is set to depend on any optional dependencies
            depends_on.extend(
                deps.into_iter()
                    .filter(|(_, data)| (data.condition.compiled)(&config, &cfgs))
                    .map(|(pkg_id, data)| (pkg_id, data.health_checks)),
            );
        };
    }

    // check that all configuration rules
    let rule_check = rules
        .into_iter()
        .map(|r| r.check(&config, &cfgs))
        .bcollect::<Vec<_>>();
    match rule_check {
        Ok(_) => {
            // create temp config file
            serde_yaml::to_writer(
                std::fs::File::create(config_path.with_extension("tmp"))?,
                &config,
            )?;
            std::fs::rename(config_path.with_extension("tmp"), config_path)?;
            // return set result
            Ok(SetResult {
                depends_on,
                // sending sigterm so service is restarted - in 0.3.x services, this is whatever signal is needed to send to the process to pick up the configuration
                signal: Some(nix::sys::signal::SIGTERM),
            })
        }
        Err(e) => Err(anyhow!("{}", e)),
    }
}

pub fn validate_dependency_configuration(
    name: &str,
    config: &Option<Config>,
    parent_name: &str,
    parent_config: Config,
    rules_path: &Path,
) -> Result<(), anyhow::Error> {
    let rules: Vec<ConfigRuleEntry> = serde_yaml::from_reader(std::fs::File::open(rules_path)?)?;
    let mut cfgs = LinearMap::new();
    cfgs.insert(parent_name, Cow::Borrowed(&parent_config));
    if let Some(config) = config {
        cfgs.insert(name, Cow::Borrowed(&config))
    } else {
        cfgs.insert(name, Cow::Owned(serde_json::Map::new()))
    };
    let rule_check = rules
        .into_iter()
        .map(|r| r.check(&parent_config, &cfgs))
        .bcollect::<Vec<_>>();
    match rule_check {
        Ok(_) => Ok(()),
        Err(e) => Err(anyhow!("{}", e)),
    }
}

pub fn apply_dependency_configuration(
    package_id: &str,
    config: Option<Config>,
    dependency_id: &str,
    mut dep_config: Config,
    rules_path: &Path,
) -> Result<Config, anyhow::Error> {
    let rules: Vec<ConfigRuleEntryWithSuggestions> =
        serde_yaml::from_reader(std::fs::File::open(rules_path)?)?;
    let mut cfgs = LinearMap::new();
    cfgs.insert(dependency_id, Cow::Owned(dep_config.clone()));
    match config {
        Some(config) => cfgs.insert(package_id, Cow::Owned(config.clone())),
        None => cfgs.insert(package_id, Cow::Owned(serde_json::Map::new())),
    };
    let rule_check = rules
        .into_iter()
        .map(|r| r.apply(dependency_id, &mut dep_config, &mut cfgs))
        .bcollect::<Vec<_>>();
    match rule_check {
        Ok(_) => Ok(dep_config),
        Err(e) => Err(anyhow!("{}", e)),
    }
}
