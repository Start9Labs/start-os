use std::borrow::Cow;
use std::path::Path;
use std::time::Duration;

use failure::ResultExt as _;
use futures::future::{BoxFuture, FutureExt};
use itertools::Itertools;
use linear_map::{set::LinearSet, LinearMap};
use rand::SeedableRng;
use regex::Regex;

use crate::dependencies::{DependencyError, TaggedDependencyError};
use crate::util::PersistencePath;
use crate::util::{from_yaml_async_reader, to_yaml_async_writer};
use crate::ResultExt as _;

pub mod rules;
pub mod spec;
pub mod util;
pub mod value;

pub use rules::{ConfigRuleEntry, ConfigRuleEntryWithSuggestions};
pub use spec::{ConfigSpec, Defaultable};
use util::NumRange;
pub use value::Config;

#[derive(Debug, Fail)]
pub enum ConfigurationError {
    #[fail(display = "Timeout Error")]
    TimeoutError,
    #[fail(display = "No Match: {}", _0)]
    NoMatch(NoMatchWithPath),
    #[fail(display = "Invalid Variant: {}", _0)]
    InvalidVariant(String),
    #[fail(display = "System Error: {}", _0)]
    SystemError(crate::Error),
}
impl From<TimeoutError> for ConfigurationError {
    fn from(_: TimeoutError) -> Self {
        ConfigurationError::TimeoutError
    }
}
impl From<NoMatchWithPath> for ConfigurationError {
    fn from(e: NoMatchWithPath) -> Self {
        ConfigurationError::NoMatch(e)
    }
}

#[derive(Clone, Copy, Debug, Fail)]
#[fail(display = "Timeout Error")]
pub struct TimeoutError;

#[derive(Clone, Debug, Fail)]
pub struct NoMatchWithPath {
    pub path: Vec<String>,
    pub error: MatchError,
}
impl NoMatchWithPath {
    pub fn new(error: MatchError) -> Self {
        NoMatchWithPath {
            path: Vec::new(),
            error,
        }
    }
    pub fn prepend(mut self, seg: String) -> Self {
        self.path.push(seg);
        self
    }
}
impl std::fmt::Display for NoMatchWithPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.path.iter().rev().join("."), self.error)
    }
}

#[derive(Clone, Debug, Fail)]
pub enum MatchError {
    #[fail(display = "String {:?} Does Not Match Pattern {}", _0, _1)]
    Pattern(String, Regex),
    #[fail(display = "String {:?} Is Not In Enum {:?}", _0, _1)]
    Enum(String, LinearSet<String>),
    #[fail(display = "Field Is Not Nullable")]
    NotNullable,
    #[fail(display = "Length Mismatch: expected {}, actual: {}", _0, _1)]
    LengthMismatch(NumRange<usize>, usize),
    #[fail(display = "Invalid Type: expected {}, actual: {}", _0, _1)]
    InvalidType(&'static str, &'static str),
    #[fail(display = "Number Out Of Range: expected {}, actual: {}", _0, _1)]
    OutOfRange(NumRange<f64>, f64),
    #[fail(display = "Number Is Not Integral: {}", _0)]
    NonIntegral(f64),
    #[fail(display = "Variant {:?} Is Not In Union {:?}", _0, _1)]
    Union(String, LinearSet<String>),
    #[fail(display = "Variant Is Missing Tag {:?}", _0)]
    MissingTag(String),
    #[fail(
        display = "Property {:?} Of Variant {:?} Conflicts With Union Tag",
        _0, _1
    )]
    PropertyMatchesUnionTag(String, String),
    #[fail(display = "Name of Property {:?} Conflicts With Map Tag Name", _0)]
    PropertyNameMatchesMapTag(String),
    #[fail(display = "Pointer Is Invalid: {}", _0)]
    InvalidPointer(spec::ValueSpecPointer),
    #[fail(display = "Object Key Is Invalid: {}", _0)]
    InvalidKey(String),
    #[fail(display = "Value In List Is Not Unique")]
    ListUniquenessViolation,
}

#[derive(Clone, Debug, Default, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct ConfigurationRes {
    pub changed: LinearMap<String, Config>,
    pub needs_restart: LinearSet<String>,
    pub stopped: LinearMap<String, TaggedDependencyError>,
}

// returns apps with changed configurations
pub async fn configure(
    name: &str,
    config: Option<Config>,
    timeout: Option<Duration>,
    dry_run: bool,
) -> Result<ConfigurationRes, crate::Error> {
    async fn handle_broken_dependent(
        name: &str,
        dependent: String,
        dry_run: bool,
        res: &mut ConfigurationRes,
        error: DependencyError,
    ) -> Result<(), crate::Error> {
        crate::control::stop_dependents(
            &dependent,
            dry_run,
            DependencyError::NotRunning,
            &mut res.stopped,
        )
        .await?;
        if crate::apps::status(&dependent).await?.status != crate::apps::DockerStatus::Stopped {
            crate::control::stop_app(&dependent, false, dry_run).await?;
            res.stopped.insert(
                // TODO: maybe don't do this if its not running
                dependent,
                TaggedDependencyError {
                    dependency: name.to_owned(),
                    error,
                },
            );
        }
        Ok(())
    }
    fn configure_rec<'a>(
        name: &'a str,
        config: Option<Config>,
        timeout: Option<Duration>,
        dry_run: bool,
        res: &'a mut ConfigurationRes,
    ) -> BoxFuture<'a, Result<Config, crate::Error>> {
        async move {
            let info = crate::apps::list_info()
                .await?
                .remove(name)
                .ok_or_else(|| failure::format_err!("{} is not installed", name))
                .with_code(crate::error::NOT_FOUND)?;
            let mut rng = rand::rngs::StdRng::from_entropy();
            let spec_path = PersistencePath::from_ref("apps")
                .join(name)
                .join("config_spec.yaml");
            let rules_path = PersistencePath::from_ref("apps")
                .join(name)
                .join("config_rules.yaml");
            let config_path = PersistencePath::from_ref("apps")
                .join(name)
                .join("config.yaml");
            let spec: ConfigSpec =
                from_yaml_async_reader(&mut *spec_path.read(false).await?).await?;
            let rules: Vec<ConfigRuleEntry> =
                from_yaml_async_reader(&mut *rules_path.read(false).await?).await?;
            let old_config: Option<Config> =
                if let Some(mut f) = config_path.maybe_read(false).await.transpose()? {
                    Some(from_yaml_async_reader(&mut *f).await?)
                } else {
                    None
                };
            let mut config = if let Some(cfg) = config {
                cfg
            } else {
                if let Some(old) = &old_config {
                    old.clone()
                } else {
                    spec.gen(&mut rng, &timeout)
                        .with_code(crate::error::CFG_SPEC_VIOLATION)?
                }
            };
            spec.matches(&config)
                .with_code(crate::error::CFG_SPEC_VIOLATION)?;
            spec.update(&mut config)
                .await
                .with_code(crate::error::CFG_SPEC_VIOLATION)?;
            let mut cfgs = LinearMap::new();
            cfgs.insert(name, Cow::Borrowed(&config));
            for rule in rules {
                rule.check(&config, &cfgs)
                    .with_code(crate::error::CFG_RULES_VIOLATION)?;
            }
            match old_config {
                Some(old) if &old == &config && info.configured && !info.recoverable => {
                    return Ok(config)
                }
                _ => (),
            };
            res.changed.insert(name.to_owned(), config.clone());
            for dependent in crate::apps::dependents(name, false).await? {
                match configure_rec(&dependent, None, timeout, dry_run, res).await {
                    Ok(dependent_config) => {
                        let man = crate::apps::manifest(&dependent).await?;
                        if let Some(dep_info) = man.dependencies.0.get(name) {
                            match dep_info
                                .satisfied(
                                    name,
                                    Some(config.clone()),
                                    &dependent,
                                    &dependent_config,
                                )
                                .await?
                            {
                                Ok(_) => (),
                                Err(e) => {
                                    handle_broken_dependent(name, dependent, dry_run, res, e)
                                        .await?;
                                }
                            }
                        }
                    }
                    Err(e) => {
                        if e.code == Some(crate::error::CFG_RULES_VIOLATION)
                            || e.code == Some(crate::error::CFG_SPEC_VIOLATION)
                        {
                            if !dry_run {
                                crate::apps::set_configured(&dependent, false).await?;
                            }
                            handle_broken_dependent(
                                name,
                                dependent,
                                dry_run,
                                res,
                                DependencyError::PointerUpdateError(format!("{}", e)),
                            )
                            .await?;
                        } else {
                            handle_broken_dependent(
                                name,
                                dependent,
                                dry_run,
                                res,
                                DependencyError::Other(format!("{}", e)),
                            )
                            .await?;
                        }
                    }
                }
            }
            if !dry_run {
                let mut file = config_path.write(None).await?;
                to_yaml_async_writer(file.as_mut(), &config).await?;
                file.commit().await?;
                let volume_config = Path::new(crate::VOLUMES)
                    .join(name)
                    .join("start9")
                    .join("config.yaml");
                tokio::fs::copy(config_path.path(), &volume_config)
                    .await
                    .with_context(|e| {
                        format!(
                            "{}: {} -> {}",
                            e,
                            config_path.path().display(),
                            volume_config.display()
                        )
                    })
                    .with_code(crate::error::FILESYSTEM_ERROR)?;
                crate::apps::set_configured(name, true).await?;
                crate::apps::set_recoverable(name, false).await?;
            }
            if crate::apps::status(name).await?.status != crate::apps::DockerStatus::Stopped {
                if !dry_run {
                    crate::apps::set_needs_restart(name, true).await?;
                }
                res.needs_restart.insert(name.to_string());
            }
            Ok(config)
        }
        .boxed()
    }
    let mut res = ConfigurationRes::default();
    configure_rec(name, config, timeout, dry_run, &mut res).await?;
    Ok(res)
}

pub async fn remove(name: &str) -> Result<(), crate::Error> {
    let config_path = PersistencePath::from_ref("apps")
        .join(name)
        .join("config.yaml")
        .path();
    if config_path.exists() {
        tokio::fs::remove_file(&config_path)
            .await
            .with_context(|e| format!("{}: {}", e, config_path.display()))
            .with_code(crate::error::FILESYSTEM_ERROR)?;
    }
    let volume_config = Path::new(crate::VOLUMES)
        .join(name)
        .join("start9")
        .join("config.yaml");
    if volume_config.exists() {
        tokio::fs::remove_file(&volume_config)
            .await
            .with_context(|e| format!("{}: {}", e, volume_config.display()))
            .with_code(crate::error::FILESYSTEM_ERROR)?;
    }
    crate::apps::set_configured(name, false).await?;
    Ok(())
}
