use std::borrow::Cow;
use std::path::Path;

use beau_collector::BeauCollector;
use embassy::config::action::SetResult;
use embassy::config::spec;
use itertools::Itertools;
use linear_map::LinearMap;
use linear_map::set::LinearSet;
use regex::Regex;

pub mod rules;
pub mod util;
pub mod value;

pub use rules::{ConfigRuleEntry, ConfigRuleEntryWithSuggestions};
use util::NumRange;
pub use value::Config;

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

pub fn set_configuration(
    name: &str,
    config: Config,
    rules_path: &Path,
    config_path: &Path,
) -> Result<SetResult, anyhow::Error> {
    let rules: Vec<ConfigRuleEntry> = serde_yaml::from_reader(std::fs::File::open(rules_path)?)?;
    let mut cfgs = LinearMap::new();
    cfgs.insert(name, Cow::Borrowed(&config));
    let rule_check= rules
        .into_iter()
        .map(|r|r.check(&config, &cfgs))
        .bcollect::<Vec<_>>();
    match rule_check {
        Ok(_) => {
            // create temp config file
            let temp = std::fs::File::create("config_temp.yaml")?;
            // copy new config that pass rule check into temp file
            serde_yaml::to_writer(temp, &config)?;
            std::fs::copy("config_temp.yaml", config_path)?;
            // return set result
            Ok(SetResult {
                depends_on: indexmap::IndexMap::new(),
                // sending sigterm so service is restarted - in 0.3.x services, this is whatever signal is needed to send to the process to pick up the configuration
                signal: Some(nix::sys::signal::SIGTERM)
            })
        }
        Err(e) => Err(e)
    }
}