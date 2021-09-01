use std::borrow::Cow;
use std::path::Path;
use std::time::Duration;

use embassy::config::action::SetResult;
use futures::future::{BoxFuture, FutureExt};
use itertools::Itertools;
use linear_map::{set::LinearSet, LinearMap};
use regex::Regex;

use embassy::dependencies::{DependencyError, TaggedDependencyError};
use embassy::util::{from_yaml_async_reader, to_yaml_async_writer};
use embassy::ResultExt as _;

pub mod rules;
pub mod util;
pub mod value;

pub use rules::{ConfigRuleEntry, ConfigRuleEntryWithSuggestions};
use util::NumRange;
pub use value::Config;

use embassy::config::spec;

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

pub async fn set_configuration(
    name: &str,
    config: Config,
) -> Result<SetResult, crate::Error> {
    fn configure_rec<'a>(
        name: &'a str,
        config: Config,
        res: &'a mut SetResult,
    ) -> BoxFuture<'a, Result<Config, crate::Error>> {
        async move {
            let rules_path = Path::new("apps")
                .join(name)
                .join("config_rules.yaml");
            let config_path = Path::new("apps")
                .join(name)
                .join("config.yaml");

            let rules: Vec<ConfigRuleEntry> =
                from_yaml_async_reader(&mut tokio::fs::File::open(rules_path).await?).await?;

            let mut cfgs = LinearMap::new();
            cfgs.insert(name, Cow::Borrowed(&config));
            for rule in rules {
                rule.check(&config, &cfgs)
                    .with_kind(embassy::ErrorKind::ConfigRulesViolation)?;
            }

            // res.depends_on.new();
            
            Ok(config)
        }
        .boxed()
    }
    let mut res = SetResult {
        depends_on: indexmap::IndexMap::new(),
        signal: None
    };
    configure_rec(name, config, &mut res).await?;
    // Ok(res);
}
