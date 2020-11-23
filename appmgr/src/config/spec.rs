use std::borrow::{Borrow, Cow};
use std::fmt;
use std::fmt::Debug;
use std::ops::RangeBounds;
use std::sync::Arc;
use std::time::Duration;

use async_trait::async_trait;
use itertools::Itertools;
use linear_map::{set::LinearSet, LinearMap};
use rand::{CryptoRng, Rng};
use regex::Regex;

use super::util::{self, CharSet, NumRange, UniqueBy, STATIC_NULL};
use super::value::{Config, Value};
use super::{MatchError, NoMatchWithPath, TimeoutError};

use crate::config::ConfigurationError;
use crate::manifest::ManifestLatest;
use crate::util::PersistencePath;

// Config Value Specifications
#[async_trait]
pub trait ValueSpec {
    // This function defines whether the value supplied in the argument is
    // consistent with the spec in &self
    fn matches(&self, value: &Value) -> Result<(), NoMatchWithPath>;
    // This function checks whether the value spec is consistent with itself,
    // since not all invariants can be checked by the type
    fn validate(&self, manifest: &ManifestLatest) -> Result<(), NoMatchWithPath>;
    // update is to fill in values for environment pointers recursively
    async fn update(&self, value: &mut Value) -> Result<(), ConfigurationError>;
    // requires returns whether the app id is the target of a pointer within it
    fn requires(&self, id: &str, value: &Value) -> bool;
    // defines if 2 values of this type are equal for the purpose of uniqueness
    fn eq(&self, lhs: &Value, rhs: &Value) -> bool;
}

// Config Value Default Generation
//
// This behavior is defined by two independent traits as well as a third that
// represents a conjunction of those two traits:
//
// DefaultableWith - defines an associated type describing the information it
// needs to be able to generate a default value, as well as a function for
// extracting relevant pieces of that information and using it to actually
// generate the default value
//
// HasDefaultSpec - only purpose is to summon the default spec for the type
//
// Defaultable - this is a redundant trait that may replace 'DefaultableWith'
// and 'HasDefaultSpec'.
pub trait DefaultableWith {
    type DefaultSpec: Sync;
    type Error: failure::Fail;

    fn gen_with<R: Rng + CryptoRng + Sync + Send + Sync + Send>(
        &self,
        spec: &Self::DefaultSpec,
        rng: &mut R,
        timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error>;
}
pub trait HasDefaultSpec: DefaultableWith {
    fn default_spec(&self) -> &Self::DefaultSpec;
}

pub trait Defaultable {
    type Error;

    fn gen<R: Rng + CryptoRng + Sync + Send + Sync + Send>(
        &self,
        rng: &mut R,
        timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error>;
}
impl<T, E> Defaultable for T
where
    T: HasDefaultSpec + DefaultableWith<Error = E> + Sync,
    E: failure::Fail,
{
    type Error = E;

    fn gen<R: Rng + CryptoRng + Sync + Send + Sync + Send>(
        &self,
        rng: &mut R,
        timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error> {
        self.gen_with(self.default_spec().borrow(), rng, timeout)
    }
}

// WithDefault - trivial wrapper that pairs a 'DefaultableWith' type with a
// default spec
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct WithDefault<T: DefaultableWith> {
    #[serde(flatten)]
    pub inner: T,
    pub default: T::DefaultSpec,
}
impl<T> DefaultableWith for WithDefault<T>
where
    T: DefaultableWith + Sync + Send,
    T::DefaultSpec: Send,
{
    type DefaultSpec = T::DefaultSpec;
    type Error = T::Error;

    fn gen_with<R: Rng + CryptoRng + Sync + Send + Sync + Send>(
        &self,
        spec: &Self::DefaultSpec,
        rng: &mut R,
        timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error> {
        self.inner.gen_with(spec, rng, timeout)
    }
}
impl<T> HasDefaultSpec for WithDefault<T>
where
    T: DefaultableWith + Sync + Send,
    T::DefaultSpec: Send,
{
    fn default_spec(&self) -> &Self::DefaultSpec {
        &self.default
    }
}
#[async_trait]
impl<T> ValueSpec for WithDefault<T>
where
    T: ValueSpec + DefaultableWith + Send + Sync,
    Self: Send + Sync,
{
    fn matches(&self, value: &Value) -> Result<(), NoMatchWithPath> {
        self.inner.matches(value)
    }
    fn validate(&self, manifest: &ManifestLatest) -> Result<(), NoMatchWithPath> {
        self.inner.validate(manifest)
    }
    async fn update(&self, value: &mut Value) -> Result<(), ConfigurationError> {
        self.inner.update(value).await
    }
    fn requires(&self, id: &str, value: &Value) -> bool {
        self.inner.requires(id, value)
    }
    fn eq(&self, lhs: &Value, rhs: &Value) -> bool {
        self.inner.eq(lhs, rhs)
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct WithNullable<T> {
    #[serde(flatten)]
    pub inner: T,
    pub nullable: bool,
}
#[async_trait]
impl<T> ValueSpec for WithNullable<T>
where
    T: ValueSpec + Send + Sync,
    Self: Send + Sync,
{
    fn matches(&self, value: &Value) -> Result<(), NoMatchWithPath> {
        match (self.nullable, value) {
            (true, &Value::Null) => Ok(()),
            _ => self.inner.matches(value),
        }
    }
    fn validate(&self, manifest: &ManifestLatest) -> Result<(), NoMatchWithPath> {
        self.inner.validate(manifest)
    }
    async fn update(&self, value: &mut Value) -> Result<(), ConfigurationError> {
        self.inner.update(value).await
    }
    fn requires(&self, id: &str, value: &Value) -> bool {
        self.inner.requires(id, value)
    }
    fn eq(&self, lhs: &Value, rhs: &Value) -> bool {
        self.inner.eq(lhs, rhs)
    }
}

impl<T> DefaultableWith for WithNullable<T>
where
    T: DefaultableWith + Sync + Send,
{
    type DefaultSpec = T::DefaultSpec;
    type Error = T::Error;

    fn gen_with<R: Rng + CryptoRng + Sync + Send + Sync + Send>(
        &self,
        spec: &Self::DefaultSpec,
        rng: &mut R,
        timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error> {
        self.inner.gen_with(spec, rng, timeout)
    }
}

impl<T> Defaultable for WithNullable<T>
where
    T: Defaultable + Sync + Send,
{
    type Error = T::Error;

    fn gen<R: Rng + CryptoRng + Sync + Send + Sync + Send>(
        &self,
        rng: &mut R,
        timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error> {
        self.inner.gen(rng, timeout)
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WithDescription<T> {
    #[serde(flatten)]
    pub inner: T,
    pub description: Option<String>,
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub change_warning: Option<String>,
}
#[async_trait]
impl<T> ValueSpec for WithDescription<T>
where
    T: ValueSpec + Sync + Send,
    Self: Sync + Send,
{
    fn matches(&self, value: &Value) -> Result<(), NoMatchWithPath> {
        self.inner.matches(value)
    }
    fn validate(&self, manifest: &ManifestLatest) -> Result<(), NoMatchWithPath> {
        self.inner.validate(manifest)
    }
    async fn update(&self, value: &mut Value) -> Result<(), ConfigurationError> {
        self.inner.update(value).await
    }
    fn requires(&self, id: &str, value: &Value) -> bool {
        self.inner.requires(id, value)
    }
    fn eq(&self, lhs: &Value, rhs: &Value) -> bool {
        self.inner.eq(lhs, rhs)
    }
}

impl<T> DefaultableWith for WithDescription<T>
where
    T: DefaultableWith + Sync + Send,
{
    type DefaultSpec = T::DefaultSpec;
    type Error = T::Error;

    fn gen_with<R: Rng + CryptoRng + Sync + Send + Sync + Send>(
        &self,
        spec: &Self::DefaultSpec,
        rng: &mut R,
        timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error> {
        self.inner.gen_with(spec, rng, timeout)
    }
}

impl<T> Defaultable for WithDescription<T>
where
    T: Defaultable + Sync + Send,
{
    type Error = T::Error;

    fn gen<R: Rng + CryptoRng + Sync + Send + Sync + Send>(
        &self,
        rng: &mut R,
        timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error> {
        self.inner.gen(rng, timeout)
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
#[serde(tag = "type")]
pub enum ValueSpecAny {
    Boolean(WithDescription<WithDefault<ValueSpecBoolean>>),
    Enum(WithDescription<WithDefault<ValueSpecEnum>>),
    List(ValueSpecList),
    Number(WithDescription<WithDefault<WithNullable<ValueSpecNumber>>>),
    Object(WithDescription<WithNullable<ValueSpecObject>>),
    String(WithDescription<WithDefault<WithNullable<ValueSpecString>>>),
    Union(WithDescription<WithDefault<ValueSpecUnion>>),
    Pointer(WithDescription<ValueSpecPointer>),
}
impl ValueSpecAny {
    pub fn name<'a>(&'a self) -> &'a str {
        match self {
            ValueSpecAny::Boolean(b) => b.name.as_str(),
            ValueSpecAny::Enum(e) => e.name.as_str(),
            ValueSpecAny::List(l) => match l {
                ValueSpecList::Enum(e) => e.name.as_str(),
                ValueSpecList::Number(n) => n.name.as_str(),
                ValueSpecList::Object(o) => o.name.as_str(),
                ValueSpecList::String(s) => s.name.as_str(),
                ValueSpecList::Union(u) => u.name.as_str(),
            },
            ValueSpecAny::Number(n) => n.name.as_str(),
            ValueSpecAny::Object(o) => o.name.as_str(),
            ValueSpecAny::Pointer(p) => p.name.as_str(),
            ValueSpecAny::String(s) => s.name.as_str(),
            ValueSpecAny::Union(u) => u.name.as_str(),
        }
    }
}
#[async_trait]
impl ValueSpec for ValueSpecAny {
    fn matches(&self, value: &Value) -> Result<(), NoMatchWithPath> {
        match self {
            ValueSpecAny::Boolean(a) => a.matches(value),
            ValueSpecAny::Enum(a) => a.matches(value),
            ValueSpecAny::List(a) => a.matches(value),
            ValueSpecAny::Number(a) => a.matches(value),
            ValueSpecAny::Object(a) => a.matches(value),
            ValueSpecAny::String(a) => a.matches(value),
            ValueSpecAny::Union(a) => a.matches(value),
            ValueSpecAny::Pointer(a) => a.matches(value),
        }
    }
    fn validate(&self, manifest: &ManifestLatest) -> Result<(), NoMatchWithPath> {
        match self {
            ValueSpecAny::Boolean(a) => a.validate(manifest),
            ValueSpecAny::Enum(a) => a.validate(manifest),
            ValueSpecAny::List(a) => a.validate(manifest),
            ValueSpecAny::Number(a) => a.validate(manifest),
            ValueSpecAny::Object(a) => a.validate(manifest),
            ValueSpecAny::String(a) => a.validate(manifest),
            ValueSpecAny::Union(a) => a.validate(manifest),
            ValueSpecAny::Pointer(a) => a.validate(manifest),
        }
    }
    async fn update(&self, value: &mut Value) -> Result<(), ConfigurationError> {
        match self {
            ValueSpecAny::Boolean(a) => a.update(value).await,
            ValueSpecAny::Enum(a) => a.update(value).await,
            ValueSpecAny::List(a) => a.update(value).await,
            ValueSpecAny::Number(a) => a.update(value).await,
            ValueSpecAny::Object(a) => a.update(value).await,
            ValueSpecAny::String(a) => a.update(value).await,
            ValueSpecAny::Union(a) => a.update(value).await,
            ValueSpecAny::Pointer(a) => a.update(value).await,
        }
    }
    fn requires(&self, id: &str, value: &Value) -> bool {
        match self {
            ValueSpecAny::Boolean(a) => a.requires(id, value),
            ValueSpecAny::Enum(a) => a.requires(id, value),
            ValueSpecAny::List(a) => a.requires(id, value),
            ValueSpecAny::Number(a) => a.requires(id, value),
            ValueSpecAny::Object(a) => a.requires(id, value),
            ValueSpecAny::String(a) => a.requires(id, value),
            ValueSpecAny::Union(a) => a.requires(id, value),
            ValueSpecAny::Pointer(a) => a.requires(id, value),
        }
    }
    fn eq(&self, lhs: &Value, rhs: &Value) -> bool {
        match self {
            ValueSpecAny::Boolean(a) => a.eq(lhs, rhs),
            ValueSpecAny::Enum(a) => a.eq(lhs, rhs),
            ValueSpecAny::List(a) => a.eq(lhs, rhs),
            ValueSpecAny::Number(a) => a.eq(lhs, rhs),
            ValueSpecAny::Object(a) => a.eq(lhs, rhs),
            ValueSpecAny::String(a) => a.eq(lhs, rhs),
            ValueSpecAny::Union(a) => a.eq(lhs, rhs),
            ValueSpecAny::Pointer(a) => a.eq(lhs, rhs),
        }
    }
}
impl Defaultable for ValueSpecAny {
    type Error = ConfigurationError;

    fn gen<R: Rng + CryptoRng + Sync + Send + Sync + Send>(
        &self,
        rng: &mut R,
        timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error> {
        match self {
            ValueSpecAny::Boolean(a) => a.gen(rng, timeout).map_err(crate::util::absurd),
            ValueSpecAny::Enum(a) => a.gen(rng, timeout).map_err(crate::util::absurd),
            ValueSpecAny::List(a) => a.gen(rng, timeout),
            ValueSpecAny::Number(a) => a.gen(rng, timeout).map_err(crate::util::absurd),
            ValueSpecAny::Object(a) => a.gen(rng, timeout),
            ValueSpecAny::String(a) => a.gen(rng, timeout).map_err(ConfigurationError::from),
            ValueSpecAny::Union(a) => a.gen(rng, timeout),
            ValueSpecAny::Pointer(a) => a.gen(rng, timeout),
        }
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct ValueSpecBoolean {}
#[async_trait]
impl ValueSpec for ValueSpecBoolean {
    fn matches(&self, val: &Value) -> Result<(), NoMatchWithPath> {
        match val {
            Value::Bool(_) => Ok(()),
            Value::Null => Err(NoMatchWithPath::new(MatchError::NotNullable)),
            a => Err(NoMatchWithPath::new(MatchError::InvalidType(
                "boolean",
                a.type_of(),
            ))),
        }
    }
    fn validate(&self, _manifest: &ManifestLatest) -> Result<(), NoMatchWithPath> {
        Ok(())
    }
    async fn update(&self, _value: &mut Value) -> Result<(), ConfigurationError> {
        Ok(())
    }
    fn requires(&self, _id: &str, _value: &Value) -> bool {
        false
    }
    fn eq(&self, lhs: &Value, rhs: &Value) -> bool {
        match (lhs, rhs) {
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}
impl DefaultableWith for ValueSpecBoolean {
    type DefaultSpec = bool;
    type Error = crate::util::Never;

    fn gen_with<R: Rng + CryptoRng + Sync + Send + Send>(
        &self,
        spec: &Self::DefaultSpec,
        _rng: &mut R,
        _timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error> {
        Ok(Value::Bool(*spec))
    }
}

#[derive(Clone, Debug, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ValueSpecEnum {
    pub values: LinearSet<String>,
    pub value_names: LinearMap<String, String>,
}
impl<'de> serde::de::Deserialize<'de> for ValueSpecEnum {
    fn deserialize<D: serde::de::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(serde::Deserialize)]
        #[serde(rename_all = "camelCase")]
        pub struct _ValueSpecEnum {
            pub values: LinearSet<String>,
            #[serde(default)]
            pub value_names: LinearMap<String, String>,
        }

        let mut r#enum = _ValueSpecEnum::deserialize(deserializer)?;
        for name in &r#enum.values {
            if !r#enum.value_names.contains_key(name) {
                r#enum.value_names.insert(name.clone(), name.clone());
            }
        }
        Ok(ValueSpecEnum {
            values: r#enum.values,
            value_names: r#enum.value_names,
        })
    }
}
#[async_trait]
impl ValueSpec for ValueSpecEnum {
    fn matches(&self, val: &Value) -> Result<(), NoMatchWithPath> {
        match val {
            Value::String(b) => {
                if self.values.contains(b) {
                    Ok(())
                } else {
                    Err(NoMatchWithPath::new(MatchError::Enum(
                        b.clone(),
                        self.values.clone(),
                    )))
                }
            }
            Value::Null => Err(NoMatchWithPath::new(MatchError::NotNullable)),
            a => Err(NoMatchWithPath::new(MatchError::InvalidType(
                "string",
                a.type_of(),
            ))),
        }
    }
    fn validate(&self, _manifest: &ManifestLatest) -> Result<(), NoMatchWithPath> {
        Ok(())
    }
    async fn update(&self, _value: &mut Value) -> Result<(), ConfigurationError> {
        Ok(())
    }
    fn requires(&self, _id: &str, _value: &Value) -> bool {
        false
    }
    fn eq(&self, lhs: &Value, rhs: &Value) -> bool {
        match (lhs, rhs) {
            (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}
impl DefaultableWith for ValueSpecEnum {
    type DefaultSpec = String;
    type Error = crate::util::Never;

    fn gen_with<R: Rng + CryptoRng + Sync + Send + Send>(
        &self,
        spec: &Self::DefaultSpec,
        _rng: &mut R,
        _timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error> {
        Ok(Value::String(spec.clone()))
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct ListSpec<T> {
    pub spec: T,
    pub range: NumRange<usize>,
}
#[async_trait]
impl<T> ValueSpec for ListSpec<T>
where
    T: ValueSpec + Sync + Send,
    Self: Sync + Send,
{
    fn matches(&self, value: &Value) -> Result<(), NoMatchWithPath> {
        match value {
            Value::List(l) => {
                if !self.range.contains(&l.len()) {
                    Err(NoMatchWithPath {
                        path: Vec::new(),
                        error: MatchError::LengthMismatch(self.range.clone(), l.len()),
                    })
                } else {
                    l.iter()
                        .enumerate()
                        .map(|(i, v)| {
                            self.spec
                                .matches(v)
                                .map_err(|e| e.prepend(format!("{}", i)))?;
                            if l.iter()
                                .enumerate()
                                .any(|(i2, v2)| i != i2 && self.spec.eq(v, v2))
                            {
                                Err(NoMatchWithPath::new(MatchError::ListUniquenessViolation)
                                    .prepend(format!("{}", i)))
                            } else {
                                Ok(())
                            }
                        })
                        .collect()
                }
            }
            Value::Null => Err(NoMatchWithPath::new(MatchError::NotNullable)),
            a => Err(NoMatchWithPath::new(MatchError::InvalidType(
                "list",
                a.type_of(),
            ))),
        }
    }
    fn validate(&self, manifest: &ManifestLatest) -> Result<(), NoMatchWithPath> {
        self.spec.validate(manifest)
    }
    async fn update(&self, value: &mut Value) -> Result<(), ConfigurationError> {
        if let Value::List(ref mut ls) = value {
            for (i, val) in ls.into_iter().enumerate() {
                match self.spec.update(val).await {
                    Err(ConfigurationError::NoMatch(e)) => {
                        Err(ConfigurationError::NoMatch(e.prepend(format!("{}", i))))
                    }
                    a => a,
                }?;
            }
            Ok(())
        } else {
            Err(ConfigurationError::NoMatch(NoMatchWithPath::new(
                MatchError::InvalidType("list", value.type_of()),
            )))
        }
    }
    fn requires(&self, id: &str, value: &Value) -> bool {
        if let Value::List(ref ls) = value {
            ls.into_iter().any(|v| self.spec.requires(id, v))
        } else {
            false
        }
    }
    fn eq(&self, lhs: &Value, rhs: &Value) -> bool {
        match (lhs, rhs) {
            (Value::List(lhs), Value::List(rhs)) => {
                lhs.iter().zip_longest(rhs.iter()).all(|zip| match zip {
                    itertools::EitherOrBoth::Both(lhs, rhs) => lhs == rhs,
                    _ => false,
                })
            }
            _ => false,
        }
    }
}

impl<T> DefaultableWith for ListSpec<T>
where
    T: DefaultableWith + Sync + Send,
{
    type DefaultSpec = Vec<T::DefaultSpec>;
    type Error = T::Error;

    fn gen_with<R: Rng + CryptoRng + Sync + Send + Sync + Send>(
        &self,
        spec: &Self::DefaultSpec,
        rng: &mut R,
        timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error> {
        let mut res = Vec::new();
        for spec_member in spec.iter() {
            res.push(self.spec.gen_with(spec_member, rng, timeout)?);
        }
        Ok(Value::List(res))
    }
}

unsafe impl Sync for ValueSpecObject {} // TODO: remove
unsafe impl Send for ValueSpecObject {} // TODO: remove
unsafe impl Sync for ValueSpecUnion {} // TODO: remove
unsafe impl Send for ValueSpecUnion {} // TODO: remove

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
#[serde(tag = "subtype")]
pub enum ValueSpecList {
    Enum(WithDescription<WithDefault<ListSpec<ValueSpecEnum>>>),
    Number(WithDescription<WithDefault<ListSpec<ValueSpecNumber>>>),
    Object(WithDescription<WithDefault<ListSpec<ValueSpecObject>>>),
    String(WithDescription<WithDefault<ListSpec<ValueSpecString>>>),
    Union(WithDescription<WithDefault<ListSpec<WithDefault<ValueSpecUnion>>>>),
}
#[async_trait]
impl ValueSpec for ValueSpecList {
    fn matches(&self, value: &Value) -> Result<(), NoMatchWithPath> {
        match self {
            ValueSpecList::Enum(a) => a.matches(value),
            ValueSpecList::Number(a) => a.matches(value),
            ValueSpecList::Object(a) => a.matches(value),
            ValueSpecList::String(a) => a.matches(value),
            ValueSpecList::Union(a) => a.matches(value),
        }
    }
    fn validate(&self, manifest: &ManifestLatest) -> Result<(), NoMatchWithPath> {
        match self {
            ValueSpecList::Enum(a) => a.validate(manifest),
            ValueSpecList::Number(a) => a.validate(manifest),
            ValueSpecList::Object(a) => a.validate(manifest),
            ValueSpecList::String(a) => a.validate(manifest),
            ValueSpecList::Union(a) => a.validate(manifest),
        }
    }
    async fn update(&self, value: &mut Value) -> Result<(), ConfigurationError> {
        match self {
            ValueSpecList::Enum(a) => a.update(value).await,
            ValueSpecList::Number(a) => a.update(value).await,
            ValueSpecList::Object(a) => a.update(value).await,
            ValueSpecList::String(a) => a.update(value).await,
            ValueSpecList::Union(a) => a.update(value).await,
        }
    }
    fn requires(&self, id: &str, value: &Value) -> bool {
        match self {
            ValueSpecList::Enum(a) => a.requires(id, value),
            ValueSpecList::Number(a) => a.requires(id, value),
            ValueSpecList::Object(a) => a.requires(id, value),
            ValueSpecList::String(a) => a.requires(id, value),
            ValueSpecList::Union(a) => a.requires(id, value),
        }
    }
    fn eq(&self, lhs: &Value, rhs: &Value) -> bool {
        match self {
            ValueSpecList::Enum(a) => a.eq(lhs, rhs),
            ValueSpecList::Number(a) => a.eq(lhs, rhs),
            ValueSpecList::Object(a) => a.eq(lhs, rhs),
            ValueSpecList::String(a) => a.eq(lhs, rhs),
            ValueSpecList::Union(a) => a.eq(lhs, rhs),
        }
    }
}

impl Defaultable for ValueSpecList {
    type Error = ConfigurationError;

    fn gen<R: Rng + CryptoRng + Sync + Send + Sync + Send>(
        &self,
        rng: &mut R,
        timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error> {
        match self {
            ValueSpecList::Enum(a) => a.gen(rng, timeout).map_err(crate::util::absurd),
            ValueSpecList::Number(a) => a.gen(rng, timeout).map_err(crate::util::absurd),
            ValueSpecList::Object(a) => {
                let mut ret = match a.gen(rng, timeout).unwrap() {
                    Value::List(l) => l,
                    a => {
                        return Err(ConfigurationError::NoMatch(NoMatchWithPath::new(
                            MatchError::InvalidType("list", a.type_of()),
                        )))
                    }
                };
                while !(
                    a.inner.inner.range.start_bound(),
                    std::ops::Bound::Unbounded,
                )
                    .contains(&ret.len())
                {
                    ret.push(
                        a.inner
                            .inner
                            .spec
                            .gen(rng, timeout)
                            .map_err(ConfigurationError::from)?,
                    );
                }
                Ok(Value::List(ret))
            }
            ValueSpecList::String(a) => a.gen(rng, timeout).map_err(ConfigurationError::from),
            ValueSpecList::Union(a) => a.gen(rng, timeout).map_err(ConfigurationError::from),
        }
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct ValueSpecNumber {
    range: Option<NumRange<f64>>,
    #[serde(default)]
    integral: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    units: Option<String>,
}
#[async_trait]
impl ValueSpec for ValueSpecNumber {
    fn matches(&self, value: &Value) -> Result<(), NoMatchWithPath> {
        match value {
            Value::Number(n) => {
                if self.integral && n.floor() != *n {
                    return Err(NoMatchWithPath::new(MatchError::NonIntegral(*n)));
                }
                if let Some(range) = &self.range {
                    if !range.contains(n) {
                        return Err(NoMatchWithPath::new(MatchError::OutOfRange(
                            range.clone(),
                            *n,
                        )));
                    }
                }
                Ok(())
            }
            Value::Null => Err(NoMatchWithPath::new(MatchError::NotNullable)),
            a => Err(NoMatchWithPath::new(MatchError::InvalidType(
                "object",
                a.type_of(),
            ))),
        }
    }
    fn validate(&self, _manifest: &ManifestLatest) -> Result<(), NoMatchWithPath> {
        Ok(())
    }
    async fn update(&self, _value: &mut Value) -> Result<(), ConfigurationError> {
        Ok(())
    }
    fn requires(&self, _id: &str, _value: &Value) -> bool {
        false
    }
    fn eq(&self, lhs: &Value, rhs: &Value) -> bool {
        match (lhs, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}
#[derive(Clone, Copy, Debug, serde::Serialize)]
pub struct Number(pub f64);
impl<'de> serde::de::Deserialize<'de> for Number {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        use serde::de::*;
        struct NumberVisitor;
        impl<'de> Visitor<'de> for NumberVisitor {
            type Value = Number;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a number")
            }
            fn visit_i8<E: Error>(self, value: i8) -> Result<Self::Value, E> {
                Ok(Number(value.into()))
            }
            fn visit_i16<E: Error>(self, value: i16) -> Result<Self::Value, E> {
                Ok(Number(value.into()))
            }
            fn visit_i32<E: Error>(self, value: i32) -> Result<Self::Value, E> {
                Ok(Number(value.into()))
            }
            fn visit_i64<E: Error>(self, value: i64) -> Result<Self::Value, E> {
                Ok(Number(value as f64))
            }
            fn visit_u8<E: Error>(self, value: u8) -> Result<Self::Value, E> {
                Ok(Number(value.into()))
            }
            fn visit_u16<E: Error>(self, value: u16) -> Result<Self::Value, E> {
                Ok(Number(value.into()))
            }
            fn visit_u32<E: Error>(self, value: u32) -> Result<Self::Value, E> {
                Ok(Number(value.into()))
            }
            fn visit_u64<E: Error>(self, value: u64) -> Result<Self::Value, E> {
                Ok(Number(value as f64))
            }
            fn visit_f32<E: Error>(self, value: f32) -> Result<Self::Value, E> {
                Ok(Number(value.into()))
            }
            fn visit_f64<E: Error>(self, value: f64) -> Result<Self::Value, E> {
                Ok(Number(value))
            }
        }
        deserializer.deserialize_any(NumberVisitor)
    }
}
impl DefaultableWith for ValueSpecNumber {
    type DefaultSpec = Option<Number>;
    type Error = crate::util::Never;

    fn gen_with<R: Rng + CryptoRng + Sync + Send>(
        &self,
        spec: &Self::DefaultSpec,
        _rng: &mut R,
        _timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error> {
        Ok(spec.map(|s| Value::Number(s.0)).unwrap_or(Value::Null))
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ValueSpecObject {
    pub spec: ConfigSpec,
    #[serde(default)]
    pub null_by_default: bool,
    pub display_as: Option<String>,
    #[serde(default)]
    pub unique_by: UniqueBy,
}
#[async_trait]
impl ValueSpec for ValueSpecObject {
    fn matches(&self, value: &Value) -> Result<(), NoMatchWithPath> {
        match value {
            Value::Object(o) => self.spec.matches(o),
            Value::Null => Err(NoMatchWithPath::new(MatchError::NotNullable)),
            a => Err(NoMatchWithPath::new(MatchError::InvalidType(
                "object",
                a.type_of(),
            ))),
        }
    }
    fn validate(&self, manifest: &ManifestLatest) -> Result<(), NoMatchWithPath> {
        self.spec.validate(manifest)
    }
    async fn update(&self, value: &mut Value) -> Result<(), ConfigurationError> {
        if let Value::Object(o) = value {
            self.spec.update(o).await
        } else {
            Err(ConfigurationError::NoMatch(NoMatchWithPath::new(
                MatchError::InvalidType("object", value.type_of()),
            )))
        }
    }
    fn requires(&self, id: &str, value: &Value) -> bool {
        if let Value::Object(o) = value {
            self.spec.requires(id, o)
        } else {
            false
        }
    }
    fn eq(&self, lhs: &Value, rhs: &Value) -> bool {
        match (lhs, rhs) {
            (Value::Object(lhs), Value::Object(rhs)) => self.unique_by.eq(lhs, rhs),
            _ => false,
        }
    }
}
impl DefaultableWith for ValueSpecObject {
    type DefaultSpec = Config;
    type Error = crate::util::Never;

    fn gen_with<R: Rng + CryptoRng + Sync + Send>(
        &self,
        spec: &Self::DefaultSpec,
        _rng: &mut R,
        _timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error> {
        if self.null_by_default {
            Ok(Value::Null)
        } else {
            Ok(Value::Object(spec.clone()))
        }
    }
}
impl Defaultable for ValueSpecObject {
    type Error = ConfigurationError;

    fn gen<R: Rng + CryptoRng + Sync + Send>(
        &self,
        rng: &mut R,
        timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error> {
        if self.null_by_default {
            Ok(Value::Null)
        } else {
            self.spec.gen(rng, timeout).map(Value::Object)
        }
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct ConfigSpec(pub LinearMap<String, ValueSpecAny>);
impl ConfigSpec {
    pub fn matches(&self, value: &Config) -> Result<(), NoMatchWithPath> {
        for (key, val) in self.0.iter() {
            if let Some(v) = value.0.get(key) {
                val.matches(v).map_err(|e| e.prepend(key.clone()))?;
            } else {
                val.matches(&Value::Null)
                    .map_err(|e| e.prepend(key.clone()))?;
            }
        }
        Ok(())
    }

    pub fn gen<R: Rng + CryptoRng + Sync + Send>(
        &self,
        rng: &mut R,
        timeout: &Option<Duration>,
    ) -> Result<Config, ConfigurationError> {
        let mut res = LinearMap::new();
        for (key, val) in self.0.iter() {
            res.insert(key.clone(), val.gen(rng, timeout)?);
        }
        Ok(Config(res))
    }

    pub fn validate(&self, manifest: &ManifestLatest) -> Result<(), NoMatchWithPath> {
        for (name, val) in &self.0 {
            if let Err(_) = super::rules::validate_key(&name) {
                return Err(NoMatchWithPath::new(MatchError::InvalidKey(
                    name.to_owned(),
                )));
            }
            val.validate(manifest)
                .map_err(|e| e.prepend(name.clone()))?;
        }
        Ok(())
    }

    pub async fn update(&self, cfg: &mut Config) -> Result<(), ConfigurationError> {
        for (k, v) in cfg.0.iter_mut() {
            match self.0.get(k) {
                None => (),
                Some(vs) => match vs.update(v).await {
                    Err(ConfigurationError::NoMatch(e)) => {
                        Err(ConfigurationError::NoMatch(e.prepend(k.clone())))
                    }
                    a => a,
                }?,
            };
        }
        Ok(())
    }
    pub fn requires(&self, id: &str, cfg: &Config) -> bool {
        self.0
            .iter()
            .any(|(k, v)| v.requires(id, cfg.0.get(k).unwrap_or(&STATIC_NULL)))
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Pattern {
    #[serde(with = "util::serde_regex")]
    pub pattern: Regex,
    pub pattern_description: String,
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct ValueSpecString {
    #[serde(flatten)]
    pub pattern: Option<Pattern>,
    #[serde(default)]
    pub copyable: bool,
    #[serde(default)]
    pub masked: bool,
}
#[async_trait]
impl ValueSpec for ValueSpecString {
    fn matches(&self, value: &Value) -> Result<(), NoMatchWithPath> {
        match value {
            Value::String(s) => {
                if let Some(pattern) = &self.pattern {
                    if pattern.pattern.is_match(s) {
                        Ok(())
                    } else {
                        Err(NoMatchWithPath::new(MatchError::Pattern(
                            s.to_owned(),
                            pattern.pattern.clone(),
                        )))
                    }
                } else {
                    Ok(())
                }
            }
            Value::Null => Err(NoMatchWithPath::new(MatchError::NotNullable)),
            a => Err(NoMatchWithPath::new(MatchError::InvalidType(
                "string",
                a.type_of(),
            ))),
        }
    }
    fn validate(&self, _manifest: &ManifestLatest) -> Result<(), NoMatchWithPath> {
        Ok(())
    }
    async fn update(&self, _value: &mut Value) -> Result<(), ConfigurationError> {
        Ok(())
    }
    fn requires(&self, _id: &str, _value: &Value) -> bool {
        false
    }
    fn eq(&self, lhs: &Value, rhs: &Value) -> bool {
        match (lhs, rhs) {
            (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}
impl DefaultableWith for ValueSpecString {
    type DefaultSpec = Option<DefaultString>;
    type Error = TimeoutError;

    fn gen_with<R: Rng + CryptoRng + Sync + Send + Sync + Send>(
        &self,
        spec: &Self::DefaultSpec,
        rng: &mut R,
        timeout: &Option<Duration>,
    ) -> Result<Value, TimeoutError> {
        if let Some(spec) = spec {
            let now = timeout.as_ref().map(|_| std::time::Instant::now());
            loop {
                let candidate = spec.gen(rng);
                match (spec, &self.pattern) {
                    (DefaultString::Entropy(_), Some(pattern))
                        if !pattern.pattern.is_match(&candidate) =>
                    {
                        ()
                    }
                    _ => {
                        return Ok(Value::String(candidate));
                    }
                }
                if let (Some(now), Some(timeout)) = (now, timeout) {
                    if &now.elapsed() > timeout {
                        return Err(TimeoutError);
                    }
                }
            }
        } else {
            Ok(Value::Null)
        }
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum DefaultString {
    Literal(String),
    Entropy(Entropy),
}
impl DefaultString {
    pub fn gen<R: Rng + CryptoRng + Sync + Send>(&self, rng: &mut R) -> String {
        match self {
            DefaultString::Literal(s) => s.clone(),
            DefaultString::Entropy(e) => e.gen(rng),
        }
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct Entropy {
    pub charset: Option<CharSet>,
    pub len: usize,
}
impl Entropy {
    pub fn gen<R: Rng + CryptoRng + Sync + Send>(&self, rng: &mut R) -> String {
        let len = self.len;
        let set = self
            .charset
            .as_ref()
            .map(|cs| Cow::Borrowed(cs))
            .unwrap_or_else(|| Cow::Owned(Default::default()));
        std::iter::repeat_with(|| set.gen(rng)).take(len).collect()
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UnionTag {
    pub id: String,
    pub name: String,
    pub description: Option<String>,
    pub variant_names: LinearMap<String, String>,
}

#[derive(Clone, Debug, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ValueSpecUnion {
    pub tag: UnionTag,
    pub variants: LinearMap<String, ConfigSpec>,
    pub display_as: Option<String>,
    pub unique_by: UniqueBy,
}

impl<'de> serde::de::Deserialize<'de> for ValueSpecUnion {
    fn deserialize<D: serde::de::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(serde::Deserialize)]
        #[serde(rename_all = "camelCase")]
        #[serde(untagged)]
        pub enum _UnionTag {
            Old(String),
            New(UnionTag),
        }
        #[derive(serde::Deserialize)]
        #[serde(rename_all = "camelCase")]
        pub struct _ValueSpecUnion {
            pub variants: LinearMap<String, ConfigSpec>,
            pub tag: _UnionTag,
            pub display_as: Option<String>,
            #[serde(default)]
            pub unique_by: UniqueBy,
        }

        let union = _ValueSpecUnion::deserialize(deserializer)?;
        Ok(ValueSpecUnion {
            tag: match union.tag {
                _UnionTag::Old(id) => UnionTag {
                    id: id.clone(),
                    name: id,
                    description: None,
                    variant_names: union
                        .variants
                        .keys()
                        .map(|k| (k.to_owned(), k.to_owned()))
                        .collect(),
                },
                _UnionTag::New(UnionTag {
                    id,
                    name,
                    description,
                    mut variant_names,
                }) => UnionTag {
                    id,
                    name,
                    description,
                    variant_names: {
                        let mut iter = union.variants.keys();
                        while variant_names.len() < union.variants.len() {
                            if let Some(variant) = iter.next() {
                                variant_names.insert(variant.to_owned(), variant.to_owned());
                            } else {
                                break;
                            }
                        }
                        variant_names
                    },
                },
            },
            variants: union.variants,
            display_as: union.display_as,
            unique_by: union.unique_by,
        })
    }
}

#[async_trait]
impl ValueSpec for ValueSpecUnion {
    fn matches(&self, value: &Value) -> Result<(), NoMatchWithPath> {
        match value {
            Value::Object(o) => {
                if let Some(Value::String(ref tag)) = o.0.get(&self.tag.id) {
                    if let Some(obj_spec) = self.variants.get(tag) {
                        let mut without_tag = o.clone();
                        without_tag.0.remove(&self.tag.id);
                        obj_spec.matches(&without_tag)
                    } else {
                        Err(NoMatchWithPath::new(MatchError::Union(
                            tag.clone(),
                            self.variants.keys().cloned().collect(),
                        )))
                    }
                } else {
                    Err(NoMatchWithPath::new(MatchError::MissingTag(
                        self.tag.id.clone(),
                    )))
                }
            }
            Value::Null => Err(NoMatchWithPath::new(MatchError::NotNullable)),
            a => Err(NoMatchWithPath::new(MatchError::InvalidType(
                "object",
                a.type_of(),
            ))),
        }
    }
    fn validate(&self, manifest: &ManifestLatest) -> Result<(), NoMatchWithPath> {
        for (name, variant) in &self.variants {
            if variant.0.get(&self.tag.id).is_some() {
                return Err(NoMatchWithPath::new(MatchError::PropertyMatchesUnionTag(
                    self.tag.id.clone(),
                    name.clone(),
                )));
            }
            variant.validate(manifest)?;
        }
        Ok(())
    }
    async fn update(&self, value: &mut Value) -> Result<(), ConfigurationError> {
        if let Value::Object(o) = value {
            match o.0.get(&self.tag.id) {
                None => Err(ConfigurationError::NoMatch(NoMatchWithPath::new(
                    MatchError::MissingTag(self.tag.id.clone()),
                ))),
                Some(Value::String(tag)) => match self.variants.get(tag) {
                    None => Err(ConfigurationError::InvalidVariant(tag.clone())),
                    Some(spec) => spec.update(o).await,
                },
                Some(other) => Err(ConfigurationError::NoMatch(
                    NoMatchWithPath::new(MatchError::InvalidType("string", other.type_of()))
                        .prepend(self.tag.id.clone()),
                )),
            }
        } else {
            Err(ConfigurationError::NoMatch(NoMatchWithPath::new(
                MatchError::InvalidType("object", value.type_of()),
            )))
        }
    }
    fn requires(&self, id: &str, value: &Value) -> bool {
        if let Value::Object(o) = value {
            match o.0.get(&self.tag.id) {
                Some(Value::String(tag)) => match self.variants.get(tag) {
                    None => false,
                    Some(spec) => spec.requires(id, o),
                },
                _ => false,
            }
        } else {
            false
        }
    }
    fn eq(&self, lhs: &Value, rhs: &Value) -> bool {
        match (lhs, rhs) {
            (Value::Object(lhs), Value::Object(rhs)) => self.unique_by.eq(lhs, rhs),
            _ => false,
        }
    }
}
impl DefaultableWith for ValueSpecUnion {
    type DefaultSpec = String;
    type Error = ConfigurationError;

    fn gen_with<R: Rng + CryptoRng + Sync + Send>(
        &self,
        spec: &Self::DefaultSpec,
        rng: &mut R,
        timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error> {
        let variant = if let Some(v) = self.variants.get(spec) {
            v
        } else {
            return Err(ConfigurationError::InvalidVariant(spec.clone()));
        };
        let cfg_res = variant.gen(rng, timeout)?;

        let mut tagged_cfg = LinearMap::new();
        tagged_cfg.insert(self.tag.id.clone(), Value::String(spec.clone()));
        tagged_cfg.extend(cfg_res.0.into_iter());

        Ok(Value::Object(Config(tagged_cfg)))
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
#[serde(tag = "subtype")]
#[serde(rename_all = "kebab-case")]
pub enum ValueSpecPointer {
    App(AppPointerSpec),
    System(SystemPointerSpec),
}
impl fmt::Display for ValueSpecPointer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueSpecPointer::App(p) => write!(f, "{}", p),
            ValueSpecPointer::System(p) => write!(f, "{}", p),
        }
    }
}
impl Defaultable for ValueSpecPointer {
    type Error = ConfigurationError;
    fn gen<R: Rng + CryptoRng + Sync + Send>(
        &self,
        _rng: &mut R,
        _timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error> {
        Ok(Value::Null)
    }
}
#[async_trait]
impl ValueSpec for ValueSpecPointer {
    fn matches(&self, value: &Value) -> Result<(), NoMatchWithPath> {
        match self {
            ValueSpecPointer::App(a) => a.matches(value),
            ValueSpecPointer::System(a) => a.matches(value),
        }
    }
    fn validate(&self, manifest: &ManifestLatest) -> Result<(), NoMatchWithPath> {
        match self {
            ValueSpecPointer::App(a) => a.validate(manifest),
            ValueSpecPointer::System(a) => a.validate(manifest),
        }
    }
    async fn update(&self, value: &mut Value) -> Result<(), ConfigurationError> {
        match self {
            ValueSpecPointer::App(a) => a.update(value).await,
            ValueSpecPointer::System(a) => a.update(value).await,
        }
    }
    fn requires(&self, id: &str, value: &Value) -> bool {
        match self {
            ValueSpecPointer::App(a) => a.requires(id, value),
            ValueSpecPointer::System(a) => a.requires(id, value),
        }
    }
    fn eq(&self, _lhs: &Value, _rhs: &Value) -> bool {
        false
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct AppPointerSpec {
    pub app_id: String,
    #[serde(flatten)]
    pub target: AppPointerSpecVariants,
}
impl fmt::Display for AppPointerSpec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}].{}", self.app_id, self.target)
    }
}
impl AppPointerSpec {
    async fn deref(&self) -> Result<Value, ConfigurationError> {
        match self.target {
            AppPointerSpecVariants::TorAddress => {
                let mut apps = crate::apps::list_info()
                    .await
                    .map_err(ConfigurationError::SystemError)?;
                let info = apps.remove(&self.app_id);
                Ok(info
                    .and_then(|info| info.tor_address)
                    .map(Value::String)
                    .unwrap_or(Value::Null))
            }
            AppPointerSpecVariants::TorKey => {
                let services_path = PersistencePath::from_ref(crate::SERVICES_YAML);
                let service_map = crate::tor::services_map(&services_path)
                    .await
                    .map_err(ConfigurationError::SystemError)?;
                let service =
                    service_map
                        .map
                        .get(&self.app_id)
                        .ok_or(ConfigurationError::SystemError(crate::Error::new(
                            failure::format_err!("App Not Found"),
                            Some(crate::error::NOT_FOUND),
                        )))?;
                Ok(
                    crate::tor::read_tor_key(&self.app_id, service.hidden_service_version, None)
                        .await
                        .map(Value::String)
                        .unwrap_or(Value::Null),
                )
            }
            AppPointerSpecVariants::LanAddress => {
                let services_path = PersistencePath::from_ref(crate::SERVICES_YAML);
                let mut service_map = crate::tor::services_map(&services_path)
                    .await
                    .map_err(ConfigurationError::SystemError)?;
                let service = service_map.map.remove(&self.app_id);
                Ok(service
                    .map(|service| Value::String(format!("{}", service.ip)))
                    .unwrap_or(Value::Null))
            }
            AppPointerSpecVariants::Config { ref index } => {
                // check if the app exists
                if !crate::apps::list_info()
                    .await
                    .map_err(ConfigurationError::SystemError)?
                    .contains_key(&self.app_id)
                {
                    return Ok(Value::Null);
                }
                // fetch the config of the pointer target
                let app_config = crate::apps::config(&self.app_id)
                    .await
                    .map_err(ConfigurationError::SystemError)?;
                let cfg = if let Some(cfg) = app_config.config {
                    cfg
                } else {
                    return Ok(Value::Null);
                };
                let mut cfgs = LinearMap::new();
                cfgs.insert(self.app_id.as_str(), Cow::Borrowed(&cfg));

                Ok((index.compiled)(&cfg, &cfgs))
            }
        }
    }
}
impl Defaultable for AppPointerSpec {
    type Error = ConfigurationError;
    fn gen<R: Rng + CryptoRng + Sync + Send>(
        &self,
        _rng: &mut R,
        _timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error> {
        Ok(Value::Null)
    }
}
#[async_trait]
impl ValueSpec for AppPointerSpec {
    fn matches(&self, _value: &Value) -> Result<(), NoMatchWithPath> {
        Ok(())
    }
    fn validate(&self, manifest: &ManifestLatest) -> Result<(), NoMatchWithPath> {
        if manifest.id != self.app_id && !manifest.dependencies.0.contains_key(&self.app_id) {
            return Err(NoMatchWithPath::new(MatchError::InvalidPointer(
                ValueSpecPointer::App(self.clone()),
            )));
        }
        match self.target {
            AppPointerSpecVariants::TorKey if manifest.id != self.app_id => {
                Err(NoMatchWithPath::new(MatchError::InvalidPointer(
                    ValueSpecPointer::App(self.clone()),
                )))
            }
            _ => Ok(()),
        }
    }
    async fn update(&self, value: &mut Value) -> Result<(), ConfigurationError> {
        *value = self.deref().await?;
        Ok(())
    }
    fn requires(&self, id: &str, _value: &Value) -> bool {
        self.app_id == id
    }
    fn eq(&self, _lhs: &Value, _rhs: &Value) -> bool {
        false
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
#[serde(tag = "target")]
#[serde(rename_all = "kebab-case")]
pub enum AppPointerSpecVariants {
    TorAddress,
    TorKey,
    LanAddress,
    Config { index: Arc<ConfigPointer> },
}
impl fmt::Display for AppPointerSpecVariants {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::TorAddress => write!(f, "TOR_ADDRESS"),
            Self::TorKey => write!(f, "TOR_KEY"),
            Self::LanAddress => write!(f, "LAN_ADDRESS"),
            Self::Config { index } => write!(f, "{}", index.src),
        }
    }
}

#[derive(Clone)]
pub struct ConfigPointer {
    pub src: String,
    pub compiled: Arc<super::rules::CompiledExpr<Value>>,
}
impl std::fmt::Debug for ConfigPointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ConfigPointer")
            .field("src", &self.src)
            .field("compiled", &"Fn(&Config, &Config) -> bool")
            .finish()
    }
}
impl<'de> serde::de::Deserialize<'de> for ConfigPointer {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        let src = String::deserialize(deserializer)?;
        let compiled = super::rules::compile_expr(&src).map_err(serde::de::Error::custom)?;
        Ok(ConfigPointer {
            src,
            compiled: Arc::new(compiled),
        })
    }
}
impl serde::ser::Serialize for ConfigPointer {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        serializer.serialize_str(&self.src)
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
#[serde(tag = "target")]
pub enum SystemPointerSpec {
    HostIp,
}
impl fmt::Display for SystemPointerSpec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "[SYSTEM].{}",
            match self {
                SystemPointerSpec::HostIp => "HOST_IP",
            }
        )
    }
}
impl SystemPointerSpec {
    async fn deref(&self) -> Result<Value, ConfigurationError> {
        Ok(match self {
            SystemPointerSpec::HostIp => {
                Value::String(format!("{}", std::net::Ipv4Addr::from(crate::HOST_IP)))
            }
        })
    }
}
impl Defaultable for SystemPointerSpec {
    type Error = ConfigurationError;
    fn gen<R: Rng + CryptoRng + Sync + Send>(
        &self,
        _rng: &mut R,
        _timeout: &Option<Duration>,
    ) -> Result<Value, Self::Error> {
        Ok(Value::Null)
    }
}
#[async_trait]
impl ValueSpec for SystemPointerSpec {
    fn matches(&self, _value: &Value) -> Result<(), NoMatchWithPath> {
        Ok(())
    }
    fn validate(&self, _manifest: &ManifestLatest) -> Result<(), NoMatchWithPath> {
        Ok(())
    }
    async fn update(&self, value: &mut Value) -> Result<(), ConfigurationError> {
        *value = self.deref().await?;
        Ok(())
    }
    fn requires(&self, _id: &str, _value: &Value) -> bool {
        false
    }
    fn eq(&self, _lhs: &Value, _rhs: &Value) -> bool {
        false
    }
}

#[cfg(test)]
mod test {
    use rand::SeedableRng;

    use super::*;

    #[test]
    fn test_config() {
        let spec = serde_json::json!({
          "randomEnum": {
            "name": "Random Enum",
            "type": "enum",
            "default": "null",
            "description": "This is not even real.",
            "changeWarning": "Be careful chnaging this!",
            "values": [
              "null",
              "option1",
              "option2",
              "option3"
            ]
          },
          "testnet": {
            "name": "Testnet",
            "type": "boolean",
            "description": "determines whether your node is running ontestnet or mainnet",
            "changeWarning": "Chain will have to resync!",
            "default": false
          },
          "favoriteNumber": {
            "name": "Favorite Number",
            "type": "number",
            "integral": false,
            "description": "Your favorite number of all time",
            "changeWarning": "Once you set this number, it can never be changed without severe consequences.",
            "nullable": false,
            "default": 7,
            "range": "(-100,100]"
          },
          "secondaryNumbers": {
            "name": "Unlucky Numbers",
            "type": "list",
            "subtype": "number",
            "description": "Numbers that you like but are not your top favorite.",
            "spec": {
              "type": "number",
              "integral": false,
              "range": "[-100,200)"
            },
            "range": "[0,10]",
            "default": [
              2,
              3
            ]
          },
          "rpcsettings": {
            "name": "RPC Settings",
            "type": "object",
            "description": "rpc username and password",
            "changeWarning": "Adding RPC users gives them special permissions on your node.",
            "nullable": false,
            "nullByDefault": false,
            "spec": {
              "laws": {
                "name": "Laws",
                "type": "object",
                "description": "the law of the realm",
                "nullable": true,
                "nullByDefault": true,
                "spec": {
                  "law1": {
                    "name": "First Law",
                    "type": "string",
                    "description": "the first law",
                    "nullable": true
                  },
                  "law2": {
                    "name": "Second Law",
                    "type": "string",
                    "description": "the second law",
                    "nullable": true
                  }
                }
              },
              "rulemakers": {
                "name": "Rule Makers",
                "type": "list",
                "subtype": "object",
                "description": "the people who make the rules",
                "range": "[0,2]",
                "default": [],
                "spec": {
                  "type": "object",
                  "spec": {
                    "rulemakername": {
                      "name": "Rulemaker Name",
                      "type": "string",
                      "description": "the name of the rule maker",
                      "nullable": false,
                      "default": {
                        "charset": "a-g,2-9",
                        "len": 12
                      }
                    },
                    "rulemakerip": {
                      "name": "Rulemaker IP",
                      "type": "string",
                      "description": "the ip of the rule maker",
                      "nullable": false,
                      "default": "192.168.1.0",
                      "pattern": "^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$",
                      "patternDescription": "may only contain numbers and periods"
                    }
                  }
                }
              },
              "rpcuser": {
                "name": "RPC Username",
                "type": "string",
                "description": "rpc username",
                "nullable": false,
                "default": "defaultrpcusername",
                "pattern": "^[a-zA-Z]+$",
                "patternDescription": "must contain only letters."
              },
              "rpcpass": {
                "name": "RPC User Password",
                "type": "string",
                "description": "rpc password",
                "nullable": false,
                "default": {
                  "charset": "a-z,A-Z,2-9",
                  "len": 20
                }
              }
            }
          },
          "advanced": {
            "name": "Advanced",
            "type": "object",
            "description": "Advanced settings",
            "nullable": false,
            "nullByDefault": false,
            "spec": {
              "notifications": {
                "name": "Notification Preferences",
                "type": "list",
                "subtype": "enum",
                "description": "how you want to be notified",
                "range": "[1,3]",
                "default": [
                  "email"
                ],
                "spec": {
                  "type": "enum",
                  "values": [
                    "email",
                    "text",
                    "call",
                    "push",
                    "webhook"
                  ]
                }
              }
            }
          },
          "bitcoinNode": {
            "name": "Bitcoin Node Settings",
            "type": "union",
            "description": "The node settings",
            "default": "internal",
            "tag": {
                "id": "type",
                "name": "Type",
                "variantNames": {}
            },
            "variants": {
              "internal": {
                "lan-address": {
                  "name": "LAN Address",
                  "type": "pointer",
                  "subtype": "app",
                  "target": "lan-address",
                  "app-id": "bitcoind",
                  "description": "the lan address"
                }
              },
              "external": {
                "public-domain": {
                  "name": "Public Domain",
                  "type": "string",
                  "description": "the public address of the node",
                  "nullable": false,
                  "default": "bitcoinnode.com",
                  "pattern": ".*",
                  "patternDescription": "anything"
                }
              }
            }
          },
          "port": {
            "name": "Port",
            "type": "number",
            "integral": true,
            "description": "the default port for your Bitcoin node. default: 8333, testnet: 18333, regtest: 18444",
            "nullable": true,
            "default": 8333,
            "range": "[0, 9999]",
            "units": "m/s"
          },
          "maxconnections": {
            "name": "Max Connections",
            "type": "string",
            "description": "the maximum number of commections allowed to your Bitcoin node",
            "nullable": true
          },
          "rpcallowip": {
            "name": "RPC Allowed IPs",
            "type": "list",
            "subtype": "string",
            "description": "external ip addresses that are authorized to access your Bitcoin node",
            "changeWarning": "Any IP you allow here will have RPC access to your Bitcoin node.",
            "range": "[1,10]",
            "default": [
              "192.168.1.1"
            ],
            "spec": {
              "type": "string",
              "pattern": "((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|((^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$)|(^[a-z2-7]{16}\\.onion$)|(^([a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?\\.)+[a-z0-9][a-z0-9-]{0,61}[a-z0-9]$))",
              "patternDescription": "must be a valid ipv4, ipv6, or domain name"
            }
          },
          "rpcauth": {
            "name": "RPC Auth",
            "type": "list",
            "subtype": "string",
            "description": "api keys that are authorized to access your Bitcoin node.",
            "range": "[0,*)",
            "default": [],
            "spec": {
              "type": "string"
            }
          }
        });
        let spec: ConfigSpec = serde_json::from_value(spec).unwrap();
        let mut deps = crate::dependencies::Dependencies::default();
        deps.0.insert(
            "bitcoind".to_owned(),
            crate::dependencies::DepInfo {
                version: "^0.20.0".parse().unwrap(),
                description: None,
                mount_public: false,
                mount_shared: false,
                optional: Some("Could be external.".to_owned()),
                config: Vec::new(),
            },
        );
        spec.validate(&crate::manifest::ManifestV0 {
            id: "test-app".to_owned(),
            version: "0.1.0".parse().unwrap(),
            title: "Test App".to_owned(),
            description: crate::manifest::Description {
                short: "A test app.".to_owned(),
                long: "A super cool test app for testing".to_owned(),
            },
            release_notes: "Some things changed".to_owned(),
            ports: Vec::new(),
            image: crate::manifest::ImageConfig::Tar,
            shm_size_mb: None,
            mount: "/root".parse().unwrap(),
            public: None,
            shared: None,
            has_instructions: false,
            os_version_required: ">=0.2.5".parse().unwrap(),
            os_version_recommended: ">=0.2.5".parse().unwrap(),
            assets: Vec::new(),
            hidden_service_version: crate::tor::HiddenServiceVersion::V3,
            dependencies: deps,
            extra: LinearMap::new(),
        })
        .unwrap();
        let config = spec
            .gen(&mut rand::rngs::StdRng::from_entropy(), &None)
            .unwrap();
        spec.matches(&config).unwrap();
    }
}
