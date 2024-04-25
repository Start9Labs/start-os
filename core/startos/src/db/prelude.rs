use std::collections::BTreeMap;
use std::future::Future;
use std::marker::PhantomData;
use std::panic::UnwindSafe;
use std::str::FromStr;

use chrono::{DateTime, Utc};
pub use imbl_value::Value;
use patch_db::json_ptr::ROOT;
use patch_db::value::InternedString;
pub use patch_db::{HasModel, PatchDb};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};

use crate::db::model::DatabaseModel;
use crate::prelude::*;

pub fn to_value<T>(value: &T) -> Result<Value, Error>
where
    T: Serialize,
{
    patch_db::value::to_value(value).with_kind(ErrorKind::Serialization)
}

pub fn from_value<T>(value: Value) -> Result<T, Error>
where
    T: DeserializeOwned,
{
    patch_db::value::from_value(value).with_kind(ErrorKind::Deserialization)
}

pub type TypedPatchDb<T> = patch_db::TypedPatchDb<T, Error>;

/// &mut Model<T> <=> &mut Value
#[repr(transparent)]
#[derive(Debug)]
pub struct Model<T> {
    value: Value,
    phantom: PhantomData<T>,
}
impl<T: DeserializeOwned> Model<T> {
    pub fn de(&self) -> Result<T, Error> {
        from_value(self.value.clone())
    }
}
impl<T: Serialize> Model<T> {
    pub fn new(value: &T) -> Result<Self, Error> {
        Ok(Self::from(to_value(value)?))
    }
    pub fn ser(&mut self, value: &T) -> Result<(), Error> {
        self.value = to_value(value)?;
        Ok(())
    }
}

impl<T> Serialize for Model<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.value.serialize(serializer)
    }
}

impl<'de, T: Serialize + Deserialize<'de>> Deserialize<'de> for Model<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        Self::new(&T::deserialize(deserializer)?).map_err(D::Error::custom)
    }
}

impl<T: Serialize + DeserializeOwned> Model<T> {
    pub fn replace(&mut self, value: &T) -> Result<T, Error> {
        let orig = self.de()?;
        self.ser(value)?;
        Ok(orig)
    }
    pub fn mutate<U>(&mut self, f: impl FnOnce(&mut T) -> Result<U, Error>) -> Result<U, Error> {
        let mut orig = self.de()?;
        let res = f(&mut orig)?;
        self.ser(&orig)?;
        Ok(res)
    }
    pub fn map_mutate(&mut self, f: impl FnOnce(T) -> Result<T, Error>) -> Result<T, Error> {
        let mut orig = self.de()?;
        let res = f(orig)?;
        self.ser(&res)?;
        Ok(res)
    }
}
impl<T> Clone for Model<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            phantom: PhantomData,
        }
    }
}
impl<T> From<Value> for Model<T> {
    fn from(value: Value) -> Self {
        Self {
            value,
            phantom: PhantomData,
        }
    }
}
impl<T> From<Model<T>> for Value {
    fn from(value: Model<T>) -> Self {
        value.value
    }
}
impl<'a, T> From<&'a Value> for &'a Model<T> {
    fn from(value: &'a Value) -> Self {
        unsafe { std::mem::transmute(value) }
    }
}
impl<'a, T> From<&'a Model<T>> for &'a Value {
    fn from(value: &'a Model<T>) -> Self {
        unsafe { std::mem::transmute(value) }
    }
}
impl<'a, T> From<&'a mut Value> for &mut Model<T> {
    fn from(value: &'a mut Value) -> Self {
        unsafe { std::mem::transmute(value) }
    }
}
impl<'a, T> From<&'a mut Model<T>> for &mut Value {
    fn from(value: &'a mut Model<T>) -> Self {
        unsafe { std::mem::transmute(value) }
    }
}
impl<T> patch_db::Model<T> for Model<T> {
    type Model<U> = Model<U>;
}

impl<T> Model<Option<T>> {
    pub fn transpose(self) -> Option<Model<T>> {
        use patch_db::ModelExt;
        if self.value.is_null() {
            None
        } else {
            Some(self.transmute(|a| a))
        }
    }
    pub fn transpose_ref(&self) -> Option<&Model<T>> {
        use patch_db::ModelExt;
        if self.value.is_null() {
            None
        } else {
            Some(self.transmute_ref(|a| a))
        }
    }
    pub fn transpose_mut(&mut self) -> Option<&mut Model<T>> {
        use patch_db::ModelExt;
        if self.value.is_null() {
            None
        } else {
            Some(self.transmute_mut(|a| a))
        }
    }
    pub fn from_option(opt: Option<Model<T>>) -> Self {
        use patch_db::ModelExt;
        match opt {
            Some(a) => a.transmute(|a| a),
            None => Self::from_value(Value::Null),
        }
    }
}

pub trait Map: DeserializeOwned + Serialize {
    type Key;
    type Value;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error>;
    fn key_string(key: &Self::Key) -> Result<InternedString, Error> {
        Ok(InternedString::intern(Self::key_str(key)?.as_ref()))
    }
}

impl<A, B> Map for BTreeMap<A, B>
where
    A: serde::Serialize + serde::de::DeserializeOwned + Ord + AsRef<str>,
    B: serde::Serialize + serde::de::DeserializeOwned,
{
    type Key = A;
    type Value = B;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        Ok(key.as_ref())
    }
}

impl<A, B> Map for BTreeMap<JsonKey<A>, B>
where
    A: serde::Serialize + serde::de::DeserializeOwned + Ord,
    B: serde::Serialize + serde::de::DeserializeOwned,
{
    type Key = A;
    type Value = B;
    fn key_str(key: &Self::Key) -> Result<impl AsRef<str>, Error> {
        serde_json::to_string(key).with_kind(ErrorKind::Serialization)
    }
}

impl<T: Map> Model<T>
where
    T::Value: Serialize,
{
    pub fn insert(&mut self, key: &T::Key, value: &T::Value) -> Result<(), Error> {
        use serde::ser::Error;
        let v = patch_db::value::to_value(value)?;
        match &mut self.value {
            Value::Object(o) => {
                o.insert(T::key_string(key)?, v);
                Ok(())
            }
            v => Err(patch_db::value::Error {
                source: patch_db::value::ErrorSource::custom(format!("expected object found {v}")),
                kind: patch_db::value::ErrorKind::Serialization,
            }
            .into()),
        }
    }
    pub fn upsert<F, D>(&mut self, key: &T::Key, value: F) -> Result<&mut Model<T::Value>, Error>
    where
        F: FnOnce() -> D,
        D: AsRef<T::Value>,
    {
        use serde::ser::Error;
        match &mut self.value {
            Value::Object(o) => {
                use patch_db::ModelExt;
                let s = T::key_str(key)?;
                let exists = o.contains_key(s.as_ref());
                let res = self.transmute_mut(|v| {
                    use patch_db::value::index::Index;
                    s.as_ref().index_or_insert(v)
                });
                if !exists {
                    res.ser(value().as_ref())?;
                }
                Ok(res)
            }
            v => Err(patch_db::value::Error {
                source: patch_db::value::ErrorSource::custom(format!("expected object found {v}")),
                kind: patch_db::value::ErrorKind::Serialization,
            }
            .into()),
        }
    }
    pub fn insert_model(&mut self, key: &T::Key, value: Model<T::Value>) -> Result<(), Error> {
        use patch_db::ModelExt;
        use serde::ser::Error;
        let v = value.into_value();
        match &mut self.value {
            Value::Object(o) => {
                o.insert(T::key_string(key)?, v);
                Ok(())
            }
            v => Err(patch_db::value::Error {
                source: patch_db::value::ErrorSource::custom(format!("expected object found {v}")),
                kind: patch_db::value::ErrorKind::Serialization,
            }
            .into()),
        }
    }
}

impl<T: Map> Model<T>
where
    T::Key: FromStr + Ord + Clone,
    Error: From<<T::Key as FromStr>::Err>,
{
    pub fn keys(&self) -> Result<Vec<T::Key>, Error> {
        use serde::de::Error;
        match &self.value {
            Value::Object(o) => o
                .keys()
                .cloned()
                .map(|k| Ok(T::Key::from_str(&*k)?))
                .collect(),
            v => Err(patch_db::value::Error {
                source: patch_db::value::ErrorSource::custom(format!("expected object found {v}")),
                kind: patch_db::value::ErrorKind::Deserialization,
            }
            .into()),
        }
    }

    pub fn into_entries(self) -> Result<Vec<(T::Key, Model<T::Value>)>, Error> {
        use patch_db::ModelExt;
        use serde::de::Error;
        match self.value {
            Value::Object(o) => o
                .into_iter()
                .map(|(k, v)| Ok((T::Key::from_str(&*k)?, Model::from_value(v))))
                .collect(),
            v => Err(patch_db::value::Error {
                source: patch_db::value::ErrorSource::custom(format!("expected object found {v}")),
                kind: patch_db::value::ErrorKind::Deserialization,
            }
            .into()),
        }
    }
    pub fn as_entries(&self) -> Result<Vec<(T::Key, &Model<T::Value>)>, Error> {
        use patch_db::ModelExt;
        use serde::de::Error;
        match &self.value {
            Value::Object(o) => o
                .iter()
                .map(|(k, v)| Ok((T::Key::from_str(&**k)?, Model::value_as(v))))
                .collect(),
            v => Err(patch_db::value::Error {
                source: patch_db::value::ErrorSource::custom(format!("expected object found {v}")),
                kind: patch_db::value::ErrorKind::Deserialization,
            }
            .into()),
        }
    }
    pub fn as_entries_mut(&mut self) -> Result<Vec<(T::Key, &mut Model<T::Value>)>, Error> {
        use patch_db::ModelExt;
        use serde::de::Error;
        match &mut self.value {
            Value::Object(o) => o
                .iter_mut()
                .map(|(k, v)| Ok((T::Key::from_str(&**k)?, Model::value_as_mut(v))))
                .collect(),
            v => Err(patch_db::value::Error {
                source: patch_db::value::ErrorSource::custom(format!("expected object found {v}")),
                kind: patch_db::value::ErrorKind::Deserialization,
            }
            .into()),
        }
    }
}
impl<T: Map> Model<T> {
    pub fn into_idx(self, key: &T::Key) -> Option<Model<T::Value>> {
        use patch_db::ModelExt;
        let s = T::key_str(key).ok()?;
        match &self.value {
            Value::Object(o) if o.contains_key(s.as_ref()) => Some(self.transmute(|v| {
                use patch_db::value::index::Index;
                s.as_ref().index_into_owned(v).unwrap()
            })),
            _ => None,
        }
    }
    pub fn as_idx<'a>(&'a self, key: &T::Key) -> Option<&'a Model<T::Value>> {
        use patch_db::ModelExt;
        let s = T::key_str(key).ok()?;
        match &self.value {
            Value::Object(o) if o.contains_key(s.as_ref()) => Some(self.transmute_ref(|v| {
                use patch_db::value::index::Index;
                s.as_ref().index_into(v).unwrap()
            })),
            _ => None,
        }
    }
    pub fn as_idx_mut<'a>(&'a mut self, key: &T::Key) -> Option<&'a mut Model<T::Value>> {
        use patch_db::ModelExt;
        let s = T::key_str(key).ok()?;
        match &mut self.value {
            Value::Object(o) if o.contains_key(s.as_ref()) => Some(self.transmute_mut(|v| {
                use patch_db::value::index::Index;
                s.as_ref().index_or_insert(v)
            })),
            _ => None,
        }
    }
    pub fn remove(&mut self, key: &T::Key) -> Result<Option<Model<T::Value>>, Error> {
        use serde::ser::Error;
        match &mut self.value {
            Value::Object(o) => {
                let v = o.remove(T::key_str(key)?.as_ref());
                Ok(v.map(patch_db::ModelExt::from_value))
            }
            v => Err(patch_db::value::Error {
                source: patch_db::value::ErrorSource::custom(format!("expected object found {v}")),
                kind: patch_db::value::ErrorKind::Serialization,
            }
            .into()),
        }
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct JsonKey<T>(pub T);
impl<T> From<T> for JsonKey<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}
impl<T> JsonKey<T> {
    pub fn new(value: T) -> Self {
        Self(value)
    }
    pub fn unwrap(self) -> T {
        self.0
    }
    pub fn new_ref(value: &T) -> &Self {
        unsafe { std::mem::transmute(value) }
    }
    pub fn new_mut(value: &mut T) -> &mut Self {
        unsafe { std::mem::transmute(value) }
    }
}
impl<T> std::ops::Deref for JsonKey<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<T> std::ops::DerefMut for JsonKey<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl<T: Serialize> Serialize for JsonKey<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::Error;
        serde_json::to_string(&self.0)
            .map_err(S::Error::custom)?
            .serialize(serializer)
    }
}
// { "foo": "bar" } -> "{ \"foo\": \"bar\" }"
impl<'de, T: Serialize + DeserializeOwned> Deserialize<'de> for JsonKey<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        let string = String::deserialize(deserializer)?;
        Ok(Self(
            serde_json::from_str(&string).map_err(D::Error::custom)?,
        ))
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct WithTimeData<T> {
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub value: T,
}
impl<T> WithTimeData<T> {
    pub fn new(value: T) -> Self {
        let now = Utc::now();
        Self {
            created_at: now,
            updated_at: now,
            value,
        }
    }
}
impl<T> std::ops::Deref for WithTimeData<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
impl<T> std::ops::DerefMut for WithTimeData<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.updated_at = Utc::now();
        &mut self.value
    }
}
