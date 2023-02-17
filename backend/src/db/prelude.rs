use std::marker::PhantomData;
use std::panic::UnwindSafe;

use patch_db::value::InternedString;
pub use patch_db::{HasModel, PatchDb, Value};
use serde::de::DeserializeOwned;
use serde::Serialize;

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

#[async_trait::async_trait]
pub trait PatchDbExt {
    async fn peek(&self) -> Result<DatabaseModel, Error>;
    async fn mutate<U: UnwindSafe + Send>(
        &self,
        f: impl FnOnce(&mut DatabaseModel) -> Result<U, Error> + UnwindSafe + Send,
    ) -> Result<U, Error>;
    async fn map_mutate(
        &self,
        f: impl FnOnce(DatabaseModel) -> Result<DatabaseModel, Error> + UnwindSafe + Send,
    ) -> Result<DatabaseModel, Error>;
}
#[async_trait::async_trait]
impl PatchDbExt for PatchDb {
    async fn peek(&self) -> Result<DatabaseModel, Error> {
        Ok(DatabaseModel::from(self.dump().await?.value))
    }
    async fn mutate<U: UnwindSafe + Send>(
        &self,
        f: impl FnOnce(&mut DatabaseModel) -> Result<U, Error> + UnwindSafe + Send,
    ) -> Result<U, Error> {
        Ok(self
            .apply_function(|v| {
                let mut model = <&mut DatabaseModel>::from(&mut v);
                let res = f(&mut model)?;
                Ok::<_, Error>((v, res))
            })
            .await?
            .1)
    }
    async fn map_mutate(
        &self,
        f: impl FnOnce(DatabaseModel) -> Result<DatabaseModel, Error> + UnwindSafe + Send,
    ) -> Result<DatabaseModel, Error> {
        Ok(DatabaseModel::from(
            self.apply_function(|v| f(DatabaseModel::from(v)).map(|a| (a.into(), ())))
                .await?
                .0,
        ))
    }
}

/// &mut Model<T> <=> &mut Value
#[repr(transparent)]
#[derive(Debug)]
pub struct Model<T> {
    value: Value,
    phantom: PhantomData<T>,
}
impl<T: DeserializeOwned> Model<T> {
    pub fn de(self) -> Result<T, Error> {
        from_value(self.value)
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
}

impl<T: Map> Model<T>
where
    T::Key: AsRef<str>,
    T::Value: Serialize,
{
    pub fn insert(&mut self, key: &T::Key, value: &T::Value) -> Result<(), Error> {
        use serde::ser::Error;
        let v = patch_db::value::to_value(value)?;
        match &mut self.value {
            Value::Object(o) => {
                o.insert(InternedString::intern(key.as_ref()), v);
                Ok(())
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
                o.insert(InternedString::intern(key.as_ref()), v);
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
    T::Key: DeserializeOwned + Ord + Clone,
{
    pub fn keys(&self) -> Result<Vec<T::Key>, Error> {
        use serde::de::Error;
        use serde::Deserialize;
        match &self.value {
            Value::Object(o) => o
                .keys()
                .cloned()
                .map(|k| {
                    T::Key::deserialize(patch_db::value::de::InternedStringDeserializer::from(k))
                        .map_err(|e| {
                            patch_db::value::Error {
                                kind: patch_db::value::ErrorKind::Deserialization,
                                source: e,
                            }
                            .into()
                        })
                })
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
        use serde::Deserialize;
        match self.value {
            Value::Object(o) => o
                .into_iter()
                .map(|(k, v)| {
                    Ok((
                        T::Key::deserialize(patch_db::value::de::InternedStringDeserializer::from(
                            k,
                        ))
                        .with_kind(ErrorKind::Deserialization)?,
                        Model::from_value(v),
                    ))
                })
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
        use serde::Deserialize;
        match &self.value {
            Value::Object(o) => o
                .iter()
                .map(|(k, v)| {
                    Ok((
                        T::Key::deserialize(patch_db::value::de::InternedStringDeserializer::from(
                            k.clone(),
                        ))
                        .with_kind(ErrorKind::Deserialization)?,
                        Model::value_as(v),
                    ))
                })
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
        use serde::Deserialize;
        match &self.value {
            Value::Object(o) => o
                .iter_mut()
                .map(|(k, v)| {
                    Ok((
                        T::Key::deserialize(patch_db::value::de::InternedStringDeserializer::from(
                            k.clone(),
                        ))
                        .with_kind(ErrorKind::Deserialization)?,
                        Model::value_as_mut(v),
                    ))
                })
                .collect(),
            v => Err(patch_db::value::Error {
                source: patch_db::value::ErrorSource::custom(format!("expected object found {v}")),
                kind: patch_db::value::ErrorKind::Deserialization,
            }
            .into()),
        }
    }
}
impl<T: Map> Model<T>
where
    T::Key: AsRef<str>,
{
    pub fn into_idx(self, key: &T::Key) -> Option<Model<T::Value>> {
        use patch_db::ModelExt;
        match &self.value {
            Value::Object(o) if o.contains_key(key.as_ref()) => Some(self.transmute(|v| {
                use patch_db::value::index::Index;
                key.as_ref().index_into_owned(v).unwrap()
            })),
            _ => None,
        }
    }
    pub fn as_idx<'a>(&'a self, key: &T::Key) -> Option<&'a Model<T::Value>> {
        use patch_db::ModelExt;
        match &self.value {
            Value::Object(o) if o.contains_key(key.as_ref()) => Some(self.transmute_ref(|v| {
                use patch_db::value::index::Index;
                key.as_ref().index_into(v).unwrap()
            })),
            _ => None,
        }
    }
    pub fn as_idx_mut<'a>(&'a mut self, key: &T::Key) -> Option<&'a mut Model<T::Value>> {
        use patch_db::ModelExt;
        match &mut self.value {
            Value::Object(o) if o.contains_key(key.as_ref()) => Some(self.transmute_mut(|v| {
                use patch_db::value::index::Index;
                key.as_ref().index_or_insert(v)
            })),
            _ => None,
        }
    }
    pub fn remove(&mut self, key: &T::Key) -> Result<Option<Model<T::Value>>, Error> {
        use serde::ser::Error;
        match &mut self.value {
            Value::Object(o) => {
                let v = o.remove(key.as_ref());
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
