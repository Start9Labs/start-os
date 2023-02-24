use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::panic::UnwindSafe;
use std::sync::Arc;

use imbl::OrdSet;
pub use patch_db::{HasModel, PatchDb, Value};
use serde::de::DeserializeOwned;
use serde::Serialize;

use crate::db::model::{DatabaseModel, DatabaseModelMut};
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
        &mut self,
        f: impl FnOnce(DatabaseModelMut) -> Result<U, Error> + UnwindSafe + Send,
    ) -> Result<U, Error>;
    async fn map_mutate(
        &mut self,
        f: impl FnOnce(DatabaseModel) -> Result<DatabaseModel, Error> + UnwindSafe + Send,
    ) -> Result<DatabaseModel, Error>;
}
#[async_trait::async_trait]
impl PatchDbExt for PatchDb {
    async fn peek(&self) -> Result<DatabaseModel, Error> {
        Ok(<DatabaseModel as patch_db::Model>::new(
            self.dump().await?.value,
        ))
    }
    async fn mutate<U: UnwindSafe + Send>(
        &mut self,
        f: impl FnOnce(DatabaseModelMut) -> Result<U, Error> + UnwindSafe + Send,
    ) -> Result<U, Error> {
        Ok(self
            .apply_function(|v| {
                let res = f(<DatabaseModelMut as patch_db::ModelMut>::new(&mut v))?;
                Ok::<_, Error>((v, res))
            })
            .await?
            .1)
    }
    async fn map_mutate(
        &mut self,
        f: impl FnOnce(DatabaseModel) -> Result<DatabaseModel, Error> + UnwindSafe + Send,
    ) -> Result<DatabaseModel, Error> {
        Ok(<DatabaseModel as patch_db::Model>::new(
            self.apply_function(|v| {
                f(<DatabaseModel as patch_db::Model>::new(v))
                    .map(|a| (patch_db::Model::into_inner(a), ()))
            })
            .await?
            .0,
        ))
    }
}

pub trait ModelExt {
    type T;
    type Mut<'a>
    where
        Self: 'a;
    fn de(&self) -> Result<Self::T, Error>;
    fn ser(&mut self, value: &Self::T) -> Result<(), Error>;
    fn mutate<U>(&mut self, f: impl FnOnce(&mut Self::T) -> Result<U, Error>) -> Result<U, Error>;
    fn map_mutate(
        &mut self,
        f: impl FnOnce(Self::T) -> Result<Self::T, Error>,
    ) -> Result<Self::T, Error>;
}
impl<T: patch_db::Model> ModelExt for T {
    type T = <T as patch_db::Model>::T;
    type Mut<'a> = <T as patch_db::Model>::Mut<'a> where T: 'a;
    fn de(&self) -> Result<Self::T, Error> {
        self.get().with_kind(ErrorKind::Deserialization)
    }
    fn ser(&mut self, value: &Self::T) -> Result<(), Error> {
        self.set(value).with_kind(ErrorKind::Serialization)
    }
    fn mutate<U>(&mut self, f: impl FnOnce(&mut Self::T) -> Result<U, Error>) -> Result<U, Error> {
        let mut v = self.de()?;
        let res = f(&mut v)?;
        self.ser(&v)?;
        Ok(res)
    }
    fn map_mutate(
        &mut self,
        f: impl FnOnce(Self::T) -> Result<Self::T, Error>,
    ) -> Result<Self::T, Error> {
        let v = self.de()?;
        let v = f(v)?;
        self.ser(&v)?;
        Ok(v)
    }
}

pub trait ModelMutExt<'a> {
    type T;
    fn de(&self) -> Result<Self::T, Error>;
    fn ser(&mut self, value: &Self::T) -> Result<(), Error>;
    fn mutate<U>(&mut self, f: impl FnOnce(&mut Self::T) -> Result<U, Error>) -> Result<U, Error>;
    fn map_mutate(
        &mut self,
        f: impl FnOnce(Self::T) -> Result<Self::T, Error>,
    ) -> Result<Self::T, Error>;
}
impl<'a, T: patch_db::ModelMut<'a>> ModelMutExt<'a> for T {
    type T = <T as patch_db::ModelMut<'a>>::T;
    fn de(&self) -> Result<Self::T, Error> {
        self.get().with_kind(ErrorKind::Deserialization)
    }
    fn ser(&mut self, value: &Self::T) -> Result<(), Error> {
        self.set(value).with_kind(ErrorKind::Serialization)
    }
    fn mutate<U>(&mut self, f: impl FnOnce(&mut Self::T) -> Result<U, Error>) -> Result<U, Error> {
        let mut v = self.de()?;
        let res = f(&mut v)?;
        self.ser(&v)?;
        Ok(res)
    }
    fn map_mutate(
        &mut self,
        f: impl FnOnce(Self::T) -> Result<Self::T, Error>,
    ) -> Result<Self::T, Error> {
        let v = self.de()?;
        let v = f(v)?;
        self.ser(&v)?;
        Ok(v)
    }
}

pub trait Map: DeserializeOwned + Serialize {
    type Key;
    type Value;
}

pub struct MapModel<T> {
    value: Value,
    phantom: PhantomData<T>,
}
impl<T: Map> MapModel<T>
where
    T::Key: DeserializeOwned + Ord + Clone,
{
    pub fn keys(&self) -> Result<OrdSet<T::Key>, Error> {
        match &self.value {
            Value::Object(o) => o
                .keys()
                .cloned()
                .map(|k| from_value(Value::String(k)))
                .collect(),
            v => Err(Error::new(
                eyre!("expected object found {v}"),
                ErrorKind::Deserialization,
            )),
        }
    }
}
impl<T: Map> MapModel<T>
where
    T::Key: AsRef<str>,
    T::Value: HasModel,
{
    pub fn idx(&self, key: &T::Key) -> Option<<T::Value as HasModel>::Model> {
        match &self.value {
            Value::Object(o) => o
                .get(key.as_ref())
                .cloned()
                .map(|v| <<T::Value as HasModel>::Model as patch_db::Model>::new(v)),
            _ => None,
        }
    }
}
impl<T: Map> MapModel<T>
where
    T::Key: AsRef<str>,
    T::Value: Serialize,
{
    pub fn insert(&mut self, key: &T::Key, value: &T::Value) -> Result<(), Error> {
        let v = to_value(value)?;
        match &mut self.value {
            Value::Object(o) => {
                o.insert(Arc::new(key.as_ref().to_owned()), v);
                Ok(())
            }
            _ => Err(Error::new(
                eyre!("expected object found {v}"),
                ErrorKind::Serialization,
            )),
        }
    }
}
impl<T> Deref for MapModel<T> {
    type Target = Value;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
impl<T> DerefMut for MapModel<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}
impl<T: DeserializeOwned + Serialize> patch_db::Model for MapModel<T> {
    type T = T;
    type Mut<'a> = MapModelMut<'a, T> where T: 'a;
    fn new(value: Value) -> Self {
        Self {
            value,
            phantom: PhantomData,
        }
    }
    fn into_inner(self) -> Value {
        self.value
    }
}

pub struct MapModelMut<'a, T> {
    value: &'a mut Value,
    phantom: PhantomData<T>,
}
impl<'a, T: Map> MapModelMut<'a, T>
where
    T::Key: DeserializeOwned + Ord + Clone,
{
    pub fn keys(&self) -> Result<OrdSet<T::Key>, Error> {
        match &*self.value {
            Value::Object(o) => o
                .keys()
                .cloned()
                .map(|k| from_value(Value::String(k)))
                .collect(),
            v => Err(Error::new(
                eyre!("expected object found {v}"),
                ErrorKind::Deserialization,
            )),
        }
    }
}
impl<'a, T: Map> MapModelMut<'a, T>
where
    T::Key: AsRef<str>,
    T::Value: HasModel,
{
    pub fn idx(&self, key: &T::Key) -> Option<<T::Value as HasModel>::Model> {
        match &*self.value {
            Value::Object(o) => o
                .get(key.as_ref())
                .cloned()
                .map(|v| <<T::Value as HasModel>::Model as patch_db::Model>::new(v)),
            _ => None,
        }
    }
}
impl<'a, T: Map> MapModelMut<'a, T>
where
    T::Key: AsRef<str>,
    T::Value: Serialize,
{
    pub fn insert(&mut self, key: &T::Key, value: &T::Value) -> Result<(), Error> {
        let v = to_value(value)?;
        match self.value {
            Value::Object(o) => {
                o.insert(Arc::new(key.as_ref().to_owned()), v);
                Ok(())
            }
            _ => Err(Error::new(
                eyre!("expected object found {v}"),
                ErrorKind::Serialization,
            )),
        }
    }
}
impl<'a, T> Deref for MapModelMut<'a, T> {
    type Target = Value;
    fn deref(&self) -> &Self::Target {
        &*self.value
    }
}
impl<'a, T> DerefMut for MapModelMut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.value
    }
}
impl<'a, T: DeserializeOwned + Serialize> patch_db::ModelMut<'a> for MapModelMut<'a, T> {
    type T = T;
    fn new(value: &'a mut Value) -> Self {
        Self {
            value,
            phantom: PhantomData,
        }
    }
    fn into_inner(self) -> &'a mut Value {
        self.value
    }
}
