use std::panic::UnwindSafe;

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
        Ok(DatabaseModel::new(self.dump().await?.value))
    }
    async fn mutate<U: UnwindSafe + Send>(
        &self,
        f: impl FnOnce(&mut DatabaseModel) -> Result<U, Error> + UnwindSafe + Send,
    ) -> Result<U, Error> {
        Ok(self
            .apply_function(|v| {
                let mut model = DatabaseModel::new(&mut v);
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
        Ok(DatabaseModel::new(
            self.apply_function(|v| f(DatabaseModel::new(v)).map(|a| (a.into_inner(), ())))
                .await?
                .0,
        ))
    }
}

pub trait ModelExt {
    type T;
    fn de(&self) -> Result<Self::T, Error>;
    fn ser(&mut self, value: &Self::T) -> Result<(), Error>;
    fn mutate<U>(&mut self, f: impl FnOnce(&mut Self::T) -> Result<U, Error>) -> Result<U, Error>;
    fn map_mutate(
        &mut self,
        f: impl FnOnce(Self::T) -> Result<Self::T, Error>,
    ) -> Result<Self::T, Error>;
}
impl<T> ModelExt for patch_db::Model<T> {
    type T = T;
    fn de(&self) -> Result<Self::T, Error> {
        <Self as patch_db::ModelExt>::get(self).with_kind(ErrorKind::Deserialization)
    }
    fn ser(&mut self, value: &Self::T) -> Result<(), Error> {
        <Self as patch_db::ModelExt>::set(self, value).with_kind(ErrorKind::Serialization)
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
