use std::panic::UnwindSafe;

use color_eyre::eyre::eyre;
use patch_db::HasModel;
pub use patch_db::PatchDb;

use crate::db::model::DatabaseModel;
use crate::prelude::*;

#[async_trait::async_trait]
pub trait PatchDbExt {
    async fn apply_fn<F: FnOnce(DatabaseModel) -> Result<T, Error> + Send + UnwindSafe, T: Send>(
        &self,
        f: F,
    ) -> Result<T, Error>;
}
#[async_trait::async_trait]
impl PatchDbExt for PatchDb {
    async fn apply_fn<F: FnOnce(DatabaseModel) -> Result<T, Error> + Send + UnwindSafe, T: Send>(
        &self,
        f: F,
    ) -> Result<T, Error> {
        self.apply_func(|v| f(<DatabaseModel as patch_db::Model>::new(v)))
            .await
    }
}

pub trait ModelExt<'a>: patch_db::Model<'a> {
    fn get(&'a self) -> Result<<Self as patch_db::Model<'a>>::T, Error> {
        patch_db::Model::get(self).with_kind(ErrorKind::Deserialization)
    }
    fn set(&'a mut self, value: &<Self as patch_db::Model<'a>>::T) -> Result<(), Error> {
        patch_db::Model::set(self, value).with_kind(ErrorKind::Serialization)
    }
    fn apply_fn<F: FnOnce(&mut <Self as patch_db::Model<'a>>::T) -> Result<T, Error>, T>(
        &'a mut self,
        f: F,
    ) -> Result<T, Error> {
        let mut value = <Self as ModelExt<'a>>::get(self)?;
        let res = f(&mut value)?;
        <Self as ModelExt<'a>>::set(self, &value)?;
        Ok(res)
    }
}
impl<'a, T: patch_db::Model<'a>> ModelExt<'a> for T {}

pub trait MapModelExt<'a, T: patch_db::Map<'a>> {
    fn expect(&'a mut self, key: &T::Key) -> Result<<T::Value as HasModel<'a>>::Model, Error>;
}
impl<'a, T: patch_db::Map<'a>> MapModelExt<'a, T> for patch_db::MapModel<'a, T> {
    fn expect(&'a mut self, key: &T::Key) -> Result<<T::Value as HasModel<'a>>::Model, Error> {
        self.idx(key)
            .ok_or_else(|| Error::new(eyre!("{}", key.as_ref()), ErrorKind::NotFound))
    }
}
