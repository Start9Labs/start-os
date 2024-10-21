use std::marker::PhantomData;
use std::str::FromStr;

use rpc_toolkit::clap;
use rpc_toolkit::clap::builder::TypedValueParser;

pub struct FromStrParser<T>(PhantomData<T>);
impl<T> FromStrParser<T> {
    pub fn new() -> Self {
        Self(PhantomData)
    }
}
impl<T> Clone for FromStrParser<T> {
    fn clone(&self) -> Self {
        Self(PhantomData)
    }
}
impl<T> TypedValueParser for FromStrParser<T>
where
    T: FromStr + Clone + Send + Sync + 'static,
    T::Err: std::fmt::Display,
{
    type Value = T;
    fn parse_ref(
        &self,
        _: &clap::Command,
        _: Option<&clap::Arg>,
        value: &std::ffi::OsStr,
    ) -> Result<Self::Value, clap::Error> {
        value
            .to_string_lossy()
            .parse()
            .map_err(|e| clap::Error::raw(clap::error::ErrorKind::ValueValidation, e))
    }
}
