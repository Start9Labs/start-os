use std::any::TypeId;
use std::collections::VecDeque;
use std::marker::PhantomData;
use std::ops::Deref;
use std::str::FromStr;

use clap::builder::ValueParserFactory;
use clap::{ArgMatches, CommandFactory, FromArgMatches};
use color_eyre::eyre::eyre;
use imbl::OrdMap;
use openssl::pkey::{PKey, Private};
use openssl::x509::{X509Ref, X509};
use rpc_toolkit::{AnyContext, Handler, HandlerArgs, HandlerArgsFor, HandlerTypes, PrintCliResult};
use serde::de::DeserializeOwned;
use serde::ser::{SerializeMap, SerializeSeq};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::Value;

use super::IntoDoubleEndedIterator;
use crate::util::clap::FromStrParser;
use crate::{Error, ResultExt};

pub fn deserialize_from_str<
    'de,
    D: serde::de::Deserializer<'de>,
    T: FromStr<Err = E>,
    E: std::fmt::Display,
>(
    deserializer: D,
) -> std::result::Result<T, D::Error> {
    struct Visitor<T: FromStr<Err = E>, E>(std::marker::PhantomData<T>);
    impl<'de, T: FromStr<Err = Err>, Err: std::fmt::Display> serde::de::Visitor<'de>
        for Visitor<T, Err>
    {
        type Value = T;
        fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(formatter, "a parsable string")
        }
        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            v.parse().map_err(|e| serde::de::Error::custom(e))
        }
    }
    deserializer.deserialize_str(Visitor(std::marker::PhantomData))
}

pub fn deserialize_from_str_opt<
    'de,
    D: serde::de::Deserializer<'de>,
    T: FromStr<Err = E>,
    E: std::fmt::Display,
>(
    deserializer: D,
) -> std::result::Result<Option<T>, D::Error> {
    struct Visitor<T: FromStr<Err = E>, E>(std::marker::PhantomData<T>);
    impl<'de, T: FromStr<Err = Err>, Err: std::fmt::Display> serde::de::Visitor<'de>
        for Visitor<T, Err>
    {
        type Value = Option<T>;
        fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(formatter, "a parsable string")
        }
        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            v.parse().map(Some).map_err(|e| serde::de::Error::custom(e))
        }
        fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
        where
            D: serde::de::Deserializer<'de>,
        {
            deserializer.deserialize_str(Visitor(std::marker::PhantomData))
        }
        fn visit_none<E>(self) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(None)
        }
        fn visit_unit<E>(self) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(None)
        }
    }
    deserializer.deserialize_any(Visitor(std::marker::PhantomData))
}

pub fn serialize_display<T: std::fmt::Display, S: Serializer>(
    t: &T,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    String::serialize(&t.to_string(), serializer)
}

pub fn serialize_display_opt<T: std::fmt::Display, S: Serializer>(
    t: &Option<T>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    Option::<String>::serialize(&t.as_ref().map(|t| t.to_string()), serializer)
}

pub mod ed25519_pubkey {
    use ed25519_dalek::VerifyingKey;
    use serde::de::{Error, Unexpected, Visitor};
    use serde::{Deserializer, Serializer};

    pub fn serialize<S: Serializer>(
        pubkey: &VerifyingKey,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&base32::encode(
            base32::Alphabet::RFC4648 { padding: true },
            pubkey.as_bytes(),
        ))
    }
    pub fn deserialize<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<VerifyingKey, D::Error> {
        struct PubkeyVisitor;
        impl<'de> Visitor<'de> for PubkeyVisitor {
            type Value = ed25519_dalek::VerifyingKey;
            fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(formatter, "an RFC4648 encoded string")
            }
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                VerifyingKey::from_bytes(
                    &<[u8; 32]>::try_from(
                        base32::decode(base32::Alphabet::RFC4648 { padding: true }, v).ok_or(
                            Error::invalid_value(Unexpected::Str(v), &"an RFC4648 encoded string"),
                        )?,
                    )
                    .map_err(|e| Error::invalid_length(e.len(), &"32 bytes"))?,
                )
                .map_err(Error::custom)
            }
        }
        deserializer.deserialize_str(PubkeyVisitor)
    }
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum ValuePrimative {
    Null,
    Boolean(bool),
    String(String),
    Number(serde_json::Number),
}
impl<'de> serde::de::Deserialize<'de> for ValuePrimative {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = ValuePrimative;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "a JSON primative value")
            }
            fn visit_unit<E>(self) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Null)
            }
            fn visit_none<E>(self) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Null)
            }
            fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Boolean(v))
            }
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::String(v.to_owned()))
            }
            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::String(v))
            }
            fn visit_f32<E>(self, v: f32) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(
                    serde_json::Number::from_f64(v as f64).ok_or_else(|| {
                        serde::de::Error::invalid_value(
                            serde::de::Unexpected::Float(v as f64),
                            &"a finite number",
                        )
                    })?,
                ))
            }
            fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(
                    serde_json::Number::from_f64(v).ok_or_else(|| {
                        serde::de::Error::invalid_value(
                            serde::de::Unexpected::Float(v),
                            &"a finite number",
                        )
                    })?,
                ))
            }
            fn visit_u8<E>(self, v: u8) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(v.into()))
            }
            fn visit_u16<E>(self, v: u16) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(v.into()))
            }
            fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(v.into()))
            }
            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(v.into()))
            }
            fn visit_i8<E>(self, v: i8) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(v.into()))
            }
            fn visit_i16<E>(self, v: i16) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(v.into()))
            }
            fn visit_i32<E>(self, v: i32) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(v.into()))
            }
            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ValuePrimative::Number(v.into()))
            }
        }
        deserializer.deserialize_any(Visitor)
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "kebab-case")]
pub enum IoFormat {
    Json,
    JsonPretty,
    Yaml,
    Cbor,
    Toml,
    TomlPretty,
}
impl Default for IoFormat {
    fn default() -> Self {
        IoFormat::JsonPretty
    }
}
impl std::fmt::Display for IoFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use IoFormat::*;
        match self {
            Json => write!(f, "JSON"),
            JsonPretty => write!(f, "JSON (pretty)"),
            Yaml => write!(f, "YAML"),
            Cbor => write!(f, "CBOR"),
            Toml => write!(f, "TOML"),
            TomlPretty => write!(f, "TOML (pretty)"),
        }
    }
}
impl std::str::FromStr for IoFormat {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        serde_json::from_value(Value::String(s.to_owned()))
            .with_kind(crate::ErrorKind::Deserialization)
    }
}
impl IoFormat {
    pub fn to_writer<W: std::io::Write, T: Serialize>(
        &self,
        mut writer: W,
        value: &T,
    ) -> Result<(), Error> {
        match self {
            IoFormat::Json => {
                serde_json::to_writer(writer, value).with_kind(crate::ErrorKind::Serialization)
            }
            IoFormat::JsonPretty => serde_json::to_writer_pretty(writer, value)
                .with_kind(crate::ErrorKind::Serialization),
            IoFormat::Yaml => {
                serde_yaml::to_writer(writer, value).with_kind(crate::ErrorKind::Serialization)
            }
            IoFormat::Cbor => serde_cbor::ser::into_writer(value, writer)
                .with_kind(crate::ErrorKind::Serialization),
            IoFormat::Toml => writer
                .write_all(
                    serde_toml::to_string(
                        &serde_toml::Value::try_from(value)
                            .with_kind(crate::ErrorKind::Serialization)?,
                    )
                    .with_kind(crate::ErrorKind::Serialization)?
                    .as_bytes(),
                )
                .with_kind(crate::ErrorKind::Serialization),
            IoFormat::TomlPretty => writer
                .write_all(
                    serde_toml::to_string_pretty(
                        &serde_toml::Value::try_from(value)
                            .with_kind(crate::ErrorKind::Serialization)?,
                    )
                    .with_kind(crate::ErrorKind::Serialization)?
                    .as_bytes(),
                )
                .with_kind(crate::ErrorKind::Serialization),
        }
    }
    pub fn to_vec<T: Serialize>(&self, value: &T) -> Result<Vec<u8>, Error> {
        match self {
            IoFormat::Json => serde_json::to_vec(value).with_kind(crate::ErrorKind::Serialization),
            IoFormat::JsonPretty => {
                serde_json::to_vec_pretty(value).with_kind(crate::ErrorKind::Serialization)
            }
            IoFormat::Yaml => serde_yaml::to_string(value)
                .with_kind(crate::ErrorKind::Serialization)
                .map(|s| s.into_bytes()),
            IoFormat::Cbor => {
                let mut res = Vec::new();
                serde_cbor::ser::into_writer(value, &mut res)
                    .with_kind(crate::ErrorKind::Serialization)?;
                Ok(res)
            }
            IoFormat::Toml => serde_toml::to_string(
                &serde_toml::Value::try_from(value).with_kind(crate::ErrorKind::Serialization)?,
            )
            .with_kind(crate::ErrorKind::Serialization)
            .map(|s| s.into_bytes()),
            IoFormat::TomlPretty => serde_toml::to_string_pretty(
                &serde_toml::Value::try_from(value).with_kind(crate::ErrorKind::Serialization)?,
            )
            .map(|s| s.into_bytes())
            .with_kind(crate::ErrorKind::Serialization),
        }
    }
    /// BLOCKING
    pub fn from_reader<R: std::io::Read, T: for<'de> Deserialize<'de>>(
        &self,
        mut reader: R,
    ) -> Result<T, Error> {
        match self {
            IoFormat::Json | IoFormat::JsonPretty => {
                serde_json::from_reader(reader).with_kind(crate::ErrorKind::Deserialization)
            }
            IoFormat::Yaml => {
                serde_yaml::from_reader(reader).with_kind(crate::ErrorKind::Deserialization)
            }
            IoFormat::Cbor => {
                serde_cbor::de::from_reader(reader).with_kind(crate::ErrorKind::Deserialization)
            }
            IoFormat::Toml | IoFormat::TomlPretty => {
                let mut s = String::new();
                reader
                    .read_to_string(&mut s)
                    .with_kind(crate::ErrorKind::Deserialization)?;
                serde_toml::from_str(&s).with_kind(crate::ErrorKind::Deserialization)
            }
        }
    }
    pub async fn from_async_reader<
        R: tokio::io::AsyncRead + Unpin,
        T: for<'de> Deserialize<'de>,
    >(
        &self,
        reader: R,
    ) -> Result<T, Error> {
        use crate::util::io::*;
        match self {
            IoFormat::Json | IoFormat::JsonPretty => from_json_async_reader(reader).await,
            IoFormat::Yaml => from_yaml_async_reader(reader).await,
            IoFormat::Cbor => from_cbor_async_reader(reader).await,
            IoFormat::Toml | IoFormat::TomlPretty => from_toml_async_reader(reader).await,
        }
    }
    pub fn from_slice<T: for<'de> Deserialize<'de>>(&self, slice: &[u8]) -> Result<T, Error> {
        match self {
            IoFormat::Json | IoFormat::JsonPretty => {
                serde_json::from_slice(slice).with_kind(crate::ErrorKind::Deserialization)
            }
            IoFormat::Yaml => {
                serde_yaml::from_slice(slice).with_kind(crate::ErrorKind::Deserialization)
            }
            IoFormat::Cbor => {
                serde_cbor::de::from_reader(slice).with_kind(crate::ErrorKind::Deserialization)
            }
            IoFormat::Toml | IoFormat::TomlPretty => {
                serde_toml::from_str(std::str::from_utf8(slice)?)
                    .with_kind(crate::ErrorKind::Deserialization)
            }
        }
    }
}

pub fn display_serializable<T: Serialize>(format: IoFormat, result: T) {
    format
        .to_writer(std::io::stdout(), &result)
        .expect("Error serializing result to stdout");
    if format == IoFormat::JsonPretty {
        println!()
    }
}

#[derive(Deserialize, Serialize)]
pub struct WithIoFormat<T> {
    pub format: Option<IoFormat>,
    #[serde(flatten)]
    pub rest: T,
}
impl<T: FromArgMatches> FromArgMatches for WithIoFormat<T> {
    fn from_arg_matches(matches: &ArgMatches) -> Result<Self, clap::Error> {
        Ok(Self {
            rest: T::from_arg_matches(matches)?,
            format: matches.get_one("format").copied(),
        })
    }
    fn update_from_arg_matches(&mut self, matches: &ArgMatches) -> Result<(), clap::Error> {
        self.rest.update_from_arg_matches(matches)?;
        self.format = matches.get_one("format").copied();
        Ok(())
    }
}
impl<T: CommandFactory> CommandFactory for WithIoFormat<T> {
    fn command() -> clap::Command {
        let cmd = T::command();
        if !cmd.get_arguments().any(|a| a.get_id() == "format") {
            cmd.arg(
                clap::Arg::new("format")
                    .long("format")
                    .value_parser(|s: &str| s.parse::<IoFormat>().map_err(|e| eyre!("{e}"))),
            )
        } else {
            cmd
        }
    }
    fn command_for_update() -> clap::Command {
        let cmd = T::command_for_update();
        if !cmd.get_arguments().any(|a| a.get_id() == "format") {
            cmd.arg(
                clap::Arg::new("format")
                    .long("format")
                    .value_parser(|s: &str| s.parse::<IoFormat>().map_err(|e| eyre!("{e}"))),
            )
        } else {
            cmd
        }
    }
}

pub trait HandlerExtSerde: Handler {
    fn with_display_serializable(self) -> DisplaySerializable<Self>;
}
impl<T: Handler> HandlerExtSerde for T {
    fn with_display_serializable(self) -> DisplaySerializable<Self> {
        DisplaySerializable(self)
    }
}

#[derive(Debug, Clone)]
pub struct DisplaySerializable<T>(pub T);
impl<T: HandlerTypes> HandlerTypes for DisplaySerializable<T> {
    type Params = WithIoFormat<T::Params>;
    type InheritedParams = T::InheritedParams;
    type Ok = T::Ok;
    type Err = T::Err;
}
#[async_trait::async_trait]
impl<T: Handler> Handler for DisplaySerializable<T> {
    type Context = T::Context;
    fn handle_sync(
        &self,
        HandlerArgs {
            context,
            parent_method,
            method,
            params,
            inherited_params,
            raw_params,
        }: HandlerArgsFor<Self::Context, Self>,
    ) -> Result<Self::Ok, Self::Err> {
        self.0.handle_sync(HandlerArgs {
            context,
            parent_method,
            method,
            params: params.rest,
            inherited_params,
            raw_params,
        })
    }
    async fn handle_async(
        &self,
        HandlerArgs {
            context,
            parent_method,
            method,
            params,
            inherited_params,
            raw_params,
        }: HandlerArgsFor<Self::Context, Self>,
    ) -> Result<Self::Ok, Self::Err> {
        self.0
            .handle_async(HandlerArgs {
                context,
                parent_method,
                method,
                params: params.rest,
                inherited_params,
                raw_params,
            })
            .await
    }
    fn contexts(&self) -> Option<imbl::OrdSet<std::any::TypeId>> {
        self.0.contexts()
    }
    fn metadata(
        &self,
        method: VecDeque<&'static str>,
        ctx_ty: TypeId,
    ) -> OrdMap<&'static str, imbl_value::Value> {
        self.0.metadata(method, ctx_ty)
    }
    fn method_from_dots(&self, method: &str, ctx_ty: TypeId) -> Option<VecDeque<&'static str>> {
        self.0.method_from_dots(method, ctx_ty)
    }
}
impl<T: HandlerTypes> PrintCliResult for DisplaySerializable<T>
where
    T::Ok: Serialize,
{
    type Context = AnyContext;
    fn print(
        &self,
        HandlerArgs { params, .. }: HandlerArgsFor<Self::Context, Self>,
        result: Self::Ok,
    ) -> Result<(), Self::Err> {
        display_serializable(params.format.unwrap_or_default(), result);
        Ok(())
    }
}

#[derive(Deserialize, Serialize)]
pub struct StdinDeserializable<T>(pub T);
impl<T> FromArgMatches for StdinDeserializable<T>
where
    T: DeserializeOwned,
{
    fn from_arg_matches(matches: &ArgMatches) -> Result<Self, clap::Error> {
        let format = matches
            .get_one::<IoFormat>("format")
            .copied()
            .unwrap_or_default();
        Ok(Self(format.from_reader(&mut std::io::stdin()).map_err(
            |e| clap::Error::raw(clap::error::ErrorKind::ValueValidation, e),
        )?))
    }
    fn update_from_arg_matches(&mut self, matches: &ArgMatches) -> Result<(), clap::Error> {
        let format = matches
            .get_one::<IoFormat>("format")
            .copied()
            .unwrap_or_default();
        self.0 = format
            .from_reader(&mut std::io::stdin())
            .map_err(|e| clap::Error::raw(clap::error::ErrorKind::ValueValidation, e))?;
        Ok(())
    }
}
impl<T> clap::Args for StdinDeserializable<T>
where
    T: DeserializeOwned,
{
    fn augment_args(cmd: clap::Command) -> clap::Command {
        if !cmd.get_arguments().any(|a| a.get_id() == "format") {
            cmd.arg(
                clap::Arg::new("format")
                    .long("format")
                    .value_parser(|s: &str| s.parse::<IoFormat>().map_err(|e| eyre!("{e}"))),
            )
        } else {
            cmd
        }
    }
    fn augment_args_for_update(cmd: clap::Command) -> clap::Command {
        if !cmd.get_arguments().any(|a| a.get_id() == "format") {
            cmd.arg(
                clap::Arg::new("format")
                    .long("format")
                    .value_parser(|s: &str| s.parse::<IoFormat>().map_err(|e| eyre!("{e}"))),
            )
        } else {
            cmd
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Duration(std::time::Duration);
impl Deref for Duration {
    type Target = std::time::Duration;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl From<std::time::Duration> for Duration {
    fn from(t: std::time::Duration) -> Self {
        Duration(t)
    }
}
impl std::str::FromStr for Duration {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let units_idx = s.find(|c: char| c.is_alphabetic()).ok_or_else(|| {
            Error::new(
                eyre!("Must specify units for duration"),
                crate::ErrorKind::Deserialization,
            )
        })?;
        let (num, units) = s.split_at(units_idx);
        use std::time::Duration;
        Ok(Duration(match units {
            "d" if num.contains(".") => Duration::from_secs_f64(num.parse::<f64>()? * 86_400_f64),
            "d" => Duration::from_secs(num.parse::<u64>()? * 86_400),
            "h" if num.contains(".") => Duration::from_secs_f64(num.parse::<f64>()? * 3_600_f64),
            "h" => Duration::from_secs(num.parse::<u64>()? * 3_600),
            "m" if num.contains(".") => Duration::from_secs_f64(num.parse::<f64>()? * 60_f64),
            "m" => Duration::from_secs(num.parse::<u64>()? * 60),
            "s" if num.contains(".") => Duration::from_secs_f64(num.parse()?),
            "s" => Duration::from_secs(num.parse()?),
            "ms" if num.contains(".") => Duration::from_secs_f64(num.parse::<f64>()? / 1_000_f64),
            "ms" => {
                let millis: u128 = num.parse()?;
                Duration::new((millis / 1_000) as u64, (millis % 1_000) as u32)
            }
            "us" | "µs" if num.contains(".") => {
                Duration::from_secs_f64(num.parse::<f64>()? / 1_000_000_f64)
            }
            "us" | "µs" => {
                let micros: u128 = num.parse()?;
                Duration::new((micros / 1_000_000) as u64, (micros % 1_000_000) as u32)
            }
            "ns" if num.contains(".") => {
                Duration::from_secs_f64(num.parse::<f64>()? / 1_000_000_000_f64)
            }
            "ns" => {
                let nanos: u128 = num.parse()?;
                Duration::new(
                    (nanos / 1_000_000_000) as u64,
                    (nanos % 1_000_000_000) as u32,
                )
            }
            _ => {
                return Err(Error::new(
                    eyre!("Invalid units for duration"),
                    crate::ErrorKind::Deserialization,
                ))
            }
        }))
    }
}
impl ValueParserFactory for Duration {
    type Parser = FromStrParser<Self>;
    fn value_parser() -> Self::Parser {
        FromStrParser::new()
    }
}
impl std::fmt::Display for Duration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let nanos = self.as_nanos();
        match () {
            _ if nanos % 86_400_000_000_000 == 0 => write!(f, "{}d", nanos / 86_400_000_000_000),
            _ if nanos % 3_600_000_000_000 == 0 => write!(f, "{}h", nanos / 3_600_000_000_000),
            _ if nanos % 60_000_000_000 == 0 => write!(f, "{}m", nanos / 60_000_000_000),
            _ if nanos % 1_000_000_000 == 0 => write!(f, "{}s", nanos / 1_000_000_000),
            _ if nanos % 1_000_000 == 0 => write!(f, "{}ms", nanos / 1_000_000),
            _ if nanos % 1_000 == 0 => write!(f, "{}µs", nanos / 1_000),
            _ => write!(f, "{}ns", nanos),
        }
    }
}
impl<'de> Deserialize<'de> for Duration {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserialize_from_str(deserializer)
    }
}
impl Serialize for Duration {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serialize_display(self, serializer)
    }
}

pub fn deserialize_number_permissive<
    'de,
    D: serde::de::Deserializer<'de>,
    T: FromStr<Err = E> + num::cast::FromPrimitive,
    E: std::fmt::Display,
>(
    deserializer: D,
) -> std::result::Result<T, D::Error> {
    struct Visitor<T: FromStr<Err = E> + num::cast::FromPrimitive, E>(std::marker::PhantomData<T>);
    impl<'de, T: FromStr<Err = Err> + num::cast::FromPrimitive, Err: std::fmt::Display>
        serde::de::Visitor<'de> for Visitor<T, Err>
    {
        type Value = T;
        fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(formatter, "a parsable string")
        }
        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            v.parse().map_err(|e| serde::de::Error::custom(e))
        }
        fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            T::from_f64(v).ok_or_else(|| {
                serde::de::Error::custom(format!(
                    "{} cannot be represented by the requested type",
                    v
                ))
            })
        }
        fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            T::from_u64(v).ok_or_else(|| {
                serde::de::Error::custom(format!(
                    "{} cannot be represented by the requested type",
                    v
                ))
            })
        }
        fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            T::from_i64(v).ok_or_else(|| {
                serde::de::Error::custom(format!(
                    "{} cannot be represented by the requested type",
                    v
                ))
            })
        }
    }
    deserializer.deserialize_str(Visitor(std::marker::PhantomData))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Port(pub u16);
impl<'de> Deserialize<'de> for Port {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        //TODO: if number, be permissive
        deserialize_number_permissive(deserializer).map(Port)
    }
}
impl Serialize for Port {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serialize_display(&self.0, serializer)
    }
}

#[derive(Debug, Clone)]
pub struct Reversible<T, Container = Vec<T>>
where
    for<'a> &'a Container: IntoDoubleEndedIterator<&'a T>,
{
    reversed: bool,
    data: Container,
    phantom: PhantomData<T>,
}
impl<T, Container> Reversible<T, Container>
where
    for<'a> &'a Container: IntoDoubleEndedIterator<&'a T>,
{
    pub fn new(data: Container) -> Self {
        Reversible {
            reversed: false,
            data,
            phantom: PhantomData,
        }
    }

    pub fn reverse(&mut self) {
        self.reversed = !self.reversed
    }

    pub fn iter(
        &self,
    ) -> itertools::Either<
        <&Container as IntoDoubleEndedIterator<&T>>::IntoIter,
        std::iter::Rev<<&Container as IntoDoubleEndedIterator<&T>>::IntoIter>,
    > {
        let iter = IntoDoubleEndedIterator::into_iter(&self.data);
        if self.reversed {
            itertools::Either::Right(iter.rev())
        } else {
            itertools::Either::Left(iter)
        }
    }
}
impl<T, Container> Serialize for Reversible<T, Container>
where
    for<'a> &'a Container: IntoDoubleEndedIterator<&'a T>,
    T: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let iter = IntoDoubleEndedIterator::into_iter(&self.data);
        let mut seq_ser = serializer.serialize_seq(match iter.size_hint() {
            (lower, Some(upper)) if lower == upper => Some(upper),
            _ => None,
        })?;
        if self.reversed {
            for elem in iter.rev() {
                seq_ser.serialize_element(elem)?;
            }
        } else {
            for elem in iter {
                seq_ser.serialize_element(elem)?;
            }
        }
        seq_ser.end()
    }
}
impl<'de, T, Container> Deserialize<'de> for Reversible<T, Container>
where
    for<'a> &'a Container: IntoDoubleEndedIterator<&'a T>,
    Container: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(Reversible::new(Deserialize::deserialize(deserializer)?))
    }
    fn deserialize_in_place<D>(deserializer: D, place: &mut Self) -> Result<(), D::Error>
    where
        D: Deserializer<'de>,
    {
        Deserialize::deserialize_in_place(deserializer, &mut place.data)
    }
}

pub struct KeyVal<K, V> {
    pub key: K,
    pub value: V,
}
impl<K: Serialize, V: Serialize> Serialize for KeyVal<K, V> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        map.serialize_entry(&self.key, &self.value)?;
        map.end()
    }
}
impl<'de, K: Deserialize<'de>, V: Deserialize<'de>> Deserialize<'de> for KeyVal<K, V> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor<K, V>(PhantomData<(K, V)>);
        impl<'de, K: Deserialize<'de>, V: Deserialize<'de>> serde::de::Visitor<'de> for Visitor<K, V> {
            type Value = KeyVal<K, V>;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "A map with a single element")
            }
            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                let (key, value) = map
                    .next_entry()?
                    .ok_or_else(|| serde::de::Error::invalid_length(0, &"1"))?;
                Ok(KeyVal { key, value })
            }
        }
        deserializer.deserialize_map(Visitor(PhantomData))
    }
}

pub struct Base32<T>(pub T);
impl<'de, T: TryFrom<Vec<u8>>> Deserialize<'de> for Base32<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        base32::decode(base32::Alphabet::RFC4648 { padding: true }, &s)
            .ok_or_else(|| {
                serde::de::Error::invalid_value(
                    serde::de::Unexpected::Str(&s),
                    &"a valid base32 string",
                )
            })?
            .try_into()
            .map_err(|_| serde::de::Error::custom("invalid length"))
            .map(Self)
    }
}
impl<T: AsRef<[u8]>> Serialize for Base32<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&base32::encode(
            base32::Alphabet::RFC4648 { padding: true },
            self.0.as_ref(),
        ))
    }
}

pub struct Base64<T>(pub T);
impl<'de, T: TryFrom<Vec<u8>>> Deserialize<'de> for Base64<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        base64::decode(&s)
            .map_err(serde::de::Error::custom)?
            .try_into()
            .map_err(|_| serde::de::Error::custom("invalid length"))
            .map(Self)
    }
}
impl<T: AsRef<[u8]>> Serialize for Base64<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&base64::encode(self.0.as_ref()))
    }
}

#[derive(Clone, Debug)]
pub struct Regex(regex::Regex);
impl From<Regex> for regex::Regex {
    fn from(value: Regex) -> Self {
        value.0
    }
}
impl From<regex::Regex> for Regex {
    fn from(value: regex::Regex) -> Self {
        Regex(value)
    }
}
impl AsRef<regex::Regex> for Regex {
    fn as_ref(&self) -> &regex::Regex {
        &self.0
    }
}
impl AsMut<regex::Regex> for Regex {
    fn as_mut(&mut self) -> &mut regex::Regex {
        &mut self.0
    }
}
impl<'de> Deserialize<'de> for Regex {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserialize_from_str(deserializer).map(Self)
    }
}
impl Serialize for Regex {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serialize_display(&self.0, serializer)
    }
}

// TODO: make this not allocate
#[derive(Debug)]
pub struct NoOutput;
impl<'de> Deserialize<'de> for NoOutput {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let _ = Value::deserialize(deserializer);
        Ok(NoOutput)
    }
}

pub fn apply_expr(input: jaq_core::Val, expr: &str) -> Result<jaq_core::Val, Error> {
    let (expr, errs) = jaq_core::parse::parse(expr, jaq_core::parse::main());

    let Some(expr) = expr else {
        return Err(Error::new(
            eyre!("Failed to parse expression: {:?}", errs),
            crate::ErrorKind::InvalidRequest,
        ));
    };

    let mut errs = Vec::new();

    let mut defs = jaq_core::Definitions::core();
    for def in jaq_std::std() {
        defs.insert(def, &mut errs);
    }

    let filter = defs.finish(expr, Vec::new(), &mut errs);

    if !errs.is_empty() {
        return Err(Error::new(
            eyre!("Failed to compile expression: {:?}", errs),
            crate::ErrorKind::InvalidRequest,
        ));
    };

    let inputs = jaq_core::RcIter::new(std::iter::empty());
    let mut res_iter = filter.run(jaq_core::Ctx::new([], &inputs), input);

    let Some(res) = res_iter
        .next()
        .transpose()
        .map_err(|e| eyre!("{e}"))
        .with_kind(crate::ErrorKind::Deserialization)?
    else {
        return Err(Error::new(
            eyre!("expr returned no results"),
            crate::ErrorKind::InvalidRequest,
        ));
    };

    if res_iter.next().is_some() {
        return Err(Error::new(
            eyre!("expr returned too many results"),
            crate::ErrorKind::InvalidRequest,
        ));
    }

    Ok(res)
}

pub trait PemEncoding: Sized {
    fn from_pem<E: serde::de::Error>(pem: &str) -> Result<Self, E>;
    fn to_pem<E: serde::ser::Error>(&self) -> Result<String, E>;
}

impl PemEncoding for X509 {
    fn from_pem<E: serde::de::Error>(pem: &str) -> Result<Self, E> {
        X509::from_pem(pem.as_bytes()).map_err(E::custom)
    }
    fn to_pem<E: serde::ser::Error>(&self) -> Result<String, E> {
        String::from_utf8((&**self).to_pem().map_err(E::custom)?).map_err(E::custom)
    }
}

impl PemEncoding for PKey<Private> {
    fn from_pem<E: serde::de::Error>(pem: &str) -> Result<Self, E> {
        PKey::<Private>::private_key_from_pem(pem.as_bytes()).map_err(E::custom)
    }
    fn to_pem<E: serde::ser::Error>(&self) -> Result<String, E> {
        String::from_utf8((&**self).private_key_to_pem_pkcs8().map_err(E::custom)?)
            .map_err(E::custom)
    }
}

impl PemEncoding for ssh_key::PrivateKey {
    fn from_pem<E: serde::de::Error>(pem: &str) -> Result<Self, E> {
        ssh_key::PrivateKey::from_openssh(pem.as_bytes()).map_err(E::custom)
    }
    fn to_pem<E: serde::ser::Error>(&self) -> Result<String, E> {
        self.to_openssh(ssh_key::LineEnding::LF)
            .map_err(E::custom)
            .map(|s| (&*s).clone())
    }
}

pub mod pem {
    use serde::{Deserialize, Deserializer, Serializer};

    use crate::util::serde::PemEncoding;

    pub fn serialize<T: PemEncoding, S: Serializer>(
        value: &T,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&value.to_pem()?)
    }

    pub fn deserialize<'de, T: PemEncoding, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<T, D::Error> {
        let pem = String::deserialize(deserializer)?;
        Ok(T::from_pem(&pem)?)
    }
}

#[repr(transparent)]
#[derive(Debug, Deserialize, Serialize)]
pub struct Pem<T: PemEncoding>(#[serde(with = "pem")] pub T);
impl<T: PemEncoding> Pem<T> {
    pub fn new(value: T) -> Self {
        Pem(value)
    }
    pub fn new_ref(value: &T) -> &Self {
        unsafe { std::mem::transmute(value) }
    }
    pub fn new_mut(value: &mut T) -> &mut Self {
        unsafe { std::mem::transmute(value) }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct MaybeUtf8String(pub Vec<u8>);
impl std::fmt::Debug for MaybeUtf8String {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Ok(s) = std::str::from_utf8(&self.0) {
            s.fmt(f)
        } else {
            self.0.fmt(f)
        }
    }
}
impl<'de> Deserialize<'de> for MaybeUtf8String {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = Vec<u8>;
            fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(formatter, "a string or byte array")
            }
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v.as_bytes().to_owned())
            }
            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v.into_bytes())
            }
            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v.to_owned())
            }
            fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v)
            }
            fn visit_unit<E>(self) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Vec::new())
            }
            fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                std::iter::repeat_with(|| seq.next_element::<u8>().transpose())
                    .take_while(|a| a.is_some())
                    .flatten()
                    .collect::<Result<Vec<u8>, _>>()
            }
        }
        deserializer.deserialize_any(Visitor).map(Self)
    }
}
impl Serialize for MaybeUtf8String {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if let Ok(s) = std::str::from_utf8(&self.0) {
            serializer.serialize_str(s)
        } else {
            serializer.serialize_bytes(&self.0)
        }
    }
}
