use std::future::Future;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::ops::Deref;
use std::path::Path;
use std::process::{exit, Stdio};
use std::str::FromStr;
use std::time::Duration;

use anyhow::anyhow;
use async_trait::async_trait;
use clap::ArgMatches;
use digest::Digest;
use patch_db::HasModel;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::Value;
use tokio::fs::File;
use tokio::sync::RwLock;
use tokio::task::{JoinError, JoinHandle};

use crate::shutdown::Shutdown;
use crate::{Error, ResultExt as _};

pub mod io;
pub mod logger;

#[derive(Clone, Copy, Debug)]
pub enum Never {}
impl Never {}
impl Never {
    pub fn absurd<T>(self) -> T {
        match self {}
    }
}
impl std::fmt::Display for Never {
    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.absurd()
    }
}
impl std::error::Error for Never {}

#[async_trait::async_trait]
pub trait Invoke {
    async fn invoke(&mut self, error_kind: crate::ErrorKind) -> Result<Vec<u8>, Error>;
}
#[async_trait::async_trait]
impl Invoke for tokio::process::Command {
    async fn invoke(&mut self, error_kind: crate::ErrorKind) -> Result<Vec<u8>, Error> {
        self.stdout(Stdio::piped());
        self.stderr(Stdio::piped());
        let res = self.output().await?;
        crate::ensure_code!(
            res.status.success(),
            error_kind,
            "{}",
            std::str::from_utf8(&res.stderr).unwrap_or("Unknown Error")
        );
        Ok(res.stdout)
    }
}

pub trait Apply: Sized {
    fn apply<O, F: FnOnce(Self) -> O>(self, func: F) -> O {
        func(self)
    }
}

pub trait ApplyRef {
    fn apply_ref<O, F: FnOnce(&Self) -> O>(&self, func: F) -> O {
        func(&self)
    }

    fn apply_mut<O, F: FnOnce(&mut Self) -> O>(&mut self, func: F) -> O {
        func(self)
    }
}

impl<T> Apply for T {}
impl<T> ApplyRef for T {}

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

pub async fn daemon<F: FnMut() -> Fut, Fut: Future<Output = ()> + Send + 'static>(
    mut f: F,
    cooldown: std::time::Duration,
    mut shutdown: tokio::sync::broadcast::Receiver<Option<Shutdown>>,
) -> Result<(), anyhow::Error> {
    loop {
        tokio::select! {
            _ = shutdown.recv() => return Ok(()),
            _ = tokio::time::sleep(cooldown) => (),
        }
        match tokio::spawn(f()).await {
            Err(e) if e.is_panic() => return Err(anyhow!("daemon panicked!")),
            _ => (),
        }
    }
}

pub trait SOption<T> {}
pub struct SSome<T>(T);
impl<T> SSome<T> {
    pub fn into(self) -> T {
        self.0
    }
}
impl<T> From<T> for SSome<T> {
    fn from(t: T) -> Self {
        SSome(t)
    }
}
impl<T> SOption<T> for SSome<T> {}
pub struct SNone<T>(PhantomData<T>);
impl<T> SNone<T> {
    pub fn new() -> Self {
        SNone(PhantomData)
    }
}
impl<T> SOption<T> for SNone<T> {}

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

#[derive(Debug, Clone, HasModel)]
pub struct Version {
    version: emver::Version,
    string: String,
}
impl Version {
    pub fn as_str(&self) -> &str {
        self.string.as_str()
    }
}
impl std::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
}
impl std::str::FromStr for Version {
    type Err = <emver::Version as FromStr>::Err;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Version {
            string: s.to_owned(),
            version: s.parse()?,
        })
    }
}
impl From<emver::Version> for Version {
    fn from(v: emver::Version) -> Self {
        Version {
            string: v.to_string(),
            version: v,
        }
    }
}
impl From<Version> for emver::Version {
    fn from(v: Version) -> Self {
        v.version
    }
}
impl Deref for Version {
    type Target = emver::Version;
    fn deref(&self) -> &Self::Target {
        &self.version
    }
}
impl AsRef<emver::Version> for Version {
    fn as_ref(&self) -> &emver::Version {
        &self.version
    }
}
impl AsRef<str> for Version {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}
impl PartialEq for Version {
    fn eq(&self, other: &Version) -> bool {
        self.version.eq(&other.version)
    }
}
impl Eq for Version {}
impl Hash for Version {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.version.hash(state)
    }
}
impl<'de> Deserialize<'de> for Version {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let string = String::deserialize(deserializer)?;
        let version = emver::Version::from_str(&string).map_err(serde::de::Error::custom)?;
        Ok(Self { string, version })
    }
}
impl Serialize for Version {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.string.serialize(serializer)
    }
}

#[async_trait]
pub trait AsyncFileExt: Sized {
    async fn maybe_open<P: AsRef<Path> + Send + Sync>(path: P) -> std::io::Result<Option<Self>>;
    async fn delete<P: AsRef<Path> + Send + Sync>(path: P) -> std::io::Result<()>;
}
#[async_trait]
impl AsyncFileExt for File {
    async fn maybe_open<P: AsRef<Path> + Send + Sync>(path: P) -> std::io::Result<Option<Self>> {
        match File::open(path).await {
            Ok(f) => Ok(Some(f)),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
            Err(e) => Err(e),
        }
    }
    async fn delete<P: AsRef<Path> + Send + Sync>(path: P) -> std::io::Result<()> {
        if let Ok(m) = tokio::fs::metadata(path.as_ref()).await {
            if m.is_dir() {
                tokio::fs::remove_dir_all(path).await
            } else {
                tokio::fs::remove_file(path).await
            }
        } else {
            Ok(())
        }
    }
}

pub struct FmtWriter<W: std::fmt::Write>(W);
impl<W: std::fmt::Write> std::io::Write for FmtWriter<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0
            .write_str(
                std::str::from_utf8(buf)
                    .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?,
            )
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
        Ok(buf.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
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
                    &serde_toml::to_vec(
                        &serde_toml::Value::try_from(value)
                            .with_kind(crate::ErrorKind::Serialization)?,
                    )
                    .with_kind(crate::ErrorKind::Serialization)?,
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
            IoFormat::Yaml => serde_yaml::to_vec(value).with_kind(crate::ErrorKind::Serialization),
            IoFormat::Cbor => {
                let mut res = Vec::new();
                serde_cbor::ser::into_writer(value, &mut res)
                    .with_kind(crate::ErrorKind::Serialization)?;
                Ok(res)
            }
            IoFormat::Toml => serde_toml::to_vec(
                &serde_toml::Value::try_from(value).with_kind(crate::ErrorKind::Serialization)?,
            )
            .with_kind(crate::ErrorKind::Serialization),
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
                serde_toml::from_slice(slice).with_kind(crate::ErrorKind::Deserialization)
            }
        }
    }
}

pub fn display_serializable<T: Serialize>(t: T, matches: &ArgMatches<'_>) {
    let format = match matches.value_of("format").map(|f| f.parse()) {
        Some(Ok(f)) => f,
        Some(Err(_)) => {
            eprintln!("unrecognized formatter");
            exit(1)
        }
        None => IoFormat::default(),
    };
    format
        .to_writer(std::io::stdout(), &t)
        .expect("Error serializing result to stdout")
}

pub fn display_none<T>(_: T, _: &ArgMatches) {
    ()
}

pub fn parse_stdin_deserializable<T: for<'de> Deserialize<'de>>(
    stdin: &mut std::io::Stdin,
    matches: &ArgMatches<'_>,
) -> Result<T, Error> {
    let format = match matches.value_of("format").map(|f| f.parse()) {
        Some(Ok(f)) => f,
        Some(Err(_)) => {
            eprintln!("unrecognized formatter");
            exit(1)
        }
        None => IoFormat::default(),
    };
    format.from_reader(stdin)
}

pub fn parse_duration(arg: &str, _: &ArgMatches<'_>) -> Result<Duration, Error> {
    let units_idx = arg.find(|c: char| c.is_alphabetic()).ok_or_else(|| {
        Error::new(
            anyhow!("Must specify units for duration"),
            crate::ErrorKind::Deserialization,
        )
    })?;
    let (num, units) = arg.split_at(units_idx);
    match units {
        "d" if num.contains(".") => Ok(Duration::from_secs_f64(num.parse::<f64>()? * 86400_f64)),
        "d" => Ok(Duration::from_secs(num.parse::<u64>()? * 86400)),
        "h" if num.contains(".") => Ok(Duration::from_secs_f64(num.parse::<f64>()? * 3600_f64)),
        "h" => Ok(Duration::from_secs(num.parse::<u64>()? * 3600)),
        "m" if num.contains(".") => Ok(Duration::from_secs_f64(num.parse::<f64>()? * 60_f64)),
        "m" => Ok(Duration::from_secs(num.parse::<u64>()? * 60)),
        "s" if num.contains(".") => Ok(Duration::from_secs_f64(num.parse()?)),
        "s" => Ok(Duration::from_secs(num.parse()?)),
        "ms" => Ok(Duration::from_millis(num.parse()?)),
        "us" => Ok(Duration::from_micros(num.parse()?)),
        "ns" => Ok(Duration::from_nanos(num.parse()?)),
        _ => Err(Error::new(
            anyhow!("Invalid units for duration"),
            crate::ErrorKind::Deserialization,
        )),
    }
}

pub struct Container<T>(RwLock<Option<T>>);
impl<T> Container<T> {
    pub fn new(value: Option<T>) -> Self {
        Container(RwLock::new(value))
    }
    pub async fn set(&self, value: T) -> Option<T> {
        std::mem::replace(&mut *self.0.write().await, Some(value))
    }
    pub async fn take(&self) -> Option<T> {
        std::mem::replace(&mut *self.0.write().await, None)
    }
    pub async fn is_empty(&self) -> bool {
        self.0.read().await.is_none()
    }
    pub async fn drop(&self) {
        *self.0.write().await = None;
    }
}

pub struct HashWriter<H: Digest, W: std::io::Write> {
    hasher: H,
    writer: W,
}
impl<H: Digest, W: std::io::Write> HashWriter<H, W> {
    pub fn new(hasher: H, writer: W) -> Self {
        HashWriter { hasher, writer }
    }
    pub fn finish(self) -> (H, W) {
        (self.hasher, self.writer)
    }
}
impl<H: Digest, W: std::io::Write> std::io::Write for HashWriter<H, W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let written = self.writer.write(buf)?;
        self.hasher.update(&buf[..written]);
        Ok(written)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.writer.flush()
    }
}
impl<H: Digest, W: std::io::Write> std::ops::Deref for HashWriter<H, W> {
    type Target = W;
    fn deref(&self) -> &Self::Target {
        &self.writer
    }
}
impl<H: Digest, W: std::io::Write> std::ops::DerefMut for HashWriter<H, W> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.writer
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Port(pub u16);
impl<'de> Deserialize<'de> for Port {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        //TODO: if number, be permissive
        deserialize_from_str(deserializer).map(Port)
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

pub trait IntoDoubleEndedIterator<T>: IntoIterator<Item = T> {
    type IntoIter: Iterator<Item = T> + DoubleEndedIterator;
    fn into_iter(self) -> <Self as IntoDoubleEndedIterator<T>>::IntoIter;
}
impl<T, U> IntoDoubleEndedIterator<U> for T
where
    T: IntoIterator<Item = U>,
    <T as IntoIterator>::IntoIter: DoubleEndedIterator,
{
    type IntoIter = <T as IntoIterator>::IntoIter;
    fn into_iter(self) -> <Self as IntoDoubleEndedIterator<U>>::IntoIter {
        IntoIterator::into_iter(self)
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
        use serde::ser::SerializeSeq;

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

#[pin_project::pin_project(PinnedDrop)]
pub struct NonDetachingJoinHandle<T>(#[pin] JoinHandle<T>);
impl<T> From<JoinHandle<T>> for NonDetachingJoinHandle<T> {
    fn from(t: JoinHandle<T>) -> Self {
        NonDetachingJoinHandle(t)
    }
}
#[pin_project::pinned_drop]
impl<T> PinnedDrop for NonDetachingJoinHandle<T> {
    fn drop(self: std::pin::Pin<&mut Self>) {
        let this = self.project();
        this.0.into_ref().get_ref().abort()
    }
}
impl<T> Future for NonDetachingJoinHandle<T> {
    type Output = Result<T, JoinError>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let this = self.project();
        this.0.poll(cx)
    }
}

pub struct GeneralGuard<F: FnOnce() -> T, T = ()>(Option<F>);
impl<F: FnOnce() -> T, T> GeneralGuard<F, T> {
    pub fn new(f: F) -> Self {
        GeneralGuard(Some(f))
    }

    pub fn drop(mut self) -> T {
        self.0.take().unwrap()()
    }
}

impl<F: FnOnce() -> T, T> Drop for GeneralGuard<F, T> {
    fn drop(&mut self) {
        if let Some(destroy) = self.0.take() {
            destroy();
        }
    }
}
