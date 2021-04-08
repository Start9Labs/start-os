use std::fmt::Display;

use anyhow::anyhow;
use patch_db::Revision;
use rpc_toolkit::yajrc::RpcError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    Unknown = 1,
    Filesystem = 2,
    Docker = 3,
    ConfigSpecViolation = 4,
    ConfigRulesViolation = 5,
    NotFound = 6,
    InvalidPassword = 7,
    VersionIncompatible = 8,
    Network = 9,
    Registry = 10,
    Serialization = 11,
    Deserialization = 12,
    Utf8 = 13,
    ParseVersion = 14,
    Duplicity = 15,
    Nginx = 16,
    Dependency = 17,
    ParseS9pk = 18,
    ParseUrl = 19,
    GParted = 20,
    Blkid = 21,
    InvalidOnionAddress = 22,
    Pack = 23,
    ValidateS9pk = 24,
    OpenSSL = 25,
    Tor = 26,
    ConfigGen = 27,
    ParseNumber = 28,
    Database = 29,
    InvalidPackageId = 30,
    InvalidSignature = 31,
    Backup = 32,
    Restore = 33,
    Authorization = 34,
    AutoConfigure = 35,
    Action = 36,
    RateLimited = 37,
    InvalidRequest = 38,
    MigrationFailed = 39,
}
impl ErrorKind {
    pub fn as_str(&self) -> &'static str {
        use ErrorKind::*;
        match self {
            Unknown => "Unknown Error",
            Filesystem => "Filesystem I/O Error",
            Docker => "Docker Error",
            ConfigSpecViolation => "Config Spec Violation",
            ConfigRulesViolation => "Config Rules Violation",
            NotFound => "Not Found",
            InvalidPassword => "Invalid Password",
            VersionIncompatible => "Version Incompatible",
            Network => "Network Error",
            Registry => "Registry Error",
            Serialization => "Serialization Error",
            Deserialization => "Deserialization Error",
            Utf8 => "UTF-8 Parse Error",
            ParseVersion => "Version Parsing Error",
            Duplicity => "Duplicity Error",
            Nginx => "Nginx Error",
            Dependency => "Dependency Error",
            ParseS9pk => "S9PK Parsing Error",
            ParseUrl => "URL Parsing Error",
            GParted => "GNU Parted Error",
            Blkid => "BLKID Error",
            InvalidOnionAddress => "Invalid Onion Address",
            Pack => "Pack Error",
            ValidateS9pk => "S9PK Validation Error",
            OpenSSL => "OpenSSL Error",
            Tor => "Tor Daemon Error",
            ConfigGen => "Config Generation Error",
            ParseNumber => "Number Parsing Error",
            Database => "Database Error",
            InvalidPackageId => "Invalid Package ID",
            InvalidSignature => "Invalid Signature",
            Backup => "Backup Error",
            Restore => "Restore Error",
            Authorization => "Unauthorized",
            AutoConfigure => "Auto-Configure Error",
            Action => "Action Failed",
            RateLimited => "Rate Limited",
            InvalidRequest => "Invalid Request",
            MigrationFailed => "Migration Failed",
        }
    }
}
impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Debug)]
pub struct Error {
    pub source: anyhow::Error,
    pub kind: ErrorKind,
    pub revision: Option<Revision>,
}
impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.kind.as_str(), self.source)
    }
}
impl Error {
    pub fn new<E: Into<anyhow::Error>>(source: E, kind: ErrorKind) -> Self {
        Error {
            source: source.into(),
            kind,
            revision: None,
        }
    }
}
impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::new(e, ErrorKind::Filesystem)
    }
}
impl From<std::str::Utf8Error> for Error {
    fn from(e: std::str::Utf8Error) -> Self {
        Error::new(e, ErrorKind::Utf8)
    }
}
impl From<std::string::FromUtf8Error> for Error {
    fn from(e: std::string::FromUtf8Error) -> Self {
        Error::new(e, ErrorKind::Utf8)
    }
}
impl From<emver::ParseError> for Error {
    fn from(e: emver::ParseError) -> Self {
        Error::new(e, ErrorKind::ParseVersion)
    }
}
impl From<rpc_toolkit::url::ParseError> for Error {
    fn from(e: rpc_toolkit::url::ParseError) -> Self {
        Error::new(e, ErrorKind::ParseUrl)
    }
}
impl From<std::num::ParseIntError> for Error {
    fn from(e: std::num::ParseIntError) -> Self {
        Error::new(e, ErrorKind::ParseNumber)
    }
}
impl From<std::num::ParseFloatError> for Error {
    fn from(e: std::num::ParseFloatError) -> Self {
        Error::new(e, ErrorKind::ParseNumber)
    }
}
impl From<patch_db::Error> for Error {
    fn from(e: patch_db::Error) -> Self {
        Error::new(e, ErrorKind::Database)
    }
}
impl From<sqlx::Error> for Error {
    fn from(e: sqlx::Error) -> Self {
        Error::new(e, ErrorKind::Database)
    }
}
impl From<ed25519_dalek::SignatureError> for Error {
    fn from(e: ed25519_dalek::SignatureError) -> Self {
        Error::new(e, ErrorKind::InvalidSignature)
    }
}
impl From<bollard::errors::Error> for Error {
    fn from(e: bollard::errors::Error) -> Self {
        Error::new(e, ErrorKind::Docker)
    }
}
impl From<Error> for RpcError {
    fn from(e: Error) -> Self {
        let mut data_object = serde_json::Map::with_capacity(2);
        data_object.insert("message".to_owned(), format!("{}", e).into());
        data_object.insert(
            "revision".to_owned(),
            match serde_json::to_value(&e.revision) {
                Ok(a) => a,
                Err(e) => {
                    log::warn!("Error serializing revision for Error object: {}", e);
                    serde_json::Value::Null
                }
            },
        );
        RpcError {
            code: e.kind as i32,
            message: e.kind.as_str().into(),
            data: Some(data_object.into()),
        }
    }
}

pub trait ResultExt<T, E>
where
    Self: Sized,
{
    fn with_kind(self, kind: ErrorKind) -> Result<T, Error>;
    fn with_ctx<F: FnOnce(&E) -> (ErrorKind, D), D: Display + Send + Sync + 'static>(
        self,
        f: F,
    ) -> Result<T, Error>;
}
impl<T, E> ResultExt<T, E> for Result<T, E>
where
    anyhow::Error: From<E>,
{
    fn with_kind(self, kind: ErrorKind) -> Result<T, Error> {
        self.map_err(|e| Error {
            source: e.into(),
            kind,
            revision: None,
        })
    }

    fn with_ctx<F: FnOnce(&E) -> (ErrorKind, D), D: Display + Send + Sync + 'static>(
        self,
        f: F,
    ) -> Result<T, Error> {
        self.map_err(|e| {
            let (kind, ctx) = f(&e);
            let source = anyhow::Error::from(e);
            let ctx = format!("{}: {}", ctx, source);
            let source = source.context(ctx);
            Error {
                kind,
                source: source.into(),
                revision: None,
            }
        })
    }
}

#[macro_export]
macro_rules! ensure_code {
    ($x:expr, $c:expr, $fmt:expr $(, $arg:expr)*) => {
        if !($x) {
            return Err(crate::Error::new(anyhow!($fmt, $($arg, )*), $c));
        }
    };
}
