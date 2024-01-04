use std::fmt::{Debug, Display};

use color_eyre::eyre::eyre;
use num_enum::TryFromPrimitive;
use patch_db::Revision;
use rpc_toolkit::hyper::http::uri::InvalidUri;
use rpc_toolkit::reqwest;
use rpc_toolkit::yajrc::{
    RpcError, INVALID_PARAMS_ERROR, INVALID_REQUEST_ERROR, METHOD_NOT_FOUND_ERROR, PARSE_ERROR,
};
use serde::{Deserialize, Serialize};

use crate::InvalidId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(i32)]
pub enum ErrorKind {
    Unknown = 1,
    Filesystem = 2,
    Docker = 3,
    ConfigSpecViolation = 4,
    ConfigRulesViolation = 5,
    NotFound = 6,
    IncorrectPassword = 7,
    VersionIncompatible = 8,
    Network = 9,
    Registry = 10,
    Serialization = 11,
    Deserialization = 12,
    Utf8 = 13,
    ParseVersion = 14,
    IncorrectDisk = 15,
    // Nginx = 16,
    Dependency = 17,
    ParseS9pk = 18,
    ParseUrl = 19,
    DiskNotAvailable = 20,
    BlockDevice = 21,
    InvalidOnionAddress = 22,
    Pack = 23,
    ValidateS9pk = 24,
    DiskCorrupted = 25, // Remove
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
    Uninitialized = 40,
    ParseNetAddress = 41,
    ParseSshKey = 42,
    SoundError = 43,
    ParseTimestamp = 44,
    ParseSysInfo = 45,
    Wifi = 46,
    Journald = 47,
    DiskManagement = 48,
    OpenSsl = 49,
    PasswordHashGeneration = 50,
    DiagnosticMode = 51,
    ParseDbField = 52,
    Duplicate = 53,
    MultipleErrors = 54,
    Incoherent = 55,
    InvalidBackupTargetId = 56,
    ProductKeyMismatch = 57,
    LanPortConflict = 58,
    Javascript = 59,
    Pem = 60,
    TLSInit = 61,
    Ascii = 62,
    MissingHeader = 63,
    Grub = 64,
    Systemd = 65,
    OpenSsh = 66,
    Zram = 67,
    Lshw = 68,
    CpuSettings = 69,
    Firmware = 70,
    Timeout = 71,
    Lxc = 72,
    Cancelled = 73,
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
            IncorrectPassword => "Incorrect Password",
            VersionIncompatible => "Version Incompatible",
            Network => "Network Error",
            Registry => "Registry Error",
            Serialization => "Serialization Error",
            Deserialization => "Deserialization Error",
            Utf8 => "UTF-8 Parse Error",
            ParseVersion => "Version Parsing Error",
            IncorrectDisk => "Incorrect Disk",
            // Nginx => "Nginx Error",
            Dependency => "Dependency Error",
            ParseS9pk => "S9PK Parsing Error",
            ParseUrl => "URL Parsing Error",
            DiskNotAvailable => "Disk Not Available",
            BlockDevice => "Block Device Error",
            InvalidOnionAddress => "Invalid Onion Address",
            Pack => "Pack Error",
            ValidateS9pk => "S9PK Validation Error",
            DiskCorrupted => "Disk Corrupted", // Remove
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
            Uninitialized => "Uninitialized",
            ParseNetAddress => "Net Address Parsing Error",
            ParseSshKey => "SSH Key Parsing Error",
            SoundError => "Sound Interface Error",
            ParseTimestamp => "Timestamp Parsing Error",
            ParseSysInfo => "System Info Parsing Error",
            Wifi => "WiFi Internal Error",
            Journald => "Journald Error",
            DiskManagement => "Disk Management Error",
            OpenSsl => "OpenSSL Internal Error",
            PasswordHashGeneration => "Password Hash Generation Error",
            DiagnosticMode => "Server is in Diagnostic Mode",
            ParseDbField => "Database Field Parse Error",
            Duplicate => "Duplication Error",
            MultipleErrors => "Multiple Errors",
            Incoherent => "Incoherent",
            InvalidBackupTargetId => "Invalid Backup Target ID",
            ProductKeyMismatch => "Incompatible Product Keys",
            LanPortConflict => "Incompatible LAN Port Configuration",
            Javascript => "Javascript Engine Error",
            Pem => "PEM Encoding Error",
            TLSInit => "TLS Backend Initialization Error",
            Ascii => "ASCII Parse Error",
            MissingHeader => "Missing Header",
            Grub => "Grub Error",
            Systemd => "Systemd Error",
            OpenSsh => "OpenSSH Error",
            Zram => "Zram Error",
            Lshw => "LSHW Error",
            CpuSettings => "CPU Settings Error",
            Firmware => "Firmware Error",
            Timeout => "Timeout Error",
            Lxc => "LXC Error",
            Cancelled => "Cancelled",
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
    pub source: color_eyre::eyre::Error,
    pub kind: ErrorKind,
    pub revision: Option<Revision>,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.kind.as_str(), self.source)
    }
}
impl Error {
    pub fn new<E: Into<color_eyre::eyre::Error>>(source: E, kind: ErrorKind) -> Self {
        Error {
            source: source.into(),
            kind,
            revision: None,
        }
    }
}
impl From<InvalidId> for Error {
    fn from(err: InvalidId) -> Self {
        Error::new(err, ErrorKind::InvalidPackageId)
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
impl From<std::net::AddrParseError> for Error {
    fn from(e: std::net::AddrParseError) -> Self {
        Error::new(e, ErrorKind::ParseNetAddress)
    }
}
impl From<torut::control::ConnError> for Error {
    fn from(e: torut::control::ConnError) -> Self {
        Error::new(e, ErrorKind::Tor)
    }
}
impl From<ipnet::AddrParseError> for Error {
    fn from(e: ipnet::AddrParseError) -> Self {
        Error::new(e, ErrorKind::ParseNetAddress)
    }
}
impl From<openssl::error::ErrorStack> for Error {
    fn from(e: openssl::error::ErrorStack) -> Self {
        Error::new(eyre!("{}", e), ErrorKind::OpenSsl)
    }
}
impl From<mbrman::Error> for Error {
    fn from(e: mbrman::Error) -> Self {
        Error::new(e, ErrorKind::DiskManagement)
    }
}
impl From<InvalidUri> for Error {
    fn from(e: InvalidUri) -> Self {
        Error::new(eyre!("{}", e), ErrorKind::ParseUrl)
    }
}
impl From<ssh_key::Error> for Error {
    fn from(e: ssh_key::Error) -> Self {
        Error::new(e, ErrorKind::OpenSsh)
    }
}
impl From<reqwest::Error> for Error {
    fn from(e: reqwest::Error) -> Self {
        let kind = match e {
            _ if e.is_builder() => ErrorKind::ParseUrl,
            _ if e.is_decode() => ErrorKind::Deserialization,
            _ => ErrorKind::Network,
        };
        Error::new(e, kind)
    }
}
impl From<patch_db::value::Error> for Error {
    fn from(value: patch_db::value::Error) -> Self {
        match value.kind {
            patch_db::value::ErrorKind::Serialization => {
                Error::new(value.source, ErrorKind::Serialization)
            }
            patch_db::value::ErrorKind::Deserialization => {
                Error::new(value.source, ErrorKind::Deserialization)
            }
        }
    }
}

#[derive(Clone, Deserialize, Serialize)]
pub struct ErrorData {
    pub details: String,
    pub debug: String,
}
impl Display for ErrorData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.details, f)
    }
}
impl Debug for ErrorData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.debug, f)
    }
}
impl std::error::Error for ErrorData {}
impl From<&RpcError> for ErrorData {
    fn from(value: &RpcError) -> Self {
        Self {
            details: value
                .data
                .as_ref()
                .and_then(|d| {
                    d.as_object()
                        .and_then(|d| {
                            d.get("details")
                                .and_then(|d| d.as_str().map(|s| s.to_owned()))
                        })
                        .or_else(|| d.as_str().map(|s| s.to_owned()))
                })
                .unwrap_or_else(|| value.message.clone().into_owned()),
            debug: value
                .data
                .as_ref()
                .and_then(|d| {
                    d.as_object()
                        .and_then(|d| {
                            d.get("debug")
                                .and_then(|d| d.as_str().map(|s| s.to_owned()))
                        })
                        .or_else(|| d.as_str().map(|s| s.to_owned()))
                })
                .unwrap_or_else(|| value.message.clone().into_owned()),
        }
    }
}

impl From<Error> for RpcError {
    fn from(e: Error) -> Self {
        let mut data_object = serde_json::Map::with_capacity(3);
        data_object.insert("details".to_owned(), format!("{}", e.source).into());
        data_object.insert("debug".to_owned(), format!("{:?}", e.source).into());
        data_object.insert(
            "revision".to_owned(),
            match serde_json::to_value(&e.revision) {
                Ok(a) => a,
                Err(e) => {
                    tracing::warn!("Error serializing revision for Error object: {}", e);
                    serde_json::Value::Null
                }
            },
        );
        RpcError {
            code: e.kind as i32,
            message: e.kind.as_str().into(),
            data: Some(
                match serde_json::to_value(&ErrorData {
                    details: format!("{}", e.source),
                    debug: format!("{:?}", e.source),
                }) {
                    Ok(a) => a,
                    Err(e) => {
                        tracing::warn!("Error serializing revision for Error object: {}", e);
                        serde_json::Value::Null
                    }
                },
            ),
        }
    }
}
impl From<RpcError> for Error {
    fn from(e: RpcError) -> Self {
        Error::new(
            ErrorData::from(&e),
            if let Ok(kind) = e.code.try_into() {
                kind
            } else if e.code == METHOD_NOT_FOUND_ERROR.code {
                ErrorKind::NotFound
            } else if e.code == PARSE_ERROR.code
                || e.code == INVALID_PARAMS_ERROR.code
                || e.code == INVALID_REQUEST_ERROR.code
            {
                ErrorKind::Deserialization
            } else {
                ErrorKind::Unknown
            },
        )
    }
}

#[derive(Debug, Default)]
pub struct ErrorCollection(Vec<Error>);
impl ErrorCollection {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn handle<T, E: Into<Error>>(&mut self, result: Result<T, E>) -> Option<T> {
        match result {
            Ok(a) => Some(a),
            Err(e) => {
                self.0.push(e.into());
                None
            }
        }
    }

    pub fn into_result(self) -> Result<(), Error> {
        if self.0.is_empty() {
            Ok(())
        } else {
            Err(Error::new(eyre!("{}", self), ErrorKind::MultipleErrors))
        }
    }
}
impl From<ErrorCollection> for Result<(), Error> {
    fn from(e: ErrorCollection) -> Self {
        e.into_result()
    }
}
impl<T, E: Into<Error>> Extend<Result<T, E>> for ErrorCollection {
    fn extend<I: IntoIterator<Item = Result<T, E>>>(&mut self, iter: I) {
        for item in iter {
            self.handle(item);
        }
    }
}
impl std::fmt::Display for ErrorCollection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (idx, e) in self.0.iter().enumerate() {
            if idx > 0 {
                write!(f, "; ")?;
            }
            write!(f, "{}", e)?;
        }
        Ok(())
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
    color_eyre::eyre::Error: From<E>,
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
            let source = color_eyre::eyre::Error::from(e);
            let ctx = format!("{}: {}", ctx, source);
            let source = source.wrap_err(ctx);
            Error {
                kind,
                source,
                revision: None,
            }
        })
    }
}

pub trait OptionExt<T>
where
    Self: Sized,
{
    fn or_not_found(self, message: impl std::fmt::Display) -> Result<T, Error>;
}
impl<T> OptionExt<T> for Option<T> {
    fn or_not_found(self, message: impl std::fmt::Display) -> Result<T, Error> {
        self.ok_or_else(|| Error::new(eyre!("{}", message), ErrorKind::NotFound))
    }
}

#[macro_export]
macro_rules! ensure_code {
    ($x:expr, $c:expr, $fmt:expr $(, $arg:expr)*) => {
        if !($x) {
            return Err(Error::new(color_eyre::eyre::eyre!($fmt, $($arg, )*), $c));
        }
    };
}
