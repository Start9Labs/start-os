use crate::profiles::ProfileIdOpt;
use color_eyre::eyre;
use rpc_toolkit::yajrc::RpcError;
use serde::{Serialize, Serializer};
use std::backtrace::Backtrace;
use std::fmt;
use thiserror::Error;

fn serialize_eyre_err<S: Serializer>(err: &eyre::Error, ser: S) -> Result<S::Ok, S::Error> {
    err.to_string().serialize(ser)
}

fn serialize_std_err<S: Serializer>(
    err: &impl std::error::Error,
    ser: S,
) -> Result<S::Ok, S::Error> {
    err.to_string().serialize(ser)
}

#[derive(Debug, Error, Serialize)]
#[serde(tag = "error")]
pub enum ErrorKind {
    #[error(transparent)]
    Io(
        #[serde(serialize_with = "serialize_std_err")]
        #[from]
        std::io::Error,
    ),
    #[error(transparent)]
    UciEdit(#[from] uciedit::Error),
    #[error("interface name {name:?} conflicts")]
    InterfaceNameConflict { name: String },
    #[error("an existing profile already owns lan")]
    LanOwnerExists,
    #[error("could not find profile identified by {id:?}")]
    MissingProfile { id: ProfileIdOpt },
    #[error("corrupted profile with {id:?}")]
    CorruptedProfile { id: ProfileIdOpt },
    #[error("corrupted wifi devices and interfaces")]
    CorruptedWifi,
    #[error("all wireless interfaces need to be named")]
    UnnamedWirelessInterface,
    #[error("all wireless devices need to be named")]
    UnnamedWirelessDevice,
    #[error("multiple vlans with tag {tag}")]
    DuplicateVlanTag { tag: u16 },
    #[error("cannot delete the LAN owner profile")]
    CannotDeleteLanOwner,
    #[error("no lan bridge device found")]
    MissingLanBridge,
    #[error("no wan interface found")]
    MissingWanInterface,
    #[error("no lan interface found")]
    MissingLanInterface,
    #[error("no firewall zone for interface {interface}")]
    MissingFirewallZone { interface: String },
    #[error("the wan port {port} can not have a profile")]
    WanPortWithProfile { port: String },
    #[error("duplicate wifi password")]
    DuplicatePassword,
    #[error("duplicate wifi password label")]
    DuplicatePasswordLabel,
    #[error("VPN {label:?} cannot be removed or disabled because it is used as a target by: {dependents:?}")]
    VpnHasDependents {
        label: String,
        dependents: Vec<String>,
    },
    #[error("VPN chain cycle detected: {label:?} would create a loop through {cycle:?}")]
    VpnChainCycle { label: String, cycle: Vec<String> },
    #[error("a profile named {name:?} already exists")]
    DuplicateFullname { name: String },
    #[error("changing the IP would break {peer_count} VPN client(s) across profiles: {profiles:?}")]
    VpnPeersWouldBreak {
        profiles: Vec<String>,
        peer_count: usize,
    },
    #[error("device {mac} has no {family} address for port forward \"{label}\"")]
    MissingDeviceAddress {
        mac: String,
        family: String,
        label: String,
    },
    #[error("invalid value for {field}: {value:?}")]
    InvalidValue { field: String, value: String },
    #[error(transparent)]
    Other(
        #[serde(serialize_with = "serialize_eyre_err")]
        #[from]
        eyre::Error,
    ),
}

impl<E> From<E> for Error
where
    ErrorKind: From<E>,
{
    fn from(value: E) -> Self {
        Error {
            kind: value.into(),
            backtrace: Backtrace::capture(),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub backtrace: Backtrace,
}

impl Error {
    pub fn other(msg: impl fmt::Display + fmt::Debug + Sync + Send + 'static) -> Self {
        Error {
            kind: ErrorKind::Other(eyre::Error::msg(msg)),
            backtrace: Backtrace::capture(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}\n{}", self.kind, self.backtrace)
    }
}

impl std::error::Error for Error {}

impl From<RpcError> for Error {
    fn from(e: RpcError) -> Self {
        Error {
            kind: ErrorKind::Other(eyre::eyre!("{}", e)),
            backtrace: Backtrace::capture(),
        }
    }
}
