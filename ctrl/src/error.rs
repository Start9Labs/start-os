use crate::profiles::ProfileIdAndName;
use std::backtrace::Backtrace;
use std::fmt;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ErrorKind {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error(transparent)]
    UciEdit(#[from] uciedit::Error),
    #[error("interface name {0:?} conflicts")]
    InterfaceNameConflict(String),
    #[error("could not find profile identified by {0:?}")]
    MissingProfile(ProfileIdAndName),
    #[error("corrupted profile with {0:?}")]
    CorruptedProfile(ProfileIdAndName),
    #[error("corrupted wifi devices and interfaces")]
    CorruptedWifi,
    #[error("all wireless interfaces need to be named")]
    UnnamedWirelessInterface,
    #[error("all wireless devices need to be named")]
    UnnamedWirelessDevice,
    #[error("multiple vlans with tag {0}")]
    DuplicateVlanTag(u16),
    #[error("no lan bridge device found")]
    MissingLanBridge,
    #[error("no lan wan interface found")]
    MissingWanInterface,
    #[error("no firewall zone for interface {0}")]
    MissingFirewallZone(String),
    #[error("the wan port can not have a profile")]
    WanPortWithProfile(String),
    #[error(transparent)]
    Other(#[from] color_eyre::eyre::Error),
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
            kind: ErrorKind::Other(color_eyre::eyre::Error::msg(msg)),
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
