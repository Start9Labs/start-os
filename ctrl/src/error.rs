use std::backtrace::Backtrace;
use std::fmt;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ErrorKind {
    #[error(transparent)]
    Other(#[from] color_eyre::eyre::Error),
    #[error("interface name too long")]
    InterfaceNameTooLong,
    #[error("interface name conflicts")]
    InterfaceNameConflict,
    #[error("interface name has invalid characters")]
    InterfaceNameInvalid,
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
