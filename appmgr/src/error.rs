use std::fmt::Display;

pub const GENERAL_ERROR: i32 = 1;
pub const FILESYSTEM_ERROR: i32 = 2;
pub const DOCKER_ERROR: i32 = 3;
pub const CFG_SPEC_VIOLATION: i32 = 4;
pub const CFG_RULES_VIOLATION: i32 = 5;
pub const NOT_FOUND: i32 = 6;
pub const INVALID_BACKUP_PASSWORD: i32 = 7;
pub const VERSION_INCOMPATIBLE: i32 = 8;
pub const NETWORK_ERROR: i32 = 9;
pub const REGISTRY_ERROR: i32 = 10;
pub const SERDE_ERROR: i32 = 11;

#[derive(Debug, Fail)]
#[fail(display = "{}", _0)]
pub struct Error {
    pub failure: failure::Error,
    pub code: Option<i32>,
}
impl Error {
    pub fn new<E: Into<failure::Error>>(e: E, code: Option<i32>) -> Self {
        Error {
            failure: e.into(),
            code,
        }
    }
    pub fn from<E: Into<failure::Error>>(e: E) -> Self {
        Error {
            failure: e.into(),
            code: None,
        }
    }
}
impl From<failure::Error> for Error {
    fn from(e: failure::Error) -> Self {
        Error {
            failure: e,
            code: None,
        }
    }
}
impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error {
            failure: e.into(),
            code: Some(2),
        }
    }
}
pub trait ResultExt<T, E>
where
    Self: Sized,
{
    fn with_code(self, code: i32) -> Result<T, Error>;
    fn with_ctx<F: FnOnce(&E) -> (Option<i32>, D), D: Display + Send + Sync + 'static>(
        self,
        f: F,
    ) -> Result<T, Error>;
    fn no_code(self) -> Result<T, Error>;
}
impl<T, E> ResultExt<T, E> for Result<T, E>
where
    failure::Error: From<E>,
{
    fn with_code(self, code: i32) -> Result<T, Error> {
        #[cfg(not(feature = "production"))]
        assert!(code != 0);
        self.map_err(|e| Error {
            failure: e.into(),
            code: Some(code),
        })
    }

    fn with_ctx<F: FnOnce(&E) -> (Option<i32>, D), D: Display + Send + Sync + 'static>(
        self,
        f: F,
    ) -> Result<T, Error> {
        self.map_err(|e| {
            let (code, ctx) = f(&e);
            let failure = failure::Error::from(e).context(ctx);
            Error {
                code,
                failure: failure.into(),
            }
        })
    }

    fn no_code(self) -> Result<T, Error> {
        self.map_err(|e| Error {
            failure: e.into(),
            code: None,
        })
    }
}

#[macro_export]
macro_rules! ensure_code {
    ($x:expr, $c:expr, $fmt:expr $(, $arg:expr)*) => {
        if !($x) {
            return Err(crate::Error {
                failure: format_err!($fmt, $($arg, )*),
                code: Some($c),
            });
        }
    };
}
