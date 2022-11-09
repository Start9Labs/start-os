use color_eyre::eyre::eyre;
pub use models::{Error, ErrorKind, ResultExt};

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

#[macro_export]
macro_rules! ensure_code {
    ($x:expr, $c:expr, $fmt:expr $(, $arg:expr)*) => {
        if !($x) {
            return Err(crate::Error::new(color_eyre::eyre::eyre!($fmt, $($arg, )*), $c));
        }
    };
}
