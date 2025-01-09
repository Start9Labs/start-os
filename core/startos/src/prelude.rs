pub use color_eyre::eyre::eyre;
pub use lazy_format::lazy_format;
pub use models::OptionExt;
pub use tracing::instrument;

pub use crate::db::prelude::*;
pub use crate::ensure_code;
pub use crate::error::{Error, ErrorCollection, ErrorKind, ResultExt};

#[macro_export]
macro_rules! dbg {
    () => {{
        tracing::debug!("[{}:{}:{}]", file!(), line!(), column!());
    }};
    ($e:expr) => {{
        let e = $e;
        tracing::debug!("[{}:{}:{}] {} = {e:?}", file!(), line!(), column!(), stringify!($e));
        e
    }};
    ($($e:expr),+) => {
        ($(
            crate::dbg!($e)
        ),+)
    }
}
