use futures::Future;
use tokio::sync::Notify;

use crate::prelude::*;

pub async fn cancellable<T>(
    cancel_transition: &Notify,
    transition: impl Future<Output = T>,
) -> Result<T, Error> {
    tokio::select! {
        a = transition => Ok(a),
        _ = cancel_transition.notified() => Err(Error::new(eyre!("transition was cancelled"), ErrorKind::Cancelled)),
    }
}
