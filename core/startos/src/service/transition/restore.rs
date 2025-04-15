use std::path::PathBuf;

use futures::channel::oneshot;
use futures::FutureExt;
use models::ProcedureName;

use crate::disk::mount::filesystem::ReadOnly;
use crate::prelude::*;
use crate::rpc_continuations::Guid;
use crate::service::transition::{TransitionKind, TransitionState};
use crate::service::ServiceActor;
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::actor::{ConflictBuilder, Handler};
use crate::util::future::RemoteCancellable;
use crate::util::serde::NoOutput;

pub(in crate::service) struct Restore {
    pub path: PathBuf,
}
impl Handler<Restore> for ServiceActor {
    type Response = Result<(), Error>;
    fn conflicts_with(_: &Restore) -> ConflictBuilder<Self> {
        ConflictBuilder::everything()
    }
    async fn handle(
        &mut self,
        id: Guid,
        restore: Restore,
        jobs: &BackgroundJobQueue,
    ) -> Self::Response {
        // So Need a handle to just a single field in the state
        let path = restore.path;
        let seed = self.0.clone();

        let state = self.0.persistent_container.state.clone();
        let (send_res, recv_res) = oneshot::channel();
        let transition = RemoteCancellable::new(
            async move {
                let backup_guard = seed
                    .persistent_container
                    .mount_backup(path, ReadOnly)
                    .await?;
                seed.persistent_container
                    .execute::<NoOutput>(id, ProcedureName::RestoreBackup, Value::Null, None)
                    .await?;
                backup_guard.unmount(true).await?;

                state.send_modify(|s| {
                    s.transition_state.take();
                });
                Ok::<_, Error>(())
            }
            .map(|res| send_res.send(res)),
        );
        let cancel_handle = transition.cancellation_handle();
        jobs.add_job(transition.map(|_| ()));

        let mut old = None;
        self.0.persistent_container.state.send_modify(|s| {
            old = std::mem::replace(
                &mut s.transition_state,
                Some(TransitionState {
                    kind: TransitionKind::Restoring,
                    cancel_handle,
                }),
            )
        });
        if let Some(t) = old {
            t.abort().await;
        }
        match recv_res.await {
            Err(_) => Err(Error::new(eyre!("Restoring canceled"), ErrorKind::Unknown)),
            Ok(res) => res,
        }
    }
}
