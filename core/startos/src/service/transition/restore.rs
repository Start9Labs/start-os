use std::path::PathBuf;

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
        let path = restore.path.clone();
        let seed = self.0.clone();

        let state = self.0.persistent_container.state.clone();
        let transition = RemoteCancellable::new(
            async move {
                let backup_guard = seed
                    .persistent_container
                    .mount_backup(path, ReadOnly)
                    .await?;
                seed.persistent_container
                    .execute(id, ProcedureName::RestoreBackup, Value::Null, None)
                    .await?;
                backup_guard.unmount(true).await?;

                state.send_modify(|s| {
                    s.transition_state.take();
                });
                Ok::<_, Error>(())
            }
            .map(|x| {
                if let Err(err) = dbg!(x) {
                    tracing::debug!("{:?}", err);
                    tracing::warn!("{}", err);
                }
            }),
        );
        let cancel_handle = transition.cancellation_handle();
        let transition = transition.shared();
        let job_transition = transition.clone();
        jobs.add_job(job_transition.map(|_| ()));

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
        match transition.await {
            None => Err(Error::new(eyre!("Restoring canceled"), ErrorKind::Unknown)),
            Some(x) => Ok(x),
        }
    }
}
