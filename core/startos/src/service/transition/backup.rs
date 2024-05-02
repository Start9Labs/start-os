use std::path::PathBuf;

use futures::{FutureExt, TryFutureExt};
use models::ProcedureName;

use super::TempDesiredRestore;
use crate::prelude::*;
use crate::service::config::GetConfig;
use crate::service::dependencies::DependencyConfig;
use crate::service::transition::{TransitionKind, TransitionState};
use crate::service::ServiceActor;
use crate::util::actor::background::BackgroundJobQueue;
use crate::util::actor::{ConflictBuilder, Handler};
use crate::util::future::RemoteCancellable;

pub(in crate::service) struct Backup {
    pub path: PathBuf,
}
impl Handler<Backup> for ServiceActor {
    type Response = Result<(), Error>;
    fn conflicts_with(_: &Backup) -> ConflictBuilder<Self> {
        ConflictBuilder::everything()
            .except::<GetConfig>()
            .except::<DependencyConfig>()
    }
    async fn handle(&mut self, backup: Backup, jobs: &BackgroundJobQueue) -> Self::Response {
        // So Need a handle to just a single field in the state
        let temp: TempDesiredRestore = TempDesiredRestore::new(&self.0.persistent_container.state);
        let mut current = self.0.persistent_container.state.subscribe();
        let path = backup.path.clone();
        let seed = self.0.clone();

        let state = self.0.persistent_container.state.clone();
        let transition = RemoteCancellable::new(
            async move {
                temp.stop();
                current
                    .wait_for(|s| s.running_status.is_none())
                    .await
                    .with_kind(ErrorKind::Unknown)?;

                let backup_guard = seed.persistent_container.mount_backup(path).await?;
                seed.persistent_container
                    .execute(ProcedureName::CreateBackup, Value::Null, None)
                    .await?;
                backup_guard.unmount(true).await?;

                if temp.restore().is_start() {
                    current
                        .wait_for(|s| s.running_status.is_some())
                        .await
                        .with_kind(ErrorKind::Unknown)?;
                }
                drop(temp);
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
                    kind: TransitionKind::BackingUp,
                    cancel_handle,
                }),
            )
        });
        if let Some(t) = old {
            t.abort().await;
        }
        match transition.await {
            None => Err(Error::new(eyre!("Backup canceled"), ErrorKind::Unknown)),
            Some(x) => Ok(x),
        }
    }
}
