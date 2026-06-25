use futures::FutureExt;
use futures::future::BoxFuture;

use crate::prelude::*;
use crate::service::ServiceActorSeed;

pub mod backup;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TransitionKind {
    BackingUp,
    Starting,
    Stopping,
}

pub struct Transition<'a> {
    pub kind: TransitionKind,
    pub future: BoxFuture<'a, Result<(), Error>>,
}
impl<'a> ::std::fmt::Debug for Transition<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Transition")
            .field("kind", &self.kind)
            .finish_non_exhaustive()
    }
}

impl ServiceActorSeed {
    pub fn start(&self) -> Transition<'_> {
        Transition {
            kind: TransitionKind::Starting,
            future: async {
                self.persistent_container.start().await?;
                let id = &self.id;
                self.ctx
                    .db
                    .mutate(|db| {
                        db.as_public_mut()
                            .as_package_data_mut()
                            .as_idx_mut(id)
                            .or_not_found(id)?
                            .as_status_info_mut()
                            .started()
                    })
                    .await
                    .result?;

                Ok(())
            }
            .boxed(),
        }
    }

    pub fn stop(&self) -> Transition<'_> {
        Transition {
            kind: TransitionKind::Stopping,
            future: async {
                self.persistent_container.stop().await?;
                let id = &self.id;
                self.ctx
                    .db
                    .mutate(|db| {
                        db.as_public_mut()
                            .as_package_data_mut()
                            .as_idx_mut(id)
                            .or_not_found(id)?
                            .as_status_info_mut()
                            .stopped()
                    })
                    .await
                    .result?;

                Ok(())
            }
            .boxed(),
        }
    }
}
