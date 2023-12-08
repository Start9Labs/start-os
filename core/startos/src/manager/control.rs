use crate::manager::start_stop::StartStop;
use crate::manager::{Service, ServiceActor};
use crate::prelude::*;
use crate::status::MainStatus;
use crate::util::actor::{BackgroundJobs, Handler};

impl ServiceActor {
    async fn export_state(&self) -> Result<(), Error> {
        let id = &self.package.as_manifest().id;
        let current = *self.persistent_container.current_state.borrow();
        let desired = self.desired_state;
        self.ctx
            .db
            .mutate(|db| {
                let main_status = db
                    .as_package_data_mut()
                    .as_idx_mut(id)
                    .and_then(|pde| pde.as_installed_mut())
                    .or_not_found(lazy_format!("{id} installed in database"))?
                    .as_status_mut()
                    .as_main_mut();
                let health = match main_status.de()? {
                    MainStatus::Running { health, .. } => Some(health),
                    _ => None,
                };
                let new_status = match (current, desired) {
                    (StartStop::Start, StartStop::Start) => MainStatus::Running {
                        started: chrono::Utc::now(),
                        health: health.unwrap_or_default(),
                    },
                    (StartStop::Start, StartStop::Stop) => MainStatus::Stopping,
                    (StartStop::Stop, StartStop::Start) => MainStatus::Starting,
                    (StartStop::Stop, StartStop::Stop) => MainStatus::Stopped,
                };
                main_status.ser(&new_status)
            })
            .await
    }
}

struct Start;
#[async_trait::async_trait]
impl Handler<Start> for ServiceActor {
    type Response = Result<(), Error>;
    async fn handle(&mut self, _: Start, _: &mut BackgroundJobs) -> Self::Response {
        self.desired_state = StartStop::Start;
        self.export_state().await?;
        self.persistent_container.start().await
    }
}
impl Service {
    pub async fn start(&self) -> Result<(), Error> {
        let start = self.actor.queue(Start);
        self.cancel_transition.notify_waiters();
        start.await?
    }
}

struct Stop;
#[async_trait::async_trait]
impl Handler<Stop> for ServiceActor {
    type Response = Result<(), Error>;
    async fn handle(&mut self, _: Stop, _: &mut BackgroundJobs) -> Self::Response {
        self.desired_state = StartStop::Stop;
        self.export_state().await?;
        self.persistent_container.stop().await
    }
}
impl Service {
    pub async fn stop(&self) -> Result<(), Error> {
        self.actor.send(Stop).await?
    }
}
