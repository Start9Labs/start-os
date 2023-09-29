use std::sync::Arc;
use std::time::Duration;

use models::OptionExt;
use tokio::sync::watch;
use tokio::sync::watch::Sender;
use tracing::instrument;

use super::start_stop::StartStop;
use super::{manager_seed, run_main, ManagerPersistentContainer, RunMainResult};
use crate::prelude::*;
use crate::procedure::NoOutput;
use crate::s9pk::manifest::Manifest;
use crate::status::MainStatus;
use crate::util::NonDetachingJoinHandle;
use crate::Error;

pub type ManageContainerOverride = Arc<watch::Sender<Option<Override>>>;

pub type Override = MainStatus;

pub struct OverrideGuard {
    override_main_status: Option<ManageContainerOverride>,
}
impl OverrideGuard {
    pub fn drop(self) {}
}
impl Drop for OverrideGuard {
    fn drop(&mut self) {
        if let Some(override_main_status) = self.override_main_status.take() {
            override_main_status.send_modify(|x| {
                *x = None;
            });
        }
    }
}

/// This is the thing describing the state machine actor for a service
/// state and current running/ desired states.
pub struct ManageContainer {
    pub(super) current_state: Arc<watch::Sender<StartStop>>,
    pub(super) desired_state: Arc<watch::Sender<StartStop>>,
    _service: NonDetachingJoinHandle<()>,
    _save_state: NonDetachingJoinHandle<()>,
    override_main_status: ManageContainerOverride,
}

impl ManageContainer {
    pub async fn new(
        seed: Arc<manager_seed::ManagerSeed>,
        persistent_container: ManagerPersistentContainer,
    ) -> Result<Self, Error> {
        let current_state = Arc::new(watch::channel(StartStop::Stop).0);
        let desired_state = Arc::new(
            watch::channel::<StartStop>(
                get_status(seed.ctx.db.peek().await?, &seed.manifest).into(),
            )
            .0,
        );
        let override_main_status: ManageContainerOverride = Arc::new(watch::channel(None).0);
        let service = tokio::spawn(create_service_manager(
            desired_state.clone(),
            seed.clone(),
            current_state.clone(),
            persistent_container,
        ))
        .into();
        let save_state = tokio::spawn(save_state(
            desired_state.clone(),
            current_state.clone(),
            override_main_status.clone(),
            seed.clone(),
        ))
        .into();
        Ok(ManageContainer {
            current_state,
            desired_state,
            _service: service,
            override_main_status,
            _save_state: save_state,
        })
    }

    /// Set override is used during something like a restart of a service. We want to show certain statuses be different
    /// from the actual status of the service.
    pub fn set_override(&self, override_status: Override) -> Result<OverrideGuard, Error> {
        let status = Some(override_status);
        if self.override_main_status.borrow().is_some() {
            return Err(Error::new(
                eyre!("Already have an override"),
                ErrorKind::InvalidRequest,
            ));
        }
        self.override_main_status
            .send_modify(|x| *x = status.clone());
        Ok(OverrideGuard {
            override_main_status: Some(self.override_main_status.clone()),
        })
    }

    /// Set the override, but don't have a guard to revert it. Used only on the mananger to do a shutdown.
    pub(super) async fn lock_state_forever(
        &self,
        seed: &manager_seed::ManagerSeed,
    ) -> Result<(), Error> {
        let current_state = get_status(seed.ctx.db.peek().await?, &seed.manifest);
        self.override_main_status
            .send_modify(|x| *x = Some(current_state));
        Ok(())
    }

    /// We want to set the state of the service, like to start or stop
    pub fn to_desired(&self, new_state: StartStop) {
        self.desired_state.send_modify(|x| *x = new_state);
    }

    /// This is a tool to say wait for the service to be in a certain state.
    pub async fn wait_for_desired(&self, new_state: StartStop) {
        let mut current_state = self.current_state();
        self.to_desired(new_state);
        while *current_state.borrow() != new_state {
            current_state.changed().await.unwrap_or_default();
        }
    }

    /// Getter
    pub fn current_state(&self) -> watch::Receiver<StartStop> {
        self.current_state.subscribe()
    }

    /// Getter
    pub fn desired_state(&self) -> watch::Receiver<StartStop> {
        self.desired_state.subscribe()
    }
}

async fn create_service_manager(
    desired_state: Arc<Sender<StartStop>>,
    seed: Arc<manager_seed::ManagerSeed>,
    current_state: Arc<Sender<StartStop>>,
    persistent_container: Arc<Option<super::persistent_container::PersistentContainer>>,
) {
    let mut desired_state_receiver = desired_state.subscribe();
    let mut running_service: Option<NonDetachingJoinHandle<()>> = None;
    let seed = seed.clone();
    loop {
        let current: StartStop = *current_state.borrow();
        let desired: StartStop = *desired_state_receiver.borrow();
        match (current, desired) {
            (StartStop::Start, StartStop::Start) => (),
            (StartStop::Start, StartStop::Stop) => {
                if persistent_container.is_none() {
                    if let Err(err) = seed.stop_container().await {
                        tracing::error!("Could not stop container");
                        tracing::debug!("{:?}", err)
                    }
                    running_service = None;
                } else if let Some(current_service) = running_service.take() {
                    tokio::select! {
                        _ = current_service => (),
                        _ = tokio::time::sleep(Duration::from_secs_f64(seed.manifest
                            .containers
                            .as_ref()
                            .and_then(|c| c.main.sigterm_timeout).map(|x| x.as_secs_f64()).unwrap_or_default())) => {
                            tracing::error!("Could not stop service");
                        }
                    }
                }
                current_state.send_modify(|x| *x = StartStop::Stop);
            }
            (StartStop::Stop, StartStop::Start) => starting_service(
                current_state.clone(),
                desired_state.clone(),
                seed.clone(),
                persistent_container.clone(),
                &mut running_service,
            ),
            (StartStop::Stop, StartStop::Stop) => (),
        }

        if desired_state_receiver.changed().await.is_err() {
            tracing::error!("Desired state error");
            break;
        }
    }
}

async fn save_state(
    desired_state: Arc<Sender<StartStop>>,
    current_state: Arc<Sender<StartStop>>,
    override_main_status: ManageContainerOverride,
    seed: Arc<manager_seed::ManagerSeed>,
) {
    let mut desired_state_receiver = desired_state.subscribe();
    let mut current_state_receiver = current_state.subscribe();
    let mut override_main_status_receiver = override_main_status.subscribe();
    loop {
        let current: StartStop = *current_state_receiver.borrow();
        let desired: StartStop = *desired_state_receiver.borrow();
        let override_status = override_main_status_receiver.borrow().clone();
        let status = match (override_status.clone(), current, desired) {
            (Some(status), _, _) => status,
            (_, StartStop::Start, StartStop::Start) => MainStatus::Running {
                started: chrono::Utc::now(),
                health: Default::default(),
            },
            (_, StartStop::Start, StartStop::Stop) => MainStatus::Stopping,
            (_, StartStop::Stop, StartStop::Start) => MainStatus::Starting,
            (_, StartStop::Stop, StartStop::Stop) => MainStatus::Stopped,
        };

        let manifest = &seed.manifest;
        if let Err(err) = seed
            .ctx
            .db
            .mutate(|db| set_status(db, manifest, &status))
            .await
        {
            tracing::error!("Did not set status for {}", seed.container_name);
            tracing::debug!("{:?}", err);
        }
        tokio::select! {
            _ = desired_state_receiver.changed() =>{},
            _ = current_state_receiver.changed() => {},
            _ = override_main_status_receiver.changed() => {}
        }
    }
}

fn starting_service(
    current_state: Arc<Sender<StartStop>>,
    desired_state: Arc<Sender<StartStop>>,
    seed: Arc<manager_seed::ManagerSeed>,
    persistent_container: ManagerPersistentContainer,
    running_service: &mut Option<NonDetachingJoinHandle<()>>,
) {
    let set_running = {
        let current_state = current_state.clone();
        Arc::new(move || {
            current_state.send_modify(|x| *x = StartStop::Start);
        })
    };
    let set_stopped = { move || current_state.send_modify(|x| *x = StartStop::Stop) };
    let running_main_loop = async move {
        while desired_state.borrow().is_start() {
            let result = run_main(
                seed.clone(),
                persistent_container.clone(),
                set_running.clone(),
            )
            .await;
            set_stopped();
            run_main_log_result(result, seed.clone()).await;
        }
    };
    *running_service = Some(tokio::spawn(running_main_loop).into());
}

async fn run_main_log_result(result: RunMainResult, seed: Arc<manager_seed::ManagerSeed>) {
    match result {
        Ok(Ok(NoOutput)) => (), // restart
        Ok(Err(e)) => {
            tracing::error!(
                "The service {} has crashed with the following exit code: {}",
                seed.manifest.id.clone(),
                e.0
            );

            tokio::time::sleep(Duration::from_secs(15)).await;
        }
        Err(e) => {
            tracing::error!("failed to start service: {}", e);
            tracing::debug!("{:?}", e);
        }
    }
}

/// Used only in the mod where we are doing a backup
#[instrument(skip(db, manifest))]
pub(super) fn get_status(db: Peeked, manifest: &Manifest) -> MainStatus {
    db.as_package_data()
        .as_idx(&manifest.id)
        .and_then(|x| x.as_installed())
        .filter(|x| x.as_manifest().as_version().de().ok() == Some(manifest.version.clone()))
        .and_then(|x| x.as_status().as_main().de().ok())
        .unwrap_or(MainStatus::Stopped)
}

#[instrument(skip(db, manifest))]
fn set_status(db: &mut Peeked, manifest: &Manifest, main_status: &MainStatus) -> Result<(), Error> {
    let Some(installed) = db
        .as_package_data_mut()
        .as_idx_mut(&manifest.id)
        .or_not_found(&manifest.id)?
        .as_installed_mut()
    else {
        return Ok(());
    };
    installed.as_status_mut().as_main_mut().ser(main_status)
}
