use std::collections::BTreeMap;
use std::sync::Arc;
use std::time::Duration;

use models::PackageId;
use tokio::sync::watch;
use tokio::sync::watch::Sender;
use tracing::instrument;

use super::manager_seed;
use super::start_stop::StartStop;
use crate::manager::persistent_container::PersistentContainer;
use crate::prelude::*;
use crate::status::MainStatus;
use crate::util::{GeneralBoxedGuard, NonDetachingJoinHandle};
use crate::{db::model::DatabaseModel, manager::manager_seed::ManagerSeed};

pub type ManageContainerOverride = Arc<watch::Sender<Option<MainStatus>>>;

// ManagerSeed + PersistentContainer
pub struct ManageContainer {
    pub(super) current_state: Arc<watch::Sender<StartStop>>,
    pub(super) desired_state: Arc<watch::Sender<StartStop>>,
    _service: NonDetachingJoinHandle<()>, // CurrentState + DesiredState + ManagerSeed + PersistentContainer
    _save_state: NonDetachingJoinHandle<()>, // CurrentState + DesiredState + ManagerSeed + ManageContainerOverride
    override_main_status: ManageContainerOverride,
}

impl ManageContainer {
    pub async fn new(
        seed: Arc<manager_seed::ManagerSeed>,
        persistent_container: Arc<PersistentContainer>,
    ) -> Result<Self, Error> {
        let db = &seed.ctx.db.peek().await?;
        let current_state = Arc::new(watch::channel(StartStop::Stop).0);
        let desired_state =
            Arc::new(watch::channel::<StartStop>(get_status(db, &seed.manifest.id).into()).0);
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

    pub fn set_override(&self, override_status: Option<MainStatus>) -> GeneralBoxedGuard {
        self.override_main_status
            .send(override_status)
            .unwrap_or_default();
        let override_main_status = self.override_main_status.clone();
        let guard = GeneralBoxedGuard::new(move || {
            override_main_status.send(None).unwrap_or_default();
        });
        guard
    }

    pub fn to_desired(&self, new_state: StartStop) {
        self.desired_state.send(new_state).unwrap_or_default();
    }

    pub async fn wait_for_desired(&self, new_state: StartStop) {
        let mut current_state = self.current_state();
        self.to_desired(new_state);
        while *current_state.borrow() != new_state {
            current_state.changed().await.unwrap_or_default();
        }
    }

    pub fn current_state(&self) -> watch::Receiver<StartStop> {
        self.current_state.subscribe()
    }

    pub fn desired_state(&self) -> watch::Receiver<StartStop> {
        self.desired_state.subscribe()
    }
}

async fn create_service_manager(
    desired_state: Arc<Sender<StartStop>>,
    seed: Arc<ManagerSeed>,
    current_state: Arc<Sender<StartStop>>,
    persistent_container: Arc<PersistentContainer>,
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
                if let Some(current_service) = running_service.take() {
                    tokio::select! {
                        _ = current_service => (),
                        _ = tokio::time::sleep(Duration::from_secs_f64(seed.manifest
                            .containers
                            .main.sigterm_timeout.as_ref().map(|x| x.as_secs_f64()).unwrap_or_default())) => {
                            tracing::error!("Could not stop service");
                        }
                    }
                }
                current_state.send(StartStop::Stop).unwrap_or_default();
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
    override_main_status: Arc<Sender<Option<MainStatus>>>,
    seed: Arc<manager_seed::ManagerSeed>,
) {
    let mut desired_state_receiver = desired_state.subscribe();
    let mut current_state_receiver = current_state.subscribe();
    let mut override_main_status_receiver = override_main_status.subscribe();
    loop {
        let current: StartStop = current_state_receiver.borrow().clone();
        let desired: StartStop = desired_state_receiver.borrow().clone();
        let override_status = override_main_status_receiver.borrow().clone();
        if let Err(err) = async move {
            let db = &seed.ctx.db.peek().await?;
            seed.ctx
                .db
                .mutate(|db| match (override_status, current, desired) {
                    (Some(status), _, _) => set_status(db, &seed.manifest.id, &status),
                    (None, StartStop::Start, StartStop::Start) => set_status(
                        db,
                        &seed.manifest.id,
                        &MainStatus::Running {
                            started: chrono::Utc::now(),
                            health: BTreeMap::new(),
                        },
                    ),
                    (None, StartStop::Start, StartStop::Stop) => {
                        set_status(db, &seed.manifest.id, &MainStatus::Stopping)
                    }
                    (None, StartStop::Stop, StartStop::Start) => {
                        set_status(db, &seed.manifest.id, &MainStatus::Starting)
                    }
                    (None, StartStop::Stop, StartStop::Stop) => {
                        set_status(db, &seed.manifest.id, &MainStatus::Stopped)
                    }
                })
                .await?;
            Ok(())
        }
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
    persistent_container: Arc<PersistentContainer>,
    running_service: &mut Option<NonDetachingJoinHandle<()>>,
) {
    let set_running = {
        let current_state = current_state.clone();
        Arc::new(move || {
            current_state.send(StartStop::Start).unwrap_or_default();
        })
    };
    let set_stopped = { move || current_state.send(StartStop::Stop) };
    let running_main_loop = async move {
        while desired_state.borrow().is_start() {
            let db = seed.ctx.db.peek().await.unwrap();
            let result = run_main(&db, persistent_container.clone(), set_running.clone()).await;
            set_stopped().unwrap_or_default();
            run_main_log_result(result, seed.clone()).await;
        }
    };
    *running_service = Some(tokio::spawn(running_main_loop).into());
}

#[instrument(skip(db))]
pub(super) fn get_status(db: &DatabaseModel, id: &PackageId) -> Result<MainStatus, Error> {
    db.as_package_data()
        .as_idx(id)
        .or_not_found(id)?
        .expect_as_installed()?
        .as_installed()
        .as_status()
        .as_main()
        .clone()
        .de()
}

#[instrument(skip(db))]
fn set_status(
    db: &mut DatabaseModel,
    id: &PackageId,
    main_status: &MainStatus,
) -> Result<(), Error> {
    db.as_package_data_mut()
        .as_idx_mut(id)
        .or_not_found(id)?
        .expect_as_installed_mut()?
        .as_installed_mut()
        .as_status_mut()
        .as_main_mut()
        .ser(main_status)
}
