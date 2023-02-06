use std::sync::Arc;
use std::time::Duration;

use patch_db::PatchDbHandle;
use tokio::sync::watch;
use tokio::sync::watch::Sender;

use super::start_stop::StartStop;
use super::{manager_seed, run_main, ManagerPersistentContainer, RunMainResult};
use crate::s9pk::manifest::Manifest;
use crate::status::MainStatus;
use crate::util::{GeneralBoxedGuard, NonDetachingJoinHandle};
use crate::Error;

pub type ManageContainerOverride = Arc<watch::Sender<Option<MainStatus>>>;

pub struct ManageContainer {
    current_state: Arc<watch::Sender<StartStop>>,
    desired_state: Arc<watch::Sender<StartStop>>,
    service: NonDetachingJoinHandle<()>,
    save_state: NonDetachingJoinHandle<()>,
    override_main_status: ManageContainerOverride,
}

impl ManageContainer {
    pub async fn new(
        seed: Arc<manager_seed::ManagerSeed>,
        persistent_container: ManagerPersistentContainer,
    ) -> Result<Self, Error> {
        let mut db = seed.ctx.db.handle();
        let current_state = Arc::new(watch::channel(StartStop::Stop).0);
        let desired_state = Arc::new(
            watch::channel::<StartStop>(get_status(&mut db, &seed.manifest).await?.into()).0,
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
            service,
            override_main_status,
            save_state,
        })
    }

    pub fn set_override(&self, override_status: Option<MainStatus>) -> GeneralBoxedGuard {
        self.override_main_status.send(override_status);
        let override_main_status = self.override_main_status.clone();
        let guard = GeneralBoxedGuard::new(move || {
            override_main_status.send(None);
        });
        guard
    }

    pub fn to_desired(&self, new_state: StartStop) {
        self.desired_state.send(new_state);
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
    seed: Arc<manager_seed::ManagerSeed>,
    current_state: Arc<Sender<StartStop>>,
    persistent_container: Arc<Option<super::persistent_container::PersistentContainer>>,
) {
    let mut desired_state_receiver = desired_state.subscribe();
    let mut running_service: Option<NonDetachingJoinHandle<()>> = None;
    let seed = seed.clone();
    loop {
        let current: StartStop = current_state.borrow().clone();
        let desired: StartStop = desired_state_receiver.borrow().clone();
        match (current, desired) {
            (StartStop::Start, StartStop::Start) => continue,
            (StartStop::Start, StartStop::Stop) => {
                if let Err(err) = seed.stop_container().await {
                    tracing::error!("Could not stop container");
                    tracing::debug!("{:?}", err)
                };
                running_service = None;
                current_state.send(StartStop::Stop);
            }
            (StartStop::Stop, StartStop::Start) => starting_service(
                current_state.clone(),
                desired_state.clone(),
                seed.clone(),
                persistent_container.clone(),
                &mut running_service,
            ),
            (StartStop::Stop, StartStop::Stop) => continue,
        }
        if let Err(_) = desired_state_receiver.changed().await {
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
        let mut db = seed.ctx.db.handle();
        let res = match (override_status, current, desired) {
            (Some(status), _, _) => set_status(&mut db, &seed.manifest, &status).await,
            (None, StartStop::Start, StartStop::Start) => {
                set_status(
                    &mut db,
                    &seed.manifest,
                    &MainStatus::Running {
                        started: chrono::Utc::now(),
                        health: Default::default(),
                    },
                )
                .await
            }
            (None, StartStop::Start, StartStop::Stop) => {
                set_status(&mut db, &seed.manifest, &MainStatus::Stopping).await
            }
            (None, StartStop::Stop, StartStop::Start) => {
                set_status(
                    &mut db,
                    &seed.manifest,
                    &MainStatus::Starting { restarting: false },
                )
                .await
            }
            (None, StartStop::Stop, StartStop::Stop) => {
                set_status(&mut db, &seed.manifest, &MainStatus::Stopped).await
            }
        };
        if let Err(err) = res {
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
            current_state.send(StartStop::Start);
        })
    };
    let set_stopped = {
        let current_state = current_state.clone();
        move || current_state.send(StartStop::Stop)
    };
    let running_main_loop = async move {
        while desired_state.borrow().is_start() {
            let result = run_main(
                seed.clone(),
                persistent_container.clone(),
                set_running.clone(),
            )
            .await;
            run_main_log_result(result, seed.clone());
            set_stopped();
        }
    };
    *running_service = Some(tokio::spawn(running_main_loop).into());
}

async fn run_main_log_result(result: RunMainResult, seed: Arc<manager_seed::ManagerSeed>) {
    match result {
        Ok(Ok(NoOutput)) => (), // restart
        Ok(Err(e)) => {
            #[cfg(feature = "unstable")]
            {
                use crate::notifications::NotificationLevel;
                use crate::status::MainStatus;
                let mut db = seed.ctx.db.handle();
                let started = crate::db::DatabaseModel::new()
                    .package_data()
                    .idx_model(&thread_shared.seed.manifest.id)
                    .and_then(|pde| pde.installed())
                    .map::<_, MainStatus>(|i| i.status().main())
                    .get(db, false)
                    .await;
                match started.as_deref() {
                    Ok(Some(MainStatus::Running { .. })) => {
                        let res = thread_shared.seed.ctx.notification_manager
                            .notify(
                                db,
                                Some(thread_shared.seed.manifest.id.clone()),
                                NotificationLevel::Warning,
                                String::from("Service Crashed"),
                                format!("The service {} has crashed with the following exit code: {}\nDetails: {}", thread_shared.seed.manifest.id.clone(), e.0, e.1),
                                (),
                                Some(3600) // 1 hour
                            )
                            .await;
                        if let Err(e) = res {
                            tracing::error!("Failed to issue notification: {}", e);
                            tracing::debug!("{:?}", e);
                        }
                    }
                    _ => {
                        tracing::error!("service just started. not issuing crash notification")
                    }
                }
            }

            tokio::time::sleep(Duration::from_secs(15)).await;
        }
        Err(e) => {
            tracing::error!("failed to start service: {}", e);
            tracing::debug!("{:?}", e);
        }
    }
}

pub(super) async fn get_status(
    db: &mut PatchDbHandle,
    manifest: &Manifest,
) -> Result<MainStatus, Error> {
    Ok(crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&manifest.id)
        .expect(db)
        .await?
        .installed()
        .expect(db)
        .await?
        .status()
        .main()
        .get(db)
        .await?
        .clone())
}

async fn set_status(
    db: &mut PatchDbHandle,
    manifest: &Manifest,
    main_status: &MainStatus,
) -> Result<(), Error> {
    crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&manifest.id)
        .expect(db)
        .await?
        .installed()
        .expect(db)
        .await?
        .status()
        .main()
        .put(db, main_status)
        .await?;
    Ok(())
}
