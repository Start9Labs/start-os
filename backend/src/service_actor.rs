use async_trait::async_trait;
use color_eyre::eyre::{bail, eyre};
use futures::TryFutureExt;
use patch_db::{DbHandle, Transaction};
use tokio::spawn;
use tokio::{
    pin,
    sync::{mpsc, oneshot},
};
use tracing::instrument;
use tracing::{debug, error};

use crate::db::DatabaseModel;
use crate::Error;
use crate::{
    db::model::{InstalledPackages, Services},
    s9pk::manifest::PackageId,
};

/// Shove a thunk to indicate that we are done.
type DoneSender = oneshot::Sender<()>;
/// Shove a thunk to indicate that we are done or we got an error;
type ResultDoneSender = oneshot::Sender<Result<(), Error>>;

type ServiceActorMessageSet = (ServiceActorMessage, ResultDoneSender);

macro_rules! bail_service {
    ($msg:literal $(,)?) => {
        return ServiceConsumptionState::Errored($crate::Error::new(
            color_eyre::eyre::eyre!("Unexpected message and services state"),
            $crate::ErrorKind::InvalidRequest,
        ));
    }; // ($err:expr $(,)?) => {
       //     return $crate::private::Err($crate::eyre!($err));
       // };
       // ($fmt:expr, $($arg:tt)*) => {
       //     return $crate::private::Err($crate::eyre!($fmt, $($arg)*));
       // };
}

pub enum ServiceActorMessage {
    /// Backup all the services
    BackupAll(DoneSender),

    /// Rest are private
    BackingUpPackage(PackageId),
    DoneBackingUpPackage(PackageId),
    DoneBackup(DoneSender),
}
impl ServiceActorMessage {
    pub fn tag(&self) -> &'static str {
        match self {
            ServiceActorMessage::BackupAll(_) => "BackupAll",
            ServiceActorMessage::BackingUp(_) => "Backup",
            ServiceActorMessage::BackingUpPackage(PackageId) => "BackingUpPackage",
            ServiceActorMessage::DoneBackingUpPackage(PackageId) => "DoneBackingUpPackage",
            ServiceActorMessage::DoneBackup(_) => "DoneBackup",
        }
    }
}

pub struct ServiceActor<Db: DbHandle> {
    ctx: Db,
    messages: mpsc::UnboundedSender<ServiceActorMessageSet>,
}

impl<Db: DbHandle> ServiceActor<Db> {
    pub fn new(ctx: Db) -> Self {
        let (messages, messages_incoming) = mpsc::unbounded_channel();
        Self { ctx, messages }
    }

    /// Deal with a message, and return a future when this message has been consumed.
    /// Note: This doesn't mean that the full message lifecycle has been completed.
    async fn send_message(&self, msg: ServiceActorMessage) -> Result<(), Error> {
        let (sender, receiver) = oneshot::channel();
        self.messages
            .send((msg, sender))
            .unwrap_or_else(|e| error!("Major error while trying to send"));
        receiver.await;
        Ok(())
    }

    fn consume_backup(
        &self,
        db: Db,
        services: Services,
        done: DoneSender,
    ) -> ServiceConsumptionState {
        let messages = self.messages.clones();
        let packages = match services {
            Services::BackingUp { packages } => packages,
            _ => bail_service!("Expected Services::BackingUp"),
        };
        let new_state = Services::BackingUp {
            packages: packages.clone(),
        };

        spawn(async move {
            let mut packages = packages;
            for (package_id, installed_model) in packages.clone().into_iter() {
                let main_status_model = installed_model.status.main;

                let (started, health) =
                    match main_status_model.get(&mut tx, true).await?.into_owned() {
                        MainStatus::Starting => (Some(Utc::now()), Default::default()),
                        MainStatus::Running { started, health } => (Some(started), health.clone()),
                        MainStatus::Stopped | MainStatus::Stopping => (None, Default::default()),
                        MainStatus::BackingUp { .. } => {
                            backup_report.insert(
                                package_id,
                                PackageBackupReport {
                                    error: Some(
                                        "Can't do backup because service is in a backing up state"
                                            .to_owned(),
                                    ),
                                },
                            );
                            continue;
                        }
                    };
                if let Err(e) = messages.send_message(ServiceActorMessage::BackingUp(package_id)) {
                    bail_service!("Could not send a message");
                };
                main_status_model
                    .put(
                        &mut tx,
                        &MainStatus::BackingUp {
                            started,
                            health: health.clone(),
                        },
                    )
                    .await?;
                tx.save().await?; // drop locks

                let manifest = installed_model
                    .clone()
                    .manifest()
                    .get(&mut db, false)
                    .await?;

                ctx.managers
                    .get(&(manifest.id.clone(), manifest.version.clone()))
                    .await
                    .ok_or_else(|| {
                        Error::new(eyre!("Manager not found"), crate::ErrorKind::InvalidRequest)
                    })?
                    .synchronize()
                    .await;

                let mut tx = db.begin().await?;

                installed_model.lock(&mut tx, LockType::Write).await?;

                let guard = backup_guard.mount_package_backup(&package_id).await?;
                let res = manifest
                    .backup
                    .create(
                        ctx,
                        &package_id,
                        &manifest.title,
                        &manifest.version,
                        &manifest.interfaces,
                        &manifest.volumes,
                    )
                    .await;
                guard.unmount().await?;
                backup_report.insert(
                    package_id.clone(),
                    PackageBackupReport {
                        error: res.as_ref().err().map(|e| e.to_string()),
                    },
                );

                if let Ok(pkg_meta) = res {
                    installed_model
                        .last_backup()
                        .put(&mut tx, &Some(pkg_meta.timestamp))
                        .await?;
                    backup_guard
                        .metadata
                        .package_backups
                        .insert(package_id, pkg_meta);
                }

                main_status_model
                    .put(
                        &mut tx,
                        &match started {
                            Some(started) => MainStatus::Running { started, health },
                            None => MainStatus::Stopped,
                        },
                    )
                    .await?;
                tx.save().await?;
            }
        });

        ServiceConsumptionState::Changed(new_state)
    }

    #[instrument(skip(self, db, services, msg))]
    fn consume(
        &self,
        db: Db,
        services: Services,
        msg: ServiceActorMessage,
    ) -> ServiceConsumptionState {
        return match msg {
            ServiceActorMessage::BackupAll(done) => self.consume_backup(db, services, done),
            ServiceActorMessage::BackingUpPackage(package_id) => todo!(),
            _ => todo!("Remove"),
        };
        // match (msg, services) {
        //     (ServiceActorMessage::BackingUp(done), _) => {}
        //     (
        //         ServiceActorMessage::DoneBackup(done),
        //         Services::BackingUp {
        //             mut packages,
        //             backing_up,
        //             to_back_ups,
        //         },
        //     ) => {
        //         if let Some((k, value)) = backing_up {
        //             packages.insert(k, value);
        //         }
        //         self.send_message(ServiceActorMessage::BackingUp(done));
        //         ServiceConsumptionState::Changed(Services::BackingUp {
        //             packages,
        //             backing_up: None,
        //             to_back_ups,
        //         })
        //     }
        //     (
        //         ServiceActorMessage::BackupAll(done),
        //         Services::Ready {
        //             packages: to_back_ups,
        //         },
        //     ) => {
        //         let packages = Default::default();
        //         self.send_message(ServiceActorMessage::BackingUp(done))
        //             .unwrap_or_else(|e| error!("Major error while trying to send"));
        //         ServiceConsumptionState::Changed(Services::BackingUp {
        //             packages,
        //             to_back_ups,
        //             backing_up: Default::default(),
        //         })
        //     }
        //     (
        //         sam @ (ServiceActorMessage::BackingUp(_)
        //         | ServiceActorMessage::BackupAll(_)
        //         | ServiceActorMessage::DoneBackup(_)),
        //         services,
        //     ) => {
        //         debug!(
        //             "Unexpected service ({}) and state of services ({})",
        //             sam.tag(),
        //             services.tag()
        //         );

        //         return ServiceConsumptionState::Errored(Error::new(
        //             eyre!("Unexpected message and services state"),
        //             crate::ErrorKind::InvalidRequest,
        //         ));
        //     }
        // }
    }

    fn spawn_consumer(
        &self,
        ctx: Db,
        messages_incoming: mpsc::UnboundedReceiver<ServiceActorMessageSet>,
    ) {
        spawn(async move {
            pin!(messages_incoming);
            while let Some(message_set) = messages_incoming.recv().await {
                let (msg, sender) = message_set;
                let mut tx: Transaction<&mut Db> = match ctx.begin().await {
                    Ok(tx) => tx,
                    Err(e) => {
                        debug!("{:?}", e);
                        let error = Error::new(
                            eyre!("Failed to begin transaction: {}", e),
                            crate::ErrorKind::InvalidRequest,
                        );
                        sender.send(Err(error)).unwrap();
                        continue;
                    }
                };
                let mut services = match DatabaseModel::new().services().get_mut(&mut tx).await {
                    Ok(x) => x,
                    Err(e) => {
                        error!("Failed to lock services: {}", e);
                        debug!("{:?}", e);
                        sender.send(Ok(())).unwrap();
                        continue;
                    }
                };
                match Self::consume(ctx, services.clone(), msg) {
                    ServiceConsumptionState::Changed(new_services) => {
                        *services = new_services;
                        if let Err(e) = (tx).commit(None).await {
                            error!("Failed to commit swap: {}", e);
                            debug!("{:?}", e);
                        }
                        sender.send(Ok(())).unwrap();
                    }
                    ServiceConsumptionState::NoChanges => {
                        if let Err(e) = (tx).abort().await {
                            error!("Failed to abort transaction: {}", e);
                            debug!("{:?}", e);
                        }
                        sender.send(Ok(())).unwrap();
                    }
                    ServiceConsumptionState::Errored(e) => {
                        sender.send(Err(e)).unwrap();
                    }
                };
            }
        });
    }
}

enum ServiceConsumptionState {
    Changed(Services),
    NoChanges,
    Errored(Error),
}
