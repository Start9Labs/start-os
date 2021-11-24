use std::collections::BTreeMap;
use std::convert::TryInto;
use std::sync::atomic::Ordering;
use std::time::Duration;

use chrono::Utc;

use super::{pause, resume, start, stop, ManagerSharedState, Status};
use crate::status::MainStatus;
use crate::Error;

/// Allocates a db handle. DO NOT CALL with a db handle already in scope
async fn synchronize_once(shared: &ManagerSharedState) -> Result<(), Error> {
    let mut db = shared.ctx.db.handle();
    let mut status = crate::db::DatabaseModel::new()
        .package_data()
        .idx_model(&shared.manifest.id)
        .expect(&mut db)
        .await?
        .installed()
        .expect(&mut db)
        .await?
        .status()
        .main()
        .get_mut(&mut db)
        .await?;
    match shared.status.load(Ordering::SeqCst).try_into().unwrap() {
        Status::Stopped => match &mut *status {
            MainStatus::Stopped => (),
            MainStatus::Stopping => {
                *status = MainStatus::Stopped;
            }
            MainStatus::Starting => {
                start(shared).await?;
            }
            MainStatus::Running { started, .. } => {
                *started = Utc::now();
                start(shared).await?;
            }
            MainStatus::BackingUp { .. } => (),
        },
        Status::Starting => match *status {
            MainStatus::Stopped | MainStatus::Stopping => {
                stop(shared).await?;
            }
            MainStatus::Starting | MainStatus::Running { .. } => (),
            MainStatus::BackingUp { .. } => {
                pause(shared).await?;
            }
        },
        Status::Running => match *status {
            MainStatus::Stopped | MainStatus::Stopping => {
                stop(shared).await?;
            }
            MainStatus::Starting => {
                *status = MainStatus::Running {
                    started: Utc::now(),
                    health: BTreeMap::new(),
                };
            }
            MainStatus::Running { .. } => (),
            MainStatus::BackingUp { .. } => {
                pause(shared).await?;
            }
        },
        Status::Paused => match *status {
            MainStatus::Stopped | MainStatus::Stopping => {
                stop(shared).await?;
            }
            MainStatus::Starting | MainStatus::Running { .. } => {
                resume(shared).await?;
            }
            MainStatus::BackingUp { .. } => (),
        },
    }
    status.save(&mut db).await?;
    Ok(())
}

pub async fn synchronizer(shared: &ManagerSharedState) {
    loop {
        if let Err(e) = synchronize_once(shared).await {
            tracing::error!(
                "Synchronizer for {}@{} failed: {}",
                shared.manifest.id,
                shared.manifest.version,
                e
            );
            tracing::debug!("{:?}", e);
        } else {
            tracing::trace!("{} status synchronized", shared.manifest.id);
            shared.synchronized.notify_waiters();
        }
        tokio::select! {
            _ = tokio::time::sleep(Duration::from_secs(5)) => (),
            _ = shared.synchronize_now.notified() => (),
        }
    }
}
