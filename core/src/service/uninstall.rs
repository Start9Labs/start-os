use std::path::Path;

use imbl::vector;

use crate::context::RpcContext;
use crate::db::model::package::{InstalledState, InstallingInfo, InstallingState, PackageState};
use crate::prelude::*;
use crate::volume::PKG_VOLUME_DIR;
use crate::{DATA_DIR, PACKAGE_DATA, PackageId};

pub async fn cleanup(ctx: &RpcContext, id: &PackageId, soft: bool) -> Result<(), Error> {
    Ok(
        if let Some(pde) = ctx
            .db
            .mutate(|d| {
                if let Some(pde) = d
                    .as_public_mut()
                    .as_package_data_mut()
                    .remove(&id)?
                    .map(|d| d.de())
                    .transpose()?
                {
                    d.as_private_mut().as_available_ports_mut().mutate(|p| {
                        p.free(
                            pde.hosts
                                .0
                                .values()
                                .flat_map(|h| h.bindings.values())
                                .flat_map(|b| {
                                    b.net
                                        .assigned_port
                                        .into_iter()
                                        .chain(b.net.assigned_ssl_port)
                                }),
                        );
                        Ok(())
                    })?;
                    d.as_private_mut().as_package_stores_mut().remove(&id)?;
                    Ok(Some(pde))
                } else {
                    Ok(None)
                }
            })
            .await
            .result?
        {
            let manifest = match pde.state_info {
                PackageState::Installing(InstallingState {
                    installing_info:
                        InstallingInfo {
                            new_manifest: manifest,
                            ..
                        },
                })
                | PackageState::Restoring(InstallingState {
                    installing_info:
                        InstallingInfo {
                            new_manifest: manifest,
                            ..
                        },
                })
                | PackageState::Removing(InstalledState { manifest }) => manifest,
                s => {
                    return Err(Error::new(
                        eyre!("Invalid package state for cleanup: {s:?}"),
                        ErrorKind::InvalidRequest,
                    ));
                }
            };
            // Trigger manifest callbacks with null to indicate uninstall
            if let Some(callbacks) = ctx.callbacks.get_service_manifest(&manifest.id) {
                callbacks.call(vector![Value::Null]).await.log_err();
            }

            if !soft {
                let path = Path::new(DATA_DIR).join(PKG_VOLUME_DIR).join(&manifest.id);
                if tokio::fs::metadata(&path).await.is_ok() {
                    tokio::fs::remove_dir_all(&path).await?;
                }
                let logs_dir = Path::new(PACKAGE_DATA).join("logs").join(&manifest.id);
                if tokio::fs::metadata(&logs_dir).await.is_ok() {
                    #[cfg(not(feature = "dev"))]
                    tokio::fs::remove_dir_all(&logs_dir).await?;
                }
            }
        },
    )
}
