use std::path::Path;

use models::PackageId;

use crate::context::RpcContext;
use crate::prelude::*;
use crate::volume::data_dir;
use crate::{DATA_DIR, PACKAGE_DATA};

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
            let state = pde.state_info.expect_removing()?;
            if !soft {
                for volume_id in &state.manifest.volumes {
                    let path = data_dir(DATA_DIR, &state.manifest.id, volume_id);
                    if tokio::fs::metadata(&path).await.is_ok() {
                        tokio::fs::remove_dir_all(&path).await?;
                    }
                }
                let logs_dir = Path::new(PACKAGE_DATA)
                    .join("logs")
                    .join(&state.manifest.id);
                if tokio::fs::metadata(&logs_dir).await.is_ok() {
                    tokio::fs::remove_dir_all(&logs_dir).await?;
                }
            }
        },
    )
}
