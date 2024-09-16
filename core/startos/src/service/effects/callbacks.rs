use std::cmp::min;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::{Arc, Mutex, Weak};
use std::time::{Duration, SystemTime};

use futures::future::join_all;
use helpers::NonDetachingJoinHandle;
use imbl::{vector, Vector};
use imbl_value::InternedString;
use models::{HostId, PackageId, ServiceInterfaceId};
use patch_db::json_ptr::JsonPointer;
use tracing::warn;

use crate::net::ssl::FullchainCertData;
use crate::prelude::*;
use crate::service::effects::context::EffectContext;
use crate::service::effects::net::ssl::Algorithm;
use crate::service::rpc::CallbackHandle;
use crate::service::{Service, ServiceActorSeed};
use crate::util::collections::EqMap;

#[derive(Default)]
pub struct ServiceCallbacks(Mutex<ServiceCallbackMap>);

#[derive(Default)]
struct ServiceCallbackMap {
    get_service_interface: BTreeMap<(PackageId, ServiceInterfaceId), Vec<CallbackHandler>>,
    list_service_interfaces: BTreeMap<PackageId, Vec<CallbackHandler>>,
    get_system_smtp: Vec<CallbackHandler>,
    get_host_info: BTreeMap<(PackageId, HostId), Vec<CallbackHandler>>,
    get_ssl_certificate: EqMap<
        (BTreeSet<InternedString>, FullchainCertData, Algorithm),
        (NonDetachingJoinHandle<()>, Vec<CallbackHandler>),
    >,
    get_store: BTreeMap<PackageId, BTreeMap<JsonPointer, Vec<CallbackHandler>>>,
}

impl ServiceCallbacks {
    fn mutate<T>(&self, f: impl FnOnce(&mut ServiceCallbackMap) -> T) -> T {
        let mut this = self.0.lock().unwrap();
        f(&mut *this)
    }

    pub fn gc(&self) {
        self.mutate(|this| {
            this.get_service_interface.retain(|_, v| {
                v.retain(|h| h.handle.is_active() && h.seed.strong_count() > 0);
                !v.is_empty()
            });
            this.list_service_interfaces.retain(|_, v| {
                v.retain(|h| h.handle.is_active() && h.seed.strong_count() > 0);
                !v.is_empty()
            });
            this.get_system_smtp
                .retain(|h| h.handle.is_active() && h.seed.strong_count() > 0);
            this.get_host_info.retain(|_, v| {
                v.retain(|h| h.handle.is_active() && h.seed.strong_count() > 0);
                !v.is_empty()
            });
            this.get_ssl_certificate.retain(|_, (_, v)| {
                v.retain(|h| h.handle.is_active() && h.seed.strong_count() > 0);
                !v.is_empty()
            });
            this.get_store.retain(|_, v| {
                v.retain(|_, v| {
                    v.retain(|h| h.handle.is_active() && h.seed.strong_count() > 0);
                    !v.is_empty()
                });
                !v.is_empty()
            });
        })
    }

    pub(super) fn add_get_service_interface(
        &self,
        package_id: PackageId,
        service_interface_id: ServiceInterfaceId,
        handler: CallbackHandler,
    ) {
        self.mutate(|this| {
            this.get_service_interface
                .entry((package_id, service_interface_id))
                .or_default()
                .push(handler);
        })
    }

    #[must_use]
    pub fn get_service_interface(
        &self,
        id: &(PackageId, ServiceInterfaceId),
    ) -> Option<CallbackHandlers> {
        self.mutate(|this| {
            Some(CallbackHandlers(
                this.get_service_interface.remove(id).unwrap_or_default(),
            ))
            .filter(|cb| !cb.0.is_empty())
        })
    }

    pub(super) fn add_list_service_interfaces(
        &self,
        package_id: PackageId,
        handler: CallbackHandler,
    ) {
        self.mutate(|this| {
            this.list_service_interfaces
                .entry(package_id)
                .or_default()
                .push(handler);
        })
    }

    #[must_use]
    pub fn list_service_interfaces(&self, id: &PackageId) -> Option<CallbackHandlers> {
        self.mutate(|this| {
            Some(CallbackHandlers(
                this.list_service_interfaces.remove(id).unwrap_or_default(),
            ))
            .filter(|cb| !cb.0.is_empty())
        })
    }

    pub(super) fn add_get_system_smtp(&self, handler: CallbackHandler) {
        self.mutate(|this| {
            this.get_system_smtp.push(handler);
        })
    }

    #[must_use]
    pub fn get_system_smtp(&self) -> Option<CallbackHandlers> {
        self.mutate(|this| {
            Some(CallbackHandlers(std::mem::take(&mut this.get_system_smtp)))
                .filter(|cb| !cb.0.is_empty())
        })
    }

    pub(super) fn add_get_host_info(
        &self,
        package_id: PackageId,
        host_id: HostId,
        handler: CallbackHandler,
    ) {
        self.mutate(|this| {
            this.get_host_info
                .entry((package_id, host_id))
                .or_default()
                .push(handler);
        })
    }

    #[must_use]
    pub fn get_host_info(&self, id: &(PackageId, HostId)) -> Option<CallbackHandlers> {
        self.mutate(|this| {
            Some(CallbackHandlers(
                this.get_host_info.remove(id).unwrap_or_default(),
            ))
            .filter(|cb| !cb.0.is_empty())
        })
    }

    pub(super) fn add_get_ssl_certificate(
        &self,
        ctx: EffectContext,
        hostnames: BTreeSet<InternedString>,
        cert: FullchainCertData,
        algorithm: Algorithm,
        handler: CallbackHandler,
    ) {
        self.mutate(|this| {
            this.get_ssl_certificate
                .entry((hostnames.clone(), cert.clone(), algorithm))
                .or_insert_with(|| {
                    (
                        tokio::spawn(async move {
                            if let Err(e) = async {
                                loop {
                                    match cert
                                        .expiration()
                                        .ok()
                                        .and_then(|e| e.duration_since(SystemTime::now()).ok())
                                    {
                                        Some(d) => {
                                            tokio::time::sleep(min(Duration::from_secs(86400), d))
                                                .await
                                        }
                                        _ => break,
                                    }
                                }
                                let Ok(ctx) = ctx.deref() else {
                                    return Ok(());
                                };

                                if let Some((_, callbacks)) =
                                    ctx.seed.ctx.callbacks.mutate(|this| {
                                        this.get_ssl_certificate
                                            .remove(&(hostnames, cert, algorithm))
                                    })
                                {
                                    CallbackHandlers(callbacks).call(vector![]).await?;
                                }
                                Ok::<_, Error>(())
                            }
                            .await
                            {
                                tracing::error!(
                                    "Error in callback handler for getSslCertificate: {e}"
                                );
                                tracing::debug!("{e:?}");
                            }
                        })
                        .into(),
                        Vec::new(),
                    )
                })
                .1
                .push(handler);
        })
    }

    pub(super) fn add_get_store(
        &self,
        package_id: PackageId,
        path: JsonPointer,
        handler: CallbackHandler,
    ) {
        self.mutate(|this| {
            this.get_store
                .entry(package_id)
                .or_default()
                .entry(path)
                .or_default()
                .push(handler)
        })
    }

    #[must_use]
    pub fn get_store(
        &self,
        package_id: &PackageId,
        path: &JsonPointer,
    ) -> Option<CallbackHandlers> {
        self.mutate(|this| {
            if let Some(watched) = this.get_store.get_mut(package_id) {
                let mut res = Vec::new();
                watched.retain(|ptr, cbs| {
                    if ptr.starts_with(path) || path.starts_with(ptr) {
                        res.append(cbs);
                        false
                    } else {
                        true
                    }
                });
                Some(CallbackHandlers(res))
            } else {
                None
            }
            .filter(|cb| !cb.0.is_empty())
        })
    }
}

pub struct CallbackHandler {
    handle: CallbackHandle,
    seed: Weak<ServiceActorSeed>,
}
impl CallbackHandler {
    pub fn new(service: &Service, handle: CallbackHandle) -> Self {
        Self {
            handle,
            seed: Arc::downgrade(&service.seed),
        }
    }
    pub async fn call(mut self, args: Vector<Value>) -> Result<(), Error> {
        if let Some(seed) = self.seed.upgrade() {
            seed.persistent_container
                .callback(self.handle.take(), args)
                .await?;
        }
        Ok(())
    }
}
impl Drop for CallbackHandler {
    fn drop(&mut self) {
        if self.handle.is_active() {
            warn!("Callback handler dropped while still active!");
        }
    }
}

pub struct CallbackHandlers(Vec<CallbackHandler>);
impl CallbackHandlers {
    pub async fn call(self, args: Vector<Value>) -> Result<(), Error> {
        let mut err = ErrorCollection::new();
        for res in join_all(self.0.into_iter().map(|cb| cb.call(args.clone()))).await {
            err.handle(res);
        }
        err.into_result()
    }
}

pub(super) fn clear_callbacks(context: EffectContext) -> Result<(), Error> {
    let context = context.deref()?;
    context
        .seed
        .persistent_container
        .state
        .send_if_modified(|s| !std::mem::take(&mut s.callbacks).is_empty());
    context.seed.ctx.callbacks.gc();
    Ok(())
}
