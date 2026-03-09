use std::cmp::min;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::{Arc, Weak};
use std::time::{Duration, SystemTime};

use clap::Parser;
use futures::future::join_all;
use imbl::{OrdMap, Vector, vector};
use imbl_value::InternedString;
use patch_db::TypedDbWatch;
use serde::{Deserialize, Serialize};
use tracing::warn;
use ts_rs::TS;

use crate::db::model::public::NetworkInterfaceInfo;
use crate::net::host::Host;
use crate::net::ssl::FullchainCertData;
use crate::prelude::*;
use crate::service::effects::context::EffectContext;
use crate::service::effects::net::ssl::Algorithm;
use crate::service::rpc::{CallbackHandle, CallbackId};
use crate::service::{Service, ServiceActorSeed};
use crate::util::collections::EqMap;
use crate::util::future::NonDetachingJoinHandle;
use crate::util::sync::SyncMutex;
use crate::status::StatusInfo;
use crate::{GatewayId, HostId, PackageId, ServiceInterfaceId};

/// Abstraction for callbacks that are triggered by patchdb subscriptions.
///
/// Handles the subscribe-wait-fire-remove pattern: when a callback is first
/// registered for a key, a patchdb subscription is spawned. When the subscription
/// fires, all handlers are consumed and invoked, then the subscription stops.
/// A new subscription is created if a handler is registered again.
pub struct DbWatchedCallbacks<K: Ord> {
    label: &'static str,
    inner: SyncMutex<BTreeMap<K, (NonDetachingJoinHandle<()>, Vec<CallbackHandler>)>>,
}

impl<K: Ord + Clone + Send + Sync + 'static> DbWatchedCallbacks<K> {
    pub fn new(label: &'static str) -> Self {
        Self {
            label,
            inner: SyncMutex::new(BTreeMap::new()),
        }
    }

    pub fn add<T: Send + 'static>(
        self: &Arc<Self>,
        key: K,
        watch: TypedDbWatch<T>,
        handler: CallbackHandler,
    ) {
        self.inner.mutate(|map| {
            map.entry(key.clone())
                .or_insert_with(|| {
                    let this = Arc::clone(self);
                    let k = key;
                    let label = self.label;
                    (
                        tokio::spawn(async move {
                            let mut watch = watch.untyped();
                            if watch.changed().await.is_ok() {
                                if let Some(cbs) = this.inner.mutate(|map| {
                                    map.remove(&k)
                                        .map(|(_, handlers)| CallbackHandlers(handlers))
                                        .filter(|cb| !cb.0.is_empty())
                                }) {
                                    let value = watch
                                        .peek_and_mark_seen()
                                        .unwrap_or_default();
                                    if let Err(e) = cbs.call(vector![value]).await {
                                        tracing::error!("Error in {label} callback: {e}");
                                        tracing::debug!("{e:?}");
                                    }
                                }
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

    pub fn gc(&self) {
        self.inner.mutate(|map| {
            map.retain(|_, (_, v)| {
                v.retain(|h| h.handle.is_active() && h.seed.strong_count() > 0);
                !v.is_empty()
            });
        })
    }
}

pub struct ServiceCallbacks {
    inner: SyncMutex<ServiceCallbackMap>,
    get_host_info: Arc<DbWatchedCallbacks<(PackageId, HostId)>>,
    get_status: Arc<DbWatchedCallbacks<PackageId>>,
}

impl Default for ServiceCallbacks {
    fn default() -> Self {
        Self {
            inner: SyncMutex::new(ServiceCallbackMap::default()),
            get_host_info: Arc::new(DbWatchedCallbacks::new("host info")),
            get_status: Arc::new(DbWatchedCallbacks::new("get_status")),
        }
    }
}

#[derive(Default)]
struct ServiceCallbackMap {
    get_service_interface: BTreeMap<(PackageId, ServiceInterfaceId), Vec<CallbackHandler>>,
    list_service_interfaces: BTreeMap<PackageId, Vec<CallbackHandler>>,
    get_system_smtp: Vec<CallbackHandler>,
    get_ssl_certificate: EqMap<
        (BTreeSet<InternedString>, FullchainCertData, Algorithm),
        (NonDetachingJoinHandle<()>, Vec<CallbackHandler>),
    >,
    get_container_ip: BTreeMap<PackageId, Vec<CallbackHandler>>,
    get_service_manifest: BTreeMap<PackageId, Vec<CallbackHandler>>,
    get_outbound_gateway: BTreeMap<PackageId, (NonDetachingJoinHandle<()>, Vec<CallbackHandler>)>,
}

impl ServiceCallbacks {
    fn mutate<T>(&self, f: impl FnOnce(&mut ServiceCallbackMap) -> T) -> T {
        self.inner.mutate(f)
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
            this.get_ssl_certificate.retain(|_, (_, v)| {
                v.retain(|h| h.handle.is_active() && h.seed.strong_count() > 0);
                !v.is_empty()
            });
            this.get_service_manifest.retain(|_, v| {
                v.retain(|h| h.handle.is_active() && h.seed.strong_count() > 0);
                !v.is_empty()
            });
            this.get_outbound_gateway.retain(|_, (_, v)| {
                v.retain(|h| h.handle.is_active() && h.seed.strong_count() > 0);
                !v.is_empty()
            });
        });
        self.get_host_info.gc();
        self.get_status.gc();
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
        watch: TypedDbWatch<Host>,
        handler: CallbackHandler,
    ) {
        self.get_host_info
            .add((package_id, host_id), watch, handler);
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

    pub(super) fn add_get_status(
        &self,
        package_id: PackageId,
        watch: TypedDbWatch<StatusInfo>,
        handler: CallbackHandler,
    ) {
        self.get_status.add(package_id, watch, handler);
    }

    pub(super) fn add_get_container_ip(&self, package_id: PackageId, handler: CallbackHandler) {
        self.mutate(|this| {
            this.get_container_ip
                .entry(package_id)
                .or_default()
                .push(handler)
        })
    }

    #[must_use]
    pub fn get_container_ip(&self, package_id: &PackageId) -> Option<CallbackHandlers> {
        self.mutate(|this| {
            this.get_container_ip
                .remove(package_id)
                .map(CallbackHandlers)
                .filter(|cb| !cb.0.is_empty())
        })
    }

    /// Register a callback for outbound gateway changes.
    pub(super) fn add_get_outbound_gateway(
        self: &Arc<Self>,
        package_id: PackageId,
        mut outbound_gateway: TypedDbWatch<Option<GatewayId>>,
        mut default_outbound: Option<TypedDbWatch<Option<GatewayId>>>,
        mut fallback: Option<TypedDbWatch<OrdMap<GatewayId, NetworkInterfaceInfo>>>,
        handler: CallbackHandler,
    ) {
        self.mutate(|this| {
            this.get_outbound_gateway
                .entry(package_id.clone())
                .or_insert_with(|| {
                    let callbacks = Arc::clone(self);
                    let key = package_id;
                    (
                        tokio::spawn(async move {
                            tokio::select! {
                                _ = outbound_gateway.changed() => {}
                                _ = async {
                                    if let Some(ref mut w) = default_outbound {
                                        let _ = w.changed().await;
                                    } else {
                                        std::future::pending::<()>().await;
                                    }
                                } => {}
                                _ = async {
                                    if let Some(ref mut w) = fallback {
                                        let _ = w.changed().await;
                                    } else {
                                        std::future::pending::<()>().await;
                                    }
                                } => {}
                            }
                            if let Some(cbs) = callbacks.mutate(|this| {
                                this.get_outbound_gateway
                                    .remove(&key)
                                    .map(|(_, handlers)| CallbackHandlers(handlers))
                                    .filter(|cb| !cb.0.is_empty())
                            }) {
                                if let Err(e) = cbs.call(vector![]).await {
                                    tracing::error!("Error in outbound gateway callback: {e}");
                                    tracing::debug!("{e:?}");
                                }
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

    pub(super) fn add_get_service_manifest(&self, package_id: PackageId, handler: CallbackHandler) {
        self.mutate(|this| {
            this.get_service_manifest
                .entry(package_id)
                .or_default()
                .push(handler)
        })
    }

    #[must_use]
    pub fn get_service_manifest(&self, package_id: &PackageId) -> Option<CallbackHandlers> {
        self.mutate(|this| {
            this.get_service_manifest
                .remove(package_id)
                .map(CallbackHandlers)
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

#[derive(Debug, Clone, Serialize, Deserialize, TS, Parser)]
#[ts(type = "{ only: number[] } | { except: number[] }")]
#[ts(export)]
pub struct ClearCallbacksParams {
    #[arg(long, conflicts_with = "except", help = "help.arg.only-callbacks")]
    pub only: Option<Vec<CallbackId>>,
    #[arg(long, conflicts_with = "only", help = "help.arg.except-callbacks")]
    pub except: Option<Vec<CallbackId>>,
}

pub(super) fn clear_callbacks(
    context: EffectContext,
    ClearCallbacksParams { only, except }: ClearCallbacksParams,
) -> Result<(), Error> {
    let context = context.deref()?;
    let only = only.map(|only| only.into_iter().collect::<BTreeSet<_>>());
    let except = except.map(|except| except.into_iter().collect::<BTreeSet<_>>());
    context.seed.persistent_container.state.send_modify(|s| {
        s.callbacks.retain(|cb| {
            only.as_ref().map_or(true, |only| !only.contains(cb))
                && except.as_ref().map_or(true, |except| except.contains(cb))
        })
    });
    context.seed.ctx.callbacks.gc();
    Ok(())
}
