//! Shared server-side RFC 2136 (DNS UPDATE) handling, used by both StartTunnel
//! (in this crate) and StartWRT's `startwrt-ctrld` (which imports this crate).
//!
//! [`DnsInjector`] is an in-memory store of injected DNS records plus the
//! policy plug-ins each gateway supplies: an authorizer (does this source IP's
//! per-device "allow DNS injection" toggle permit it?) and an `on_change` hook
//! (persist the records — to PatchDb on the tunnel, to an addn-hosts file on
//! StartWRT). [`InjectingHandler`] wraps any forwarding `RequestHandler` with
//! it: a `Query` for an injected name is answered locally, an authorized
//! `Update` mutates the store, and everything else is forwarded unchanged.
//!
//! Authorization is by **source IP only** (no TSIG): a device is trusted to
//! inject or it isn't. Manual CRUD via [`DnsInjector::upsert`] /
//! [`DnsInjector::delete`] bypasses the authorizer (it's an admin action).

use std::collections::BTreeMap;
use std::net::IpAddr;
use std::sync::Arc;

use hickory_server::net::runtime::Time;
use hickory_server::proto::op::{Header, HeaderCounts, Metadata, OpCode, ResponseCode};
use hickory_server::proto::rr::{DNSClass, LowerName, Name, RData, Record, RecordType};
use hickory_server::server::{Request, RequestHandler, ResponseHandler, ResponseInfo};
use hickory_server::zone_handler::{Catalog, MessageResponseBuilder};

use crate::util::sync::SyncMutex;

/// One injected record. Kept Rust-only; each gateway maps it to its own
/// serializable view for the API/UI.
#[derive(Clone, Debug)]
pub struct InjectedRecord {
    pub name: Name,
    pub rtype: RecordType,
    pub rdata: RData,
    pub ttl: u32,
    /// The device IP that injected this (empty/unspecified for manual entries).
    pub source: IpAddr,
}

type Authorizer = Box<dyn Fn(IpAddr) -> bool + Send + Sync>;
type OnChange = Box<dyn Fn(Vec<InjectedRecord>) + Send + Sync>;

pub struct DnsInjector {
    records: SyncMutex<BTreeMap<LowerName, Vec<InjectedRecord>>>,
    authorize: Authorizer,
    on_change: OnChange,
}

impl DnsInjector {
    pub fn new(
        initial: Vec<InjectedRecord>,
        authorize: impl Fn(IpAddr) -> bool + Send + Sync + 'static,
        on_change: impl Fn(Vec<InjectedRecord>) + Send + Sync + 'static,
    ) -> Arc<Self> {
        let mut records: BTreeMap<LowerName, Vec<InjectedRecord>> = BTreeMap::new();
        for r in initial {
            records.entry(LowerName::from(&r.name)).or_default().push(r);
        }
        Arc::new(Self {
            records: SyncMutex::new(records),
            authorize: Box::new(authorize),
            on_change: Box::new(on_change),
        })
    }

    /// Every injected record, in a stable order.
    pub fn list(&self) -> Vec<InjectedRecord> {
        self.records
            .peek(|m| m.values().flatten().cloned().collect())
    }

    fn notify(&self) {
        (self.on_change)(self.list());
    }

    /// Manually add or replace a record (admin action; skips authorization).
    pub fn upsert(&self, record: InjectedRecord) {
        self.records.mutate(|m| {
            let v = m.entry(LowerName::from(&record.name)).or_default();
            v.retain(|r| !(r.rtype == record.rtype && r.rdata == record.rdata));
            v.push(record);
        });
        self.notify();
    }

    /// Manually delete records for a name (optionally a single type).
    pub fn delete(&self, name: &Name, rtype: Option<RecordType>) {
        self.records.mutate(|m| {
            let key = LowerName::from(name);
            match rtype {
                None => {
                    m.remove(&key);
                }
                Some(rt) => {
                    if let Some(v) = m.get_mut(&key) {
                        v.retain(|r| r.rtype != rt);
                        if v.is_empty() {
                            m.remove(&key);
                        }
                    }
                }
            }
        });
        self.notify();
    }

    fn lookup(&self, name: &LowerName, rtype: RecordType) -> Vec<Record> {
        self.records.peek(|m| {
            m.get(name)
                .into_iter()
                .flatten()
                .filter(|r| rtype == RecordType::ANY || r.rtype == rtype)
                .map(|r| Record::from_rdata(r.name.clone(), r.ttl, r.rdata.clone()))
                .collect()
        })
    }

    /// Apply an UPDATE's records (RFC 2136 §2.5) from `src`, after authorizing.
    fn apply_update(&self, src: IpAddr, updates: &[Record]) -> ResponseCode {
        if !(self.authorize)(src) {
            return ResponseCode::Refused;
        }
        self.records.mutate(|m| {
            for rec in updates {
                let key = LowerName::from(&rec.name);
                match rec.dns_class {
                    // Add to an RRset.
                    DNSClass::IN => {
                        let record = InjectedRecord {
                            name: rec.name.clone(),
                            rtype: rec.record_type(),
                            rdata: rec.data.clone(),
                            ttl: rec.ttl,
                            source: src,
                        };
                        let v = m.entry(key).or_default();
                        v.retain(|r| {
                            !(r.rtype == record.rtype && r.rdata == record.rdata)
                        });
                        v.push(record);
                    }
                    // Delete an RRset (a whole type, or every type for the name).
                    DNSClass::ANY => {
                        if rec.record_type() == RecordType::ANY {
                            m.remove(&key);
                        } else if let Some(v) = m.get_mut(&key) {
                            v.retain(|r| r.rtype != rec.record_type());
                            if v.is_empty() {
                                m.remove(&key);
                            }
                        }
                    }
                    // Delete one specific record.
                    DNSClass::NONE => {
                        if let Some(v) = m.get_mut(&key) {
                            let rdata = rec.data.clone();
                            v.retain(|r| r.rdata != rdata);
                            if v.is_empty() {
                                m.remove(&key);
                            }
                        }
                    }
                    _ => {}
                }
            }
        });
        self.notify();
        ResponseCode::NoError
    }
}

/// Wraps a forwarding `Catalog` with injected-record answering and authorized
/// RFC 2136 UPDATE handling. Both gateways forward non-injected queries through
/// a `Catalog` (a `ForwardZoneHandler` pointed at their upstream / dnsmasq).
pub struct InjectingHandler {
    injector: Arc<DnsInjector>,
    forwarder: Catalog,
}

impl InjectingHandler {
    pub fn new(injector: Arc<DnsInjector>, forwarder: Catalog) -> Self {
        Self {
            injector,
            forwarder,
        }
    }
}

fn header_with_code(request: &Request, code: ResponseCode) -> Metadata {
    let mut header = Metadata::response_from_request(&request.metadata);
    header.recursion_available = true;
    header.response_code = code;
    header
}

fn fallback_info(header: Metadata) -> ResponseInfo {
    Header {
        metadata: header,
        counts: HeaderCounts::default(),
    }
    .into()
}

#[async_trait::async_trait]
impl RequestHandler for InjectingHandler {
    async fn handle_request<R: ResponseHandler, T: Time>(
        &self,
        request: &Request,
        mut response_handle: R,
    ) -> ResponseInfo {
        match request.metadata.op_code {
            OpCode::Update => {
                // The update RRs live in the authority section; re-decode the
                // raw message to read them (MessageRequest only exposes the
                // query/zone section).
                let code = match hickory_server::proto::op::Message::from_vec(request.as_slice()) {
                    Ok(msg) => self.injector.apply_update(request.src().ip(), &msg.authorities),
                    Err(_) => ResponseCode::FormErr,
                };
                let header = header_with_code(request, code);
                response_handle
                    .send_response(
                        MessageResponseBuilder::from_message_request(&*request)
                            .build(header, [], [], [], []),
                    )
                    .await
                    .unwrap_or_else(|_| fallback_info(header))
            }
            OpCode::Query => {
                let answers = request
                    .request_info()
                    .ok()
                    .map(|req| self.injector.lookup(req.query.name(), req.query.query_type()))
                    .unwrap_or_default();
                if answers.is_empty() {
                    return self.forwarder.handle_request::<R, T>(request, response_handle).await;
                }
                let header = header_with_code(request, ResponseCode::NoError);
                response_handle
                    .send_response(
                        MessageResponseBuilder::from_message_request(&*request)
                            .build(header, &answers, [], [], []),
                    )
                    .await
                    .unwrap_or_else(|_| fallback_info(header))
            }
            _ => self.forwarder.handle_request::<R, T>(request, response_handle).await,
        }
    }
}
