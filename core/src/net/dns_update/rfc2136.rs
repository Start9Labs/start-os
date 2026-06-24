//! Shared server-side RFC 2136 (DNS UPDATE) handling, used by both StartTunnel
//! (in this crate) and StartWRT's `startwrt-ctrld` (which imports this crate).
//!
//! [`DnsInjector`] is an in-memory store of injected DNS records plus per-gateway
//! policy plug-ins: an authorizer (does this source IP's "allow DNS injection"
//! toggle permit it?) and an `on_change` hook (persist the records — to PatchDb
//! on the tunnel, an addn-hosts file on StartWRT). [`InjectingHandler`] wraps a
//! forwarding `RequestHandler`: an injected-name `Query` is answered locally, an
//! authorized `Update` mutates the store, everything else is forwarded unchanged.
//!
//! Authorization is by **source IP only** (no TSIG). Manual CRUD via
//! [`DnsInjector::upsert`] / [`DnsInjector::delete`] bypasses the authorizer.

use std::collections::BTreeMap;
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};
use std::sync::Arc;

use hickory_server::net::runtime::Time;
use hickory_server::proto::op::{Header, HeaderCounts, Metadata, OpCode, ResponseCode};
use hickory_server::proto::rr::rdata::{CNAME, TXT};
use hickory_server::proto::rr::{DNSClass, LowerName, Name, RData, Record, RecordType};

use crate::prelude::*;
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
    /// Device IP that injected this; unspecified for manual entries.
    pub source: IpAddr,
}

impl InjectedRecord {
    /// Render to the text form gateways persist and show; `source` is `None`
    /// for a manual record.
    pub fn to_parts(&self) -> (String, String, String, u32, Option<IpAddr>) {
        let source = if self.source.is_unspecified() {
            None
        } else {
            Some(self.source)
        };
        (
            self.name.to_utf8().trim_end_matches('.').to_string(),
            self.rtype.to_string(),
            self.rdata.to_string(),
            self.ttl,
            source,
        )
    }

    /// Parse the text form back into a record; pass `Ipv4Addr::UNSPECIFIED`
    /// as `source` for a manual record.
    pub fn from_parts(
        name: &str,
        rtype: &str,
        value: &str,
        ttl: u32,
        source: IpAddr,
    ) -> Result<Self, Error> {
        let mut n = Name::from_utf8(name).with_kind(ErrorKind::ParseUrl)?;
        n.set_fqdn(true);
        let (rtype, rdata) = parse_rdata(rtype, value)?;
        Ok(Self {
            name: n,
            rtype,
            rdata,
            ttl,
            source,
        })
    }
}

fn parse_rdata(rtype: &str, value: &str) -> Result<(RecordType, RData), Error> {
    let invalid = |what: &str| Error::new(eyre!("invalid {what}: {value}"), ErrorKind::InvalidRequest);
    Ok(match rtype.to_ascii_uppercase().as_str() {
        "A" => (
            RecordType::A,
            RData::A(value.parse::<Ipv4Addr>().map_err(|_| invalid("A record"))?.into()),
        ),
        "AAAA" => (
            RecordType::AAAA,
            RData::AAAA(value.parse::<Ipv6Addr>().map_err(|_| invalid("AAAA record"))?.into()),
        ),
        "CNAME" => (
            RecordType::CNAME,
            RData::CNAME(CNAME(Name::from_utf8(value).with_kind(ErrorKind::ParseUrl)?)),
        ),
        "TXT" => (RecordType::TXT, RData::TXT(TXT::new(vec![value.to_string()]))),
        other => {
            return Err(Error::new(
                eyre!("unsupported DNS record type: {other}"),
                ErrorKind::InvalidRequest,
            ));
        }
    })
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

    /// Every injected record, in stable order.
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

    /// Whether any record (of any type) exists for `name`; distinguishes an
    /// authoritative NODATA from a name we don't serve.
    fn contains_name(&self, name: &LowerName) -> bool {
        self.records.peek(|m| m.contains_key(name))
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
/// RFC 2136 UPDATE handling. Non-injected queries fall through to the `Catalog`
/// (a `ForwardZoneHandler` pointed at upstream / dnsmasq).
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
                // Re-decode the raw message: MessageRequest hides the authority
                // section where the update RRs live.
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
                let req = request.request_info().ok();
                let answers = req
                    .as_ref()
                    .map(|req| self.injector.lookup(req.query.name(), req.query.query_type()))
                    .unwrap_or_default();
                // Authoritative for any injected name: serve records or NODATA.
                // Forwarding a held name's missing-type query lets upstream NXDOMAIN
                // poison it (RFC 8020) — e.g. an AAAA probe for an A-only domain.
                let known = req
                    .as_ref()
                    .is_some_and(|req| self.injector.contains_name(req.query.name()));
                if answers.is_empty() && !known {
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


#[cfg(test)]
mod tests {
    use super::*;

    fn injector() -> Arc<DnsInjector> {
        DnsInjector::new(Vec::new(), |_| true, |_| {})
    }

    fn fqdn(s: &str) -> LowerName {
        let mut n = Name::from_utf8(s).unwrap();
        n.set_fqdn(true);
        LowerName::from(&n)
    }

    /// An A-only injected name must answer NODATA (not forward) for an AAAA
    /// query: the name is held, so `contains_name` is true even though the
    /// queried type is absent. Forwarding would let upstream NXDOMAIN poison it.
    // tokio runtime required: DnsInjector's SyncMutex spawns a lock watchdog.
    #[tokio::test]
    async fn held_name_is_nodata_not_forwarded() {
        let inj = injector();
        inj.upsert(
            InjectedRecord::from_parts(
                "scrunge.goop",
                "A",
                "10.59.0.2",
                300,
                IpAddr::V4(Ipv4Addr::UNSPECIFIED),
            )
            .unwrap(),
        );
        let q = fqdn("scrunge.goop");
        assert_eq!(inj.lookup(&q, RecordType::A).len(), 1, "A query resolves");
        assert!(inj.lookup(&q, RecordType::AAAA).is_empty(), "no AAAA record");
        assert!(inj.contains_name(&q), "held name -> handler answers NODATA");
        assert!(!inj.contains_name(&fqdn("nope.goop")), "unknown name -> handler forwards");
    }
}
