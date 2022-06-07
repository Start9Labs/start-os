use std::collections::BTreeMap;
use std::net::IpAddr;

use avahi_sys::{
    self, avahi_client_errno, avahi_entry_group_add_service, avahi_entry_group_commit,
    avahi_entry_group_free, avahi_entry_group_reset, avahi_free, avahi_strerror, AvahiClient,
    AvahiEntryGroup,
};
use color_eyre::eyre::eyre;
use libc::c_void;
use tokio::process::Command;
use tokio::sync::Mutex;
use torut::onion::TorSecretKeyV3;

use super::interface::InterfaceId;
use crate::s9pk::manifest::PackageId;
use crate::util::Invoke;
use crate::Error;

pub async fn resolve_mdns(hostname: &str) -> Result<IpAddr, Error> {
    Ok(String::from_utf8(
        Command::new("avahi-resolve-host-name")
            .arg("-4")
            .arg(hostname)
            .invoke(crate::ErrorKind::Network)
            .await?,
    )?
    .split_once("\t")
    .ok_or_else(|| {
        Error::new(
            eyre!("Failed to resolve hostname: {}", hostname),
            crate::ErrorKind::Network,
        )
    })?
    .1
    .trim()
    .parse()?)
}

pub struct MdnsController(Mutex<MdnsControllerInner>);
impl MdnsController {
    pub fn init() -> Self {
        MdnsController(Mutex::new(MdnsControllerInner::init()))
    }
    pub async fn add<'a, I: IntoIterator<Item = (InterfaceId, TorSecretKeyV3)>>(
        &self,
        pkg_id: &PackageId,
        interfaces: I,
    ) {
        self.0.lock().await.add(pkg_id, interfaces)
    }
    pub async fn remove<I: IntoIterator<Item = InterfaceId>>(
        &self,
        pkg_id: &PackageId,
        interfaces: I,
    ) {
        self.0.lock().await.remove(pkg_id, interfaces)
    }
}

pub struct MdnsControllerInner {
    hostname: Vec<u8>,
    hostname_raw: *const libc::c_char,
    entry_group: *mut AvahiEntryGroup,
    services: BTreeMap<(PackageId, InterfaceId), TorSecretKeyV3>,
    _client_error: std::pin::Pin<Box<i32>>,
}
unsafe impl Send for MdnsControllerInner {}
unsafe impl Sync for MdnsControllerInner {}

impl MdnsControllerInner {
    fn load_services(&mut self) {
        unsafe {
            tracing::debug!("Loading services for mDNS");
            let mut res;
            let http_tcp_cstr = std::ffi::CString::new("_http._tcp")
                .expect("Could not cast _http._tcp to c string");
            res = avahi_entry_group_add_service(
                self.entry_group,
                avahi_sys::AVAHI_IF_UNSPEC,
                avahi_sys::AVAHI_PROTO_UNSPEC,
                avahi_sys::AvahiPublishFlags_AVAHI_PUBLISH_USE_MULTICAST,
                self.hostname_raw,
                http_tcp_cstr.as_ptr(),
                std::ptr::null(),
                std::ptr::null(),
                443,
                // below is a secret final argument that the type signature of this function does not tell you that it
                // needs. This is because the C lib function takes a variable number of final arguments indicating the
                // desired TXT records to add to this service entry. The way it decides when to stop taking arguments
                // from the stack and dereferencing them is when it finds a null pointer...because fuck you, that's why.
                // The consequence of this is that forgetting this last argument will cause segfaults or other undefined
                // behavior. Welcome back to the stone age motherfucker.
                std::ptr::null::<libc::c_char>(),
            );
            if res < avahi_sys::AVAHI_OK {
                log_str_error("add service to Avahi entry group", res);
                panic!("Failed to load Avahi services");
            }
            tracing::info!(
                "Published {:?}",
                std::ffi::CStr::from_ptr(self.hostname_raw)
            );
            for key in self.services.values() {
                let lan_address = key
                    .public()
                    .get_onion_address()
                    .get_address_without_dot_onion()
                    + ".local";
                tracing::debug!("Adding mdns CNAME entry for {}", &lan_address);
                let lan_address_ptr = std::ffi::CString::new(lan_address)
                    .expect("Could not cast lan address to c string");
                res = avahi_sys::avahi_entry_group_add_record(
                    self.entry_group,
                    avahi_sys::AVAHI_IF_UNSPEC,
                    avahi_sys::AVAHI_PROTO_UNSPEC,
                    avahi_sys::AvahiPublishFlags_AVAHI_PUBLISH_USE_MULTICAST
                        | avahi_sys::AvahiPublishFlags_AVAHI_PUBLISH_ALLOW_MULTIPLE,
                    lan_address_ptr.as_ptr(),
                    avahi_sys::AVAHI_DNS_CLASS_IN as u16,
                    avahi_sys::AVAHI_DNS_TYPE_CNAME as u16,
                    avahi_sys::AVAHI_DEFAULT_TTL,
                    self.hostname.as_ptr().cast(),
                    self.hostname.len(),
                );
                if res < avahi_sys::AVAHI_OK {
                    log_str_error("add CNAME record to Avahi entry group", res);
                    panic!("Failed to load Avahi services");
                }
                tracing::info!("Published {:?}", lan_address_ptr);
            }
        }
    }
    fn init() -> Self {
        unsafe {
            tracing::debug!("Initializing mDNS controller");
            let simple_poll = avahi_sys::avahi_simple_poll_new();
            let poll = avahi_sys::avahi_simple_poll_get(simple_poll);
            let mut box_err = Box::pin(0 as i32);
            let err_c: *mut i32 = box_err.as_mut().get_mut();
            let avahi_client = avahi_sys::avahi_client_new(
                poll,
                avahi_sys::AvahiClientFlags::AVAHI_CLIENT_NO_FAIL,
                Some(client_callback),
                std::ptr::null_mut(),
                err_c,
            );
            if avahi_client == std::ptr::null_mut::<AvahiClient>() {
                log_str_error("create Avahi client", *box_err);
                panic!("Failed to create Avahi Client");
            }
            let group = avahi_sys::avahi_entry_group_new(
                avahi_client,
                Some(entry_group_callback),
                std::ptr::null_mut(),
            );
            if group == std::ptr::null_mut() {
                log_str_error("create Avahi entry group", avahi_client_errno(avahi_client));
                panic!("Failed to create Avahi Entry Group");
            }
            let mut hostname_buf = vec![0];
            let hostname_raw = avahi_sys::avahi_client_get_host_name_fqdn(avahi_client);
            hostname_buf
                .extend_from_slice(std::ffi::CStr::from_ptr(hostname_raw).to_bytes_with_nul());
            let buflen = hostname_buf.len();
            debug_assert!(hostname_buf.ends_with(b".local\0"));
            debug_assert!(!hostname_buf[..(buflen - 7)].contains(&b'.'));
            // assume fixed length prefix on hostname due to local address
            hostname_buf[0] = (buflen - 8) as u8; // set the prefix length to len - 8 (leading byte, .local, nul) for the main address
            hostname_buf[buflen - 7] = 5; // set the prefix length to 5 for "local"

            let mut res = MdnsControllerInner {
                hostname: hostname_buf,
                hostname_raw,
                entry_group: group,
                services: BTreeMap::new(),
                _client_error: box_err,
            };
            res.load_services();
            let commit_err = avahi_entry_group_commit(res.entry_group);
            if commit_err < avahi_sys::AVAHI_OK {
                log_str_error("reset Avahi entry group", commit_err);
                panic!("Failed to load Avahi services: reset");
            }
            res
        }
    }
    fn sync(&mut self) {
        unsafe {
            let mut res;
            res = avahi_entry_group_reset(self.entry_group);
            if res < avahi_sys::AVAHI_OK {
                log_str_error("reset Avahi entry group", res);
                panic!("Failed to load Avahi services: reset");
            }
            self.load_services();
            res = avahi_entry_group_commit(self.entry_group);
            if res < avahi_sys::AVAHI_OK {
                log_str_error("commit Avahi entry group", res);
                panic!("Failed to load Avahi services: commit");
            }
        }
    }
    fn add<'a, I: IntoIterator<Item = (InterfaceId, TorSecretKeyV3)>>(
        &mut self,
        pkg_id: &PackageId,
        interfaces: I,
    ) {
        self.services.extend(
            interfaces
                .into_iter()
                .map(|(interface_id, key)| ((pkg_id.clone(), interface_id), key)),
        );
        self.sync();
    }
    fn remove<I: IntoIterator<Item = InterfaceId>>(&mut self, pkg_id: &PackageId, interfaces: I) {
        for interface_id in interfaces {
            self.services.remove(&(pkg_id.clone(), interface_id));
        }
        self.sync();
    }
}
impl Drop for MdnsControllerInner {
    fn drop(&mut self) {
        unsafe {
            avahi_free(self.hostname_raw as *mut c_void);
            avahi_entry_group_free(self.entry_group);
        }
    }
}

fn log_str_error(action: &str, e: i32) {
    unsafe {
        let e_str = avahi_strerror(e);
        tracing::error!(
            "Could not {}: {:?}",
            action,
            std::ffi::CStr::from_ptr(e_str)
        );
        avahi_free(e_str as *mut c_void);
    }
}

unsafe extern "C" fn entry_group_callback(
    _group: *mut avahi_sys::AvahiEntryGroup,
    state: avahi_sys::AvahiEntryGroupState,
    _userdata: *mut core::ffi::c_void,
) {
    match state {
        avahi_sys::AvahiEntryGroupState_AVAHI_ENTRY_GROUP_FAILURE => {
            tracing::warn!("AvahiCallback: EntryGroupState = AVAHI_ENTRY_GROUP_FAILURE");
        }
        avahi_sys::AvahiEntryGroupState_AVAHI_ENTRY_GROUP_COLLISION => {
            tracing::warn!("AvahiCallback: EntryGroupState = AVAHI_ENTRY_GROUP_COLLISION");
        }
        avahi_sys::AvahiEntryGroupState_AVAHI_ENTRY_GROUP_UNCOMMITED => {
            tracing::warn!("AvahiCallback: EntryGroupState = AVAHI_ENTRY_GROUP_UNCOMMITED");
        }
        avahi_sys::AvahiEntryGroupState_AVAHI_ENTRY_GROUP_ESTABLISHED => {
            tracing::warn!("AvahiCallback: EntryGroupState = AVAHI_ENTRY_GROUP_ESTABLISHED");
        }
        avahi_sys::AvahiEntryGroupState_AVAHI_ENTRY_GROUP_REGISTERING => {
            tracing::warn!("AvahiCallback: EntryGroupState = AVAHI_ENTRY_GROUP_REGISTERING");
        }
        other => {
            tracing::warn!("AvahiCallback: EntryGroupState = {}", other);
        }
    }
}

unsafe extern "C" fn client_callback(
    _group: *mut avahi_sys::AvahiClient,
    state: avahi_sys::AvahiClientState,
    _userdata: *mut core::ffi::c_void,
) {
    match state {
        avahi_sys::AvahiClientState_AVAHI_CLIENT_FAILURE => {
            tracing::warn!("AvahiCallback: ClientState = AVAHI_CLIENT_FAILURE");
        }
        avahi_sys::AvahiClientState_AVAHI_CLIENT_S_RUNNING => {
            tracing::warn!("AvahiCallback: ClientState = AVAHI_CLIENT_S_RUNNING");
        }
        avahi_sys::AvahiClientState_AVAHI_CLIENT_CONNECTING => {
            tracing::warn!("AvahiCallback: ClientState = AVAHI_CLIENT_CONNECTING");
        }
        avahi_sys::AvahiClientState_AVAHI_CLIENT_S_COLLISION => {
            tracing::warn!("AvahiCallback: ClientState = AVAHI_CLIENT_S_COLLISION");
        }
        avahi_sys::AvahiClientState_AVAHI_CLIENT_S_REGISTERING => {
            tracing::warn!("AvahiCallback: ClientState = AVAHI_CLIENT_S_REGISTERING");
        }
        other => {
            tracing::warn!("AvahiCallback: ClientState = {}", other);
        }
    }
}
