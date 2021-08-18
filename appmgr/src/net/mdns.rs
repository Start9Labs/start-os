use std::collections::HashMap;

use avahi_sys::{
    self, avahi_entry_group_add_service, avahi_entry_group_commit, avahi_entry_group_free,
    avahi_entry_group_reset, avahi_free, AvahiEntryGroup,
};
use libc::c_void;
use patch_db::{DbHandle, OptionModel};
use tokio::sync::Mutex;
use torut::onion::TorSecretKeyV3;

use super::interface::InterfaceId;
use crate::s9pk::manifest::PackageId;
use crate::util::Apply;
use crate::Error;

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
    entry_group: *mut AvahiEntryGroup,
    services: HashMap<(PackageId, InterfaceId), TorSecretKeyV3>,
}
unsafe impl Send for MdnsControllerInner {}
unsafe impl Sync for MdnsControllerInner {}

impl MdnsControllerInner {
    fn load_services(&mut self) {
        unsafe {
            for key in self.services.values() {
                let lan_address = key
                    .public()
                    .get_onion_address()
                    .get_address_without_dot_onion()
                    + ".local";
                let lan_address_ptr = std::ffi::CString::new(lan_address)
                    .expect("Could not cast lan address to c string");
                let _ = avahi_sys::avahi_entry_group_add_record(
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
                log::info!("Published {:?}", lan_address_ptr);
            }
        }
    }
    fn init() -> Self {
        unsafe {
            // let app_list = crate::apps::list_info().await?;

            let simple_poll = avahi_sys::avahi_simple_poll_new();
            let poll = avahi_sys::avahi_simple_poll_get(simple_poll);
            let mut stack_err = 0;
            let err_c: *mut i32 = &mut stack_err;
            let avahi_client = avahi_sys::avahi_client_new(
                poll,
                avahi_sys::AvahiClientFlags::AVAHI_CLIENT_NO_FAIL,
                None,
                std::ptr::null_mut(),
                err_c,
            );
            let group =
                avahi_sys::avahi_entry_group_new(avahi_client, Some(noop), std::ptr::null_mut());
            let mut hostname_buf = vec![0];
            {
                let hostname_raw = avahi_sys::avahi_client_get_host_name_fqdn(avahi_client);
                hostname_buf
                    .extend_from_slice(std::ffi::CStr::from_ptr(hostname_raw).to_bytes_with_nul());
                let http_tcp_cstr = std::ffi::CString::new("_http._tcp")
                    .expect("Could not cast _http._tcp to c string");
                avahi_entry_group_add_service(
                    group,
                    avahi_sys::AVAHI_IF_UNSPEC,
                    avahi_sys::AVAHI_PROTO_UNSPEC,
                    avahi_sys::AvahiPublishFlags_AVAHI_PUBLISH_USE_MULTICAST,
                    hostname_raw,
                    http_tcp_cstr.as_ptr(),
                    std::ptr::null(),
                    std::ptr::null(),
                    443,
                );
                avahi_free(hostname_raw as *mut c_void);
            }
            let buflen = hostname_buf.len();
            debug_assert!(hostname_buf.ends_with(b".local\0"));
            debug_assert!(!hostname_buf[..(buflen - 7)].contains(&b'.'));
            // assume fixed length prefix on hostname due to local address
            hostname_buf[0] = (buflen - 8) as u8; // set the prefix length to len - 8 (leading byte, .local, nul) for the main address
            hostname_buf[buflen - 7] = 5; // set the prefix length to 5 for "local"

            avahi_entry_group_commit(group);

            MdnsControllerInner {
                hostname: hostname_buf,
                entry_group: group,
                services: HashMap::new(),
            }
        }
    }
    fn sync(&mut self) {
        unsafe {
            avahi_entry_group_reset(self.entry_group);
            self.load_services();
            avahi_entry_group_commit(self.entry_group);
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
            avahi_entry_group_free(self.entry_group);
        }
    }
}

unsafe extern "C" fn noop(
    _group: *mut avahi_sys::AvahiEntryGroup,
    _state: avahi_sys::AvahiEntryGroupState,
    _userdata: *mut core::ffi::c_void,
) {
}
