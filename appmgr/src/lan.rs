use crate::Error;
use avahi_sys;
use futures::future::pending;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct AppId {
    pub un_app_id: String,
}

pub async fn enable_lan(app_id: &AppId) -> Result<(), Error> {
    let tor_address = crate::apps::info(&app_id.un_app_id).await?.tor_address;
    let lan_address = tor_address
        .as_ref()
        .ok_or_else(|| {
            failure::format_err!("Service {} does not have Tor Address", app_id.un_app_id)
        })?
        .strip_suffix(".onion")
        .ok_or_else(|| failure::format_err!("Invalid Tor Address: {:?}", tor_address))?
        .to_owned()
        + ".local";
    println!("{}", lan_address);
    let lan_address_ptr =
        std::ffi::CString::new(lan_address).expect("Could not cast lan address to c string");
    unsafe {
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
        let hostname = avahi_sys::avahi_client_get_host_name_fqdn(avahi_client);
        let hostname_len = libc::strlen(hostname);
        let _ = avahi_sys::avahi_entry_group_add_record(
            group,
            avahi_sys::AVAHI_IF_UNSPEC,
            avahi_sys::AVAHI_PROTO_UNSPEC,
            avahi_sys::AvahiPublishFlags_AVAHI_PUBLISH_USE_MULTICAST
                | avahi_sys::AvahiPublishFlags_AVAHI_PUBLISH_ALLOW_MULTIPLE,
            lan_address_ptr.as_ptr(),
            avahi_sys::AVAHI_DNS_CLASS_IN as u16,
            avahi_sys::AVAHI_DNS_TYPE_CNAME as u16,
            avahi_sys::AVAHI_DEFAULT_TTL,
            hostname.cast(),
            hostname_len,
        );
        avahi_sys::avahi_entry_group_commit(group);
        ctrlc::set_handler(move || {
            // please the borrow checker with the below semantics
            // avahi_sys::avahi_entry_group_free(group);
            // avahi_sys::avahi_client_free(avahi_client);
            // drop(Box::from_raw(err_c));
            std::process::exit(0);
        })
        .expect("Error setting signal handler");
    }
    pending().await
}

unsafe extern "C" fn noop(
    _group: *mut avahi_sys::AvahiEntryGroup,
    _state: avahi_sys::AvahiEntryGroupState,
    _userdata: *mut core::ffi::c_void,
) {
}
