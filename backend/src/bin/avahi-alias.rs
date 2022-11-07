use avahi_sys::{
    self, avahi_client_errno, avahi_entry_group_add_service, avahi_entry_group_commit,
    avahi_strerror, AvahiClient,
};

fn log_str_error(action: &str, e: i32) {
    unsafe {
        let e_str = avahi_strerror(e);
        eprintln!(
            "Could not {}: {:?}",
            action,
            std::ffi::CStr::from_ptr(e_str)
        );
    }
}

fn main() {
    let aliases: Vec<_> = std::env::args().skip(1).collect();
    unsafe {
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
        hostname_buf.extend_from_slice(std::ffi::CStr::from_ptr(hostname_raw).to_bytes_with_nul());
        let buflen = hostname_buf.len();
        debug_assert!(hostname_buf.ends_with(b".local\0"));
        debug_assert!(!hostname_buf[..(buflen - 7)].contains(&b'.'));
        // assume fixed length prefix on hostname due to local address
        hostname_buf[0] = (buflen - 8) as u8; // set the prefix length to len - 8 (leading byte, .local, nul) for the main address
        hostname_buf[buflen - 7] = 5; // set the prefix length to 5 for "local"
        let mut res;
        let http_tcp_cstr =
            std::ffi::CString::new("_http._tcp").expect("Could not cast _http._tcp to c string");
        res = avahi_entry_group_add_service(
            group,
            avahi_sys::AVAHI_IF_UNSPEC,
            avahi_sys::AVAHI_PROTO_UNSPEC,
            avahi_sys::AvahiPublishFlags_AVAHI_PUBLISH_USE_MULTICAST,
            hostname_raw,
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
        eprintln!("Published {:?}", std::ffi::CStr::from_ptr(hostname_raw));
        for alias in aliases {
            let lan_address = alias + ".local";
            let lan_address_ptr = std::ffi::CString::new(lan_address)
                .expect("Could not cast lan address to c string");
            res = avahi_sys::avahi_entry_group_add_record(
                group,
                avahi_sys::AVAHI_IF_UNSPEC,
                avahi_sys::AVAHI_PROTO_UNSPEC,
                avahi_sys::AvahiPublishFlags_AVAHI_PUBLISH_USE_MULTICAST
                    | avahi_sys::AvahiPublishFlags_AVAHI_PUBLISH_ALLOW_MULTIPLE,
                lan_address_ptr.as_ptr(),
                avahi_sys::AVAHI_DNS_CLASS_IN as u16,
                avahi_sys::AVAHI_DNS_TYPE_CNAME as u16,
                avahi_sys::AVAHI_DEFAULT_TTL,
                hostname_buf.as_ptr().cast(),
                hostname_buf.len(),
            );
            if res < avahi_sys::AVAHI_OK {
                log_str_error("add CNAME record to Avahi entry group", res);
                panic!("Failed to load Avahi services");
            }
            eprintln!("Published {:?}", lan_address_ptr);
        }
        let commit_err = avahi_entry_group_commit(group);
        if commit_err < avahi_sys::AVAHI_OK {
            log_str_error("reset Avahi entry group", commit_err);
            panic!("Failed to load Avahi services: reset");
        }
    }
    std::thread::park()
}

unsafe extern "C" fn entry_group_callback(
    _group: *mut avahi_sys::AvahiEntryGroup,
    state: avahi_sys::AvahiEntryGroupState,
    _userdata: *mut core::ffi::c_void,
) {
    match state {
        avahi_sys::AvahiEntryGroupState_AVAHI_ENTRY_GROUP_FAILURE => {
            eprintln!("AvahiCallback: EntryGroupState = AVAHI_ENTRY_GROUP_FAILURE");
        }
        avahi_sys::AvahiEntryGroupState_AVAHI_ENTRY_GROUP_COLLISION => {
            eprintln!("AvahiCallback: EntryGroupState = AVAHI_ENTRY_GROUP_COLLISION");
        }
        avahi_sys::AvahiEntryGroupState_AVAHI_ENTRY_GROUP_UNCOMMITED => {
            eprintln!("AvahiCallback: EntryGroupState = AVAHI_ENTRY_GROUP_UNCOMMITED");
        }
        avahi_sys::AvahiEntryGroupState_AVAHI_ENTRY_GROUP_ESTABLISHED => {
            eprintln!("AvahiCallback: EntryGroupState = AVAHI_ENTRY_GROUP_ESTABLISHED");
        }
        avahi_sys::AvahiEntryGroupState_AVAHI_ENTRY_GROUP_REGISTERING => {
            eprintln!("AvahiCallback: EntryGroupState = AVAHI_ENTRY_GROUP_REGISTERING");
        }
        other => {
            eprintln!("AvahiCallback: EntryGroupState = {}", other);
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
            eprintln!("AvahiCallback: ClientState = AVAHI_CLIENT_FAILURE");
        }
        avahi_sys::AvahiClientState_AVAHI_CLIENT_S_RUNNING => {
            eprintln!("AvahiCallback: ClientState = AVAHI_CLIENT_S_RUNNING");
        }
        avahi_sys::AvahiClientState_AVAHI_CLIENT_CONNECTING => {
            eprintln!("AvahiCallback: ClientState = AVAHI_CLIENT_CONNECTING");
        }
        avahi_sys::AvahiClientState_AVAHI_CLIENT_S_COLLISION => {
            eprintln!("AvahiCallback: ClientState = AVAHI_CLIENT_S_COLLISION");
        }
        avahi_sys::AvahiClientState_AVAHI_CLIENT_S_REGISTERING => {
            eprintln!("AvahiCallback: ClientState = AVAHI_CLIENT_S_REGISTERING");
        }
        other => {
            eprintln!("AvahiCallback: ClientState = {}", other);
        }
    }
}
