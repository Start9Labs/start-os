use avahi_sys;
use futures::future::pending;
use patch_db::{DbHandle, OptionModel};

use crate::db::model::{InterfaceAddressesModel, InterfaceInfoModel};
use crate::util::Apply;
use crate::Error;

pub async fn enable_lan<Db: DbHandle>(mut db: Db) -> Result<LanHandle, Error> {
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
        let hostname_raw = avahi_sys::avahi_client_get_host_name_fqdn(avahi_client);
        let hostname_bytes = std::ffi::CStr::from_ptr(hostname_raw).to_bytes_with_nul();
        const HOSTNAME_LEN: usize = 1 + 15 + 1 + 5; // leading byte, main address, dot, "local"
        debug_assert_eq!(hostname_bytes.len(), HOSTNAME_LEN);
        let mut hostname_buf = [0; HOSTNAME_LEN + 1];
        hostname_buf[1..].copy_from_slice(hostname_bytes);
        // assume fixed length prefix on hostname due to local address
        hostname_buf[0] = 15; // set the prefix length to 15 for the main address
        hostname_buf[16] = 5; // set the prefix length to 5 for "local"

        for app_id in crate::db::DatabaseModel::new()
            .package_data()
            .keys(&mut db)
            .await?
        {
            let iface_model = if let Some(model) = crate::db::DatabaseModel::new()
                .package_data()
                .idx_model(&app_id)
                .expect(&mut db)
                .await?
                .installed()
                .map(|i| i.interface_info().addresses())
                .apply(OptionModel::from)
                .check(&mut db)
                .await?
            {
                model
            } else {
                continue;
            };
            for iface in iface_model.keys(&mut db).await? {
                let lan_address = if let Some(addr) = iface_model
                    .clone()
                    .idx_model(&iface)
                    .expect(&mut db)
                    .await?
                    .lan_address()
                    .get(&mut db)
                    .await?
                    .to_owned()
                {
                    addr
                } else {
                    continue;
                };
                let lan_address_ptr = std::ffi::CString::new(lan_address)
                    .expect("Could not cast lan address to c string");
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
                    hostname_buf.as_ptr().cast(),
                    hostname_buf.len(),
                );
                log::info!("Published {:?}", lan_address_ptr);
            }
        }
        avahi_sys::avahi_entry_group_commit(group);
        Ok(LanHandle(group))
    }
}

unsafe extern "C" fn noop(
    _group: *mut avahi_sys::AvahiEntryGroup,
    _state: avahi_sys::AvahiEntryGroupState,
    _userdata: *mut core::ffi::c_void,
) {
}

pub struct LanHandle(*mut avahi_sys::AvahiEntryGroup);
impl Drop for LanHandle {
    fn drop(&mut self) {
        unsafe {
            avahi_sys::avahi_entry_group_reset(self.0);
            avahi_sys::avahi_entry_group_free(self.0);
        }
    }
}
