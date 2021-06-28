use avahi_sys::{
    self, avahi_client_free, avahi_entry_group_commit, avahi_entry_group_free,
    avahi_entry_group_reset, avahi_free, AvahiClient, AvahiEntryGroup,
};
use libc::c_void;
use patch_db::{DbHandle, OptionModel};
use tokio::sync::RwLock;

use crate::util::Apply;
use crate::Error;

pub struct MdnsController(RwLock<MdnsControllerInner>);
impl MdnsController {
    pub async fn init<Db: DbHandle>(db: &mut Db) -> Result<Self, Error> {
        Ok(MdnsController(RwLock::new(
            MdnsControllerInner::init(db).await?,
        )))
    }
    pub async fn sync<Db: DbHandle>(&self, db: &mut Db) -> Result<(), Error> {
        self.0.write().await.sync(db).await
    }
}

pub struct MdnsControllerInner {
    hostname: Vec<u8>,
    entry_group: *mut AvahiEntryGroup,
}
unsafe impl Send for MdnsControllerInner {}
unsafe impl Sync for MdnsControllerInner {}

impl MdnsControllerInner {
    async fn load_services<Db: DbHandle>(&mut self, db: &mut Db) -> Result<(), Error> {
        unsafe {
            for app_id in crate::db::DatabaseModel::new()
                .package_data()
                .keys(db)
                .await?
            {
                let iface_model = if let Some(model) = crate::db::DatabaseModel::new()
                    .package_data()
                    .idx_model(&app_id)
                    .expect(db)
                    .await?
                    .installed()
                    .map(|i| i.interface_info().addresses())
                    .apply(OptionModel::from)
                    .check(db)
                    .await?
                {
                    model
                } else {
                    continue;
                };
                for iface in iface_model.keys(db).await? {
                    let lan_address = if let Some(addr) = iface_model
                        .clone()
                        .idx_model(&iface)
                        .expect(db)
                        .await?
                        .lan_address()
                        .get(db)
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
        Ok(())
    }
    async fn init<Db: DbHandle>(db: &mut Db) -> Result<Self, Error> {
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
                avahi_free(hostname_raw as *mut c_void);
            }
            let buflen = hostname_buf.len();
            debug_assert!(hostname_buf.ends_with(b".local\0"));
            debug_assert!(!hostname_buf[..(buflen - 7)].contains(&b'.'));
            // assume fixed length prefix on hostname due to local address
            hostname_buf[0] = (buflen - 8) as u8; // set the prefix length to len - 8 (leading byte, .local, nul) for the main address
            hostname_buf[buflen - 7] = 5; // set the prefix length to 5 for "local"

            let mut ctrl = MdnsControllerInner {
                hostname: hostname_buf,
                entry_group: group,
            };
            ctrl.load_services(db).await?;
            avahi_entry_group_commit(group);
            Ok(ctrl)
        }
    }
    async fn sync<Db: DbHandle>(&mut self, db: &mut Db) -> Result<(), Error> {
        unsafe {
            avahi_entry_group_reset(self.entry_group);
            self.load_services(db).await?;
            avahi_entry_group_commit(self.entry_group);
        }
        Ok(())
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
