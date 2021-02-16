use crate::Error;
use avahi_sys;
use futures::future::pending;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct AppId {
    un_app_id: String,
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
    ctrlc::set_handler(move || {}).expect("Error setting signal handler");
    pending().await
}
