use crate::net::host::multi::MultiHost;

pub mod multi;

pub enum Host {
    Multi(MultiHost),
    // Single(SingleHost),
    // Static(StaticHost),
}
