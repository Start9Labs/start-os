use std::collections::BTreeMap;

use imbl_value::InternedString;
use serde::{Deserialize, Serialize};

use crate::net::host::BindOptions;
use crate::net::keys::Key;

pub struct MultiHost {
    id: InternedString,
    key: Key,
    binds: BTreeMap<u16, BindOptions>,
}
