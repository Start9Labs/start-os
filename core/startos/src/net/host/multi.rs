use imbl_value::InternedString;

use crate::net::keys::Key;

pub struct MultiHost {
    id: InternedString,
    key: Key,
}
