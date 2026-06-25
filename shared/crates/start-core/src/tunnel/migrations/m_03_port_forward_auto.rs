use imbl_value::InternedString;

use super::TunnelMigration;
use crate::prelude::*;

/// Backfill the `auto` field on existing forwards/SNI routes: an entry labeled
/// `PCP`/`UPnP` was gateway-created, everything else user-added.
pub struct PortForwardAuto;
impl TunnelMigration for PortForwardAuto {
    fn action(&self, db: &mut Value) -> Result<(), Error> {
        let auto_key = InternedString::intern("auto");
        let label_key = InternedString::intern("label");
        let is_auto = |v: Option<&Value>| {
            v.and_then(|v| v.as_str())
                .is_some_and(|l| l == "PCP" || l == "UPnP")
        };
        let Some(forwards) = db["portForwards"].as_object_mut() else {
            return Ok(());
        };
        for (_, value) in forwards.iter_mut() {
            let Some(obj) = value.as_object_mut() else {
                continue;
            };
            match obj
                .get(&InternedString::intern("kind"))
                .and_then(|v| v.as_str())
            {
                Some("dnat") => {
                    if !obj.contains_key(&auto_key) {
                        let auto = is_auto(obj.get(&label_key));
                        obj.insert(auto_key.clone(), auto.into());
                    }
                }
                Some("sni") => {
                    let Some(routes) = obj
                        .get_mut(&InternedString::intern("routes"))
                        .and_then(|v| v.as_object_mut())
                    else {
                        continue;
                    };
                    for (_, route) in routes.iter_mut() {
                        if let Some(r) = route.as_object_mut() {
                            if !r.contains_key(&auto_key) {
                                let auto = is_auto(r.get(&label_key));
                                r.insert(auto_key.clone(), auto.into());
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }
}
