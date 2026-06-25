use imbl_value::InternedString;

use super::TunnelMigration;
use crate::prelude::*;

/// Backfill the `kind` field on existing WireGuard clients: a client that
/// already has both autoconfig flags on was a Server; everything else a Client.
/// Never touches the flag values.
pub struct WgClientKind;
impl TunnelMigration for WgClientKind {
    fn action(&self, db: &mut Value) -> Result<(), Error> {
        let kind_key = InternedString::intern("kind");
        let Some(subnets) = db["wg"]["subnets"].as_object_mut() else {
            return Ok(());
        };
        for (_, subnet) in subnets.iter_mut() {
            let Some(clients) = subnet["clients"].as_object_mut() else {
                continue;
            };
            for (_, client) in clients.iter_mut() {
                let Some(obj) = client.as_object_mut() else {
                    continue;
                };
                if obj.contains_key(&kind_key) {
                    continue;
                }
                let flag = |k: &str| {
                    obj.get(&InternedString::intern(k))
                        .and_then(|v| v.as_bool())
                        .unwrap_or(false)
                };
                let kind = if flag("allowDnsInjection") && flag("allowAutoPortForward") {
                    "server"
                } else {
                    "client"
                };
                obj.insert(kind_key.clone(), kind.into());
            }
        }
        Ok(())
    }
}
