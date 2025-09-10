use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use exver::{PreReleaseSegment, VersionRange};
use imbl_value::json;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{VersionT, v0_4_0_alpha_9};
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_4_0_alpha_10: exver::Version = exver::Version::new(
        [0, 4, 0],
        [PreReleaseSegment::String("alpha".into()), 10.into()]
    );
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_4_0_alpha_9::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_4_0_alpha_10.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    #[instrument]
    fn up(self, db: &mut Value, _: Self::PreUpRes) -> Result<Value, Error> {
        let default_gateway = db["public"]["serverInfo"]["network"]["networkInterfaces"]
            .as_object()
            .into_iter()
            .flatten()
            .find(|(_, i)| i["ipInfo"]["wanIp"].is_string())
            .map(|(g, _)| g.clone());
        let fix_host = |host: &mut Value| {
            let mut public = BTreeMap::new();
            let mut private = BTreeSet::new();
            for (domain, info) in host["domains"]
                .as_object_mut()
                .ok_or_else(|| {
                    Error::new(
                        eyre!("expected public.packageData[id].hosts[id].domains to be an object"),
                        ErrorKind::Database,
                    )
                })?
                .iter_mut()
            {
                let Some(info) = info.as_object_mut() else {
                    continue;
                };
                if info["public"].as_bool().unwrap_or_default()
                    && let Some(gateway) = &default_gateway
                {
                    info.insert(
                        "gateway".into(),
                        Value::String(Arc::new((&**gateway).to_owned())),
                    );
                    public.insert(domain.clone(), info.clone());
                } else {
                    private.insert(domain.clone());
                }
            }
            host["hostnameInfo"] = json!({});
            host["publicDomains"] = to_value(&public)?;
            host["privateDomains"] = to_value(&private)?;
            Ok::<_, Error>(())
        };
        for (_, package) in db["public"]["packageData"]
            .as_object_mut()
            .ok_or_else(|| {
                Error::new(
                    eyre!("expected public.packageData to be an object"),
                    ErrorKind::Database,
                )
            })?
            .iter_mut()
        {
            for (_, host) in package["hosts"]
                .as_object_mut()
                .ok_or_else(|| {
                    Error::new(
                        eyre!("expected public.packageData[id].hosts to be an object"),
                        ErrorKind::Database,
                    )
                })?
                .iter_mut()
            {
                fix_host(host)?;
            }
        }
        fix_host(&mut db["public"]["serverInfo"]["network"]["host"])?;
        let network = &mut db["public"]["serverInfo"]["network"];
        network["gateways"] = json!({});
        network["dns"] = json!({
            "dhcpServers": [],
        });
        db["private"]["authPubkeys"] = json!([]);

        Ok(Value::Null)
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
