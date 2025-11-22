use std::collections::BTreeSet;

use exver::{PreReleaseSegment, VersionRange};
use imbl_value::InternedString;

use super::v0_3_5::V0_3_0_COMPAT;
use super::{v0_4_0_alpha_11, VersionT};
use crate::net::tor::TorSecretKey;
use crate::prelude::*;

lazy_static::lazy_static! {
    static ref V0_4_0_alpha_12: exver::Version = exver::Version::new(
        [0, 4, 0],
        [PreReleaseSegment::String("alpha".into()), 12.into()]
    );
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Version;

impl VersionT for Version {
    type Previous = v0_4_0_alpha_11::Version;
    type PreUpRes = ();

    async fn pre_up(self) -> Result<Self::PreUpRes, Error> {
        Ok(())
    }
    fn semver(self) -> exver::Version {
        V0_4_0_alpha_12.clone()
    }
    fn compat(self) -> &'static VersionRange {
        &V0_3_0_COMPAT
    }
    #[instrument(skip_all)]
    fn up(self, db: &mut Value, _: Self::PreUpRes) -> Result<Value, Error> {
        let mut err = None;
        let onion_store = db["private"]["keyStore"]["onion"]
            .as_object_mut()
            .or_not_found("private.keyStore.onion")?;
        onion_store.retain(|o, v| match from_value::<TorSecretKey>(v.clone()) {
            Ok(k) => k.is_valid() && &InternedString::from_display(&k.onion_address()) == o,
            Err(e) => {
                err = Some(e);
                true
            }
        });
        if let Some(e) = err {
            return Err(e);
        }
        let allowed_addresses = onion_store.keys().cloned().collect::<BTreeSet<_>>();
        let fix_host = |host: &mut Value| {
            Ok::<_, Error>(
                host["onions"]
                    .as_array_mut()
                    .or_not_found("host.onions")?
                    .retain(|addr| {
                        addr.as_str()
                            .map(|s| allowed_addresses.contains(s))
                            .unwrap_or(false)
                    }),
            )
        };
        for (_, pde) in db["public"]["packageData"]
            .as_object_mut()
            .or_not_found("public.packageData")?
            .iter_mut()
        {
            for (_, host) in pde["hosts"]
                .as_object_mut()
                .or_not_found("public.packageData[].hosts")?
                .iter_mut()
            {
                fix_host(host)?;
            }
        }
        fix_host(&mut db["public"]["serverInfo"]["network"]["host"])?;

        if db["private"]["keyStore"]["localCerts"].is_null() {
            db["private"]["keyStore"]["localCerts"] =
                db["private"]["keyStore"]["local_certs"].clone();
        }

        Ok(Value::Null)
    }
    fn down(self, _db: &mut Value) -> Result<(), Error> {
        Ok(())
    }
}
