use async_trait::async_trait;
use emver::VersionRange;
use serde_json::{json, Value};

use super::v0_3_0::V0_3_0_COMPAT;
use super::*;
use crate::{COMMUNITY_MARKETPLACE, DEFAULT_MARKETPLACE};

const V0_3_3: emver::Version = emver::Version::new(0, 3, 3, 0);
#[derive(Clone, Debug)]
pub struct Version;
#[async_trait]
impl VersionT for Version {
    type Previous = v0_3_2_1::Version;
    fn new() -> Self {
        Version
    }
    fn semver(&self) -> emver::Version {
        V0_3_3
    }
    fn compat(&self) -> &'static VersionRange {
        &*V0_3_0_COMPAT
    }
    async fn up<Db: DbHandle>(&self, db: &mut Db) -> Result<(), Error> {
        let mut ui = crate::db::DatabaseModel::new().ui().get_mut(db).await?;

        if let Some(Value::String(selected_url)) =
            ui["marketplace"]
                .get("selected-id")
                .and_then(|selected_id| {
                    if let Value::String(selected_id) = selected_id {
                        return Some(ui["marketplace"]["known-hosts"].get(&selected_id)?);
                    }
                    None
                })
        {
            ui["marketplace"]["selected-url"] = json!(selected_url);
        }
        if let Value::Object(ref mut obj) = *ui {
            obj.remove("pkg-order");
            obj.remove("auto-check-updates");
        }
        let known_hosts = ui["marketplace"]["known-hosts"].take();
        if let Value::Object(known_hosts) = known_hosts {
            for (_id, value) in known_hosts {
                if let (Value::String(name), Value::String(url)) = (&value["name"], &value["url"]) {
                    ui["marketplace"]["known-hosts"][name] = json!(url);
                }
            }
        }
        if let Some(Value::Object(ref mut obj)) = ui.get_mut("marketplace") {
            obj.remove("selected-id");
        }
        if ui["marketplace"]["selected-url"].is_null() {
            ui["marketplace"]["selected-url"] = json!(MarketPlaceUrls::Default.url());
        }
        ui.save(db).await?;
        Ok(())
    }
    async fn down<Db: DbHandle>(&self, _db: &mut Db) -> Result<(), Error> {
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MarketPlaceUrls {
    Default,
    Community,
}

impl MarketPlaceUrls {
    pub fn url(&self) -> String {
        let url_string = match self {
            MarketPlaceUrls::Default => DEFAULT_MARKETPLACE,
            MarketPlaceUrls::Community => COMMUNITY_MARKETPLACE,
        };
        format!("{url_string}/")
    }
}

#[test]
fn test_that_ui_includes_url() {
    let ui: Value =
        serde_json::from_str(include_str!("../../../frontend/patchdb-ui-seed.json")).unwrap();
    for market_place in [MarketPlaceUrls::Default, MarketPlaceUrls::Community] {
        let url = market_place.url();
        assert!(
            !ui["marketplace"]["known-hosts"][&url].is_null(),
            "Should have a market place for {url}"
        );
    }
}
