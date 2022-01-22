use std::collections::HashMap;

use emver::Version;
use serde::{Deserialize, Serialize};
use serde_with::{serde_as, DisplayFromStr};

#[serde_as]
#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct LatestInformation {
    release_notes: HashMap<String, String>,
    headline: String,
    #[serde_as(as = "DisplayFromStr")]
    pub version: Version,
}

/// Captured from https://beta-registry-0-3.start9labs.com/eos/latest 2021-09-24
#[test]
fn latest_information_from_server() {
    let data_from_server = r#"{"release-notes":{"0.3.0":"This major software release encapsulates the optimal performance, security, and management enhancments to the EmbassyOS experience."},"headline":"Major EmbassyOS release","version":"0.3.0"}"#;
    let latest_information: LatestInformation = serde_json::from_str(data_from_server).unwrap();
    assert_eq!(latest_information.version.minor(), 3);
}
