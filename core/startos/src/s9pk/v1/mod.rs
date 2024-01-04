use std::path::PathBuf;

use imbl::OrdMap;
use serde_json::Value;

use crate::context::CliContext;
use crate::prelude::*;
use crate::s9pk::v1::reader::S9pkReader;

pub mod builder;
pub mod docker;
pub mod git_hash;
pub mod header;
pub mod manifest;
pub mod reader;

pub const SIG_CONTEXT: &[u8] = b"s9pk";

pub async fn verify(_: CliContext, path: PathBuf) -> Result<(), Error> {
    let mut s9pk = S9pkReader::open(path, true).await?;
    // s9pk.validate().await?;
    todo!();

    Ok(())
}

fn enumerate_extra_keys(reference: &Value, candidate: &Value) -> Vec<String> {
    match (reference, candidate) {
        (Value::Object(m_r), Value::Object(m_c)) => {
            let om_r: OrdMap<String, Value> = m_r.clone().into_iter().collect();
            let om_c: OrdMap<String, Value> = m_c.clone().into_iter().collect();
            let common = om_r.clone().intersection(om_c.clone());
            let top_extra = common.clone().symmetric_difference(om_c.clone());
            let mut all_extra = top_extra
                .keys()
                .map(|s| format!(".{}", s))
                .collect::<Vec<String>>();
            for (k, v) in common {
                all_extra.extend(
                    enumerate_extra_keys(&v, om_c.get(&k).unwrap())
                        .into_iter()
                        .map(|s| format!(".{}{}", k, s)),
                )
            }
            all_extra
        }
        (_, Value::Object(m1)) => m1.clone().keys().map(|s| format!(".{}", s)).collect(),
        _ => Vec::new(),
    }
}

#[test]
fn test_enumerate_extra_keys() {
    use serde_json::json;
    let extras = enumerate_extra_keys(
        &json!({
            "test": 1,
            "test2": null,
        }),
        &json!({
            "test": 1,
            "test2": { "test3": null },
            "test4": null
        }),
    );
    println!("{:?}", extras)
}
