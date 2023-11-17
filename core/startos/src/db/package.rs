use models::{PackageId, Version};

use crate::prelude::*;

pub fn get_packages(db: Peeked) -> Result<Vec<(PackageId, Version)>, Error> {
    Ok(db
        .as_package_data()
        .keys()?
        .into_iter()
        .flat_map(|package_id| {
            let version = db
                .as_package_data()
                .as_idx(&package_id)?
                .as_manifest()
                .as_version()
                .de()
                .ok()?;
            Some((package_id, version))
        })
        .collect())
}
