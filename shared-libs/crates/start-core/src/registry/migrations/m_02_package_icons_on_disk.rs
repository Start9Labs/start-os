use super::RegistryMigration;
use crate::prelude::*;

/// Drops inline package icons. `RegistryContext::init` writes them to the `IconStore` first.
pub struct PackageIconsOnDisk;
impl RegistryMigration for PackageIconsOnDisk {
    fn action(&self, db: &mut Value) -> Result<(), Error> {
        for (_, package) in db["index"]["package"]["packages"]
            .as_object_mut()
            .into_iter()
            .flat_map(|p| p.iter_mut())
        {
            for (_, info) in package["versions"]
                .as_object_mut()
                .into_iter()
                .flat_map(|v| v.iter_mut())
            {
                if let Some(info) = info.as_object_mut() {
                    info.remove("icon");
                }
                for (_, dep) in info["dependencyMetadata"]
                    .as_object_mut()
                    .into_iter()
                    .flat_map(|d| d.iter_mut())
                {
                    if let Some(dep) = dep.as_object_mut() {
                        dep.remove("icon");
                    }
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use imbl_value::json;

    use super::*;

    #[test]
    fn strips_inline_icons() {
        let mut db = json!({ "index": { "package": { "packages": { "pkg": { "versions": {
            "1.0.0:0": {
                "icon": "data:image/png;base64,aWNvbg==",
                "title": "Pkg",
                "dependencyMetadata": { "dep": { "icon": "data:image/png;base64,aWNvbg==", "optional": false } },
            },
        } } } } } });
        PackageIconsOnDisk.action(&mut db).unwrap();
        assert_eq!(
            db,
            json!({ "index": { "package": { "packages": { "pkg": { "versions": {
                "1.0.0:0": {
                    "title": "Pkg",
                    "dependencyMetadata": { "dep": { "optional": false } },
                },
            } } } } } })
        );
    }
}
