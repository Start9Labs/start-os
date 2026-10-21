use axum::body::Body;
use axum::response::Response;
use http::header::{CACHE_CONTROL, CONTENT_TYPE};

use crate::PackageId;
use crate::net::static_server::{bad_request, not_found, server_error};
use crate::prelude::*;
use crate::registry::context::RegistryContext;
use crate::registry::package::index::PackageIndex;
use crate::util::{DataUrl, VersionString};

/// Serves public package and dependency icons; private packages return 404.
pub async fn icon(
    ctx: &RegistryContext,
    id: &str,
    version: &str,
    dependency: Option<&str>,
) -> Response {
    let (Ok(id), Ok(version), Ok(dependency)) = (
        id.parse::<PackageId>(),
        version.parse::<VersionString>(),
        dependency.map(str::parse::<PackageId>).transpose(),
    ) else {
        return bad_request();
    };
    let peek = ctx.db.peek().await;
    let icon = package_icon(
        peek.as_index().as_package(),
        &id,
        &version,
        dependency.as_ref(),
    );
    match icon {
        Ok(Some(icon)) => Response::builder()
            .header(CONTENT_TYPE, &*icon.mime)
            .header(CACHE_CONTROL, "public, max-age=3600")
            .body(Body::from(icon.data.into_owned()))
            .unwrap(),
        Ok(None) => not_found(),
        Err(e) => server_error(e),
    }
}

fn package_icon(
    index: &Model<PackageIndex>,
    id: &PackageId,
    version: &VersionString,
    dependency: Option<&PackageId>,
) -> Result<Option<DataUrl<'static>>, Error> {
    let Some(info) = index
        .visible_package(id, None)?
        .and_then(|p| p.as_versions().as_idx(version))
    else {
        return Ok(None);
    };
    match dependency {
        None => info.as_icon().de(),
        Some(dep) => info
            .as_dependency_metadata()
            .as_idx(dep)
            .map_or(Ok(None), |d| d.as_icon().de()),
    }
}

#[cfg(test)]
mod tests {
    use imbl_value::json;

    use super::*;

    #[test]
    fn anonymous_icon_lookup_hides_private_packages() {
        let icon = "data:image/png;base64,aWNvbg==";
        let version = "1.0.0:0".parse().unwrap();
        let dependency = "dep".parse().unwrap();
        let mut index: Model<PackageIndex> = json!({
            "categories": {},
            "packages": {
                "pkg": {
                    "authorized": {},
                    "categories": [],
                    "private": false,
                    "readers": [],
                    "versions": { "1.0.0:0": {
                        "icon": icon,
                        "dependencyMetadata": { "dep": { "icon": icon } },
                    } },
                },
            },
        })
        .into();
        let id = "pkg".parse().unwrap();
        for dep in [None, Some(&dependency)] {
            let result = package_icon(&index, &id, &version, dep).unwrap().unwrap();
            assert_eq!(result.mime, "image/png");
            assert_eq!(&*result.data, b"icon");
        }
        index
            .as_packages_mut()
            .as_idx_mut(&id)
            .unwrap()
            .as_private_mut()
            .ser(&true)
            .unwrap();
        for dep in [None, Some(&dependency)] {
            assert!(package_icon(&index, &id, &version, dep).unwrap().is_none());
            assert!(
                package_icon(&index, &"missing".parse().unwrap(), &version, dep)
                    .unwrap()
                    .is_none()
            );
        }
    }
}
