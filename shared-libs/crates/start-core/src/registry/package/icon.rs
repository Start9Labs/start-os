use axum::body::Body;
use axum::extract::Request;
use axum::response::Response;
use http::header::{CACHE_CONTROL, CONTENT_TYPE};

use crate::PackageId;
use crate::middleware::auth::signature::{AUTH_SIG_HEADER, verify_request_signature};
use crate::net::static_server::{bad_request, not_found, server_error};
use crate::prelude::*;
use crate::registry::context::RegistryContext;
use crate::registry::package::index::{PackageIndex, Reader};
use crate::util::{DataUrl, VersionString};

/// Serves the icon of a package version, or of one of its dependencies as that version packs it.
/// A private package's icons require a signed request from a signer who may see it.
pub async fn icon(
    ctx: &RegistryContext,
    id: &str,
    version: &str,
    dependency: Option<&str>,
    mut request: Request,
) -> Response {
    let (Ok(id), Ok(version), Ok(dependency)) = (
        id.parse::<PackageId>(),
        version.parse::<VersionString>(),
        dependency.map(str::parse::<PackageId>).transpose(),
    ) else {
        return bad_request();
    };
    let signer = if request.headers().contains_key(AUTH_SIG_HEADER) {
        verify_request_signature(ctx, &mut request).await.ok()
    } else {
        None
    };
    let peek = ctx.db.peek().await;
    let icon = Reader::resolve(&peek, signer.as_ref()).and_then(|reader| {
        package_icon(
            peek.as_index().as_package(),
            reader.as_ref(),
            &id,
            &version,
            dependency.as_ref(),
        )
    });
    match icon {
        Ok(Some(PackageIcon { icon, private })) => Response::builder()
            .header(CONTENT_TYPE, &*icon.mime)
            .header(
                CACHE_CONTROL,
                if private {
                    "private, max-age=3600"
                } else {
                    "public, max-age=3600"
                },
            )
            .body(Body::from(icon.data.into_owned()))
            .unwrap(),
        Ok(None) => not_found(),
        Err(e) => server_error(e),
    }
}

struct PackageIcon {
    icon: DataUrl<'static>,
    private: bool,
}

/// None when the package, version or icon is absent or the package is hidden from the reader.
fn package_icon(
    index: &Model<PackageIndex>,
    reader: Option<&Reader>,
    id: &PackageId,
    version: &VersionString,
    dependency: Option<&PackageId>,
) -> Result<Option<PackageIcon>, Error> {
    let Some(package) = index.visible_package(id, reader)? else {
        return Ok(None);
    };
    let Some(info) = package.as_versions().as_idx(version) else {
        return Ok(None);
    };
    let icon = match dependency {
        None => info.as_icon().de()?,
        Some(dep) => info
            .as_dependency_metadata()
            .as_idx(dep)
            .map_or(Ok(None), |d| d.as_icon().de())?,
    };
    let private = package.as_private().de()?;
    Ok(icon.map(|icon| PackageIcon { icon, private }))
}

#[cfg(test)]
mod tests {
    use imbl_value::json;

    use super::*;
    use crate::rpc_continuations::Guid;

    #[test]
    fn private_icons_require_a_reader() {
        let icon = "data:image/png;base64,aWNvbg==";
        let reader = Guid::new();
        let version = "1.0.0:0".parse().unwrap();
        let dependency = "dep".parse().unwrap();
        let mut index: Model<PackageIndex> = json!({
            "categories": {},
            "packages": {
                "pkg": {
                    "authorized": {},
                    "categories": [],
                    "private": false,
                    "readers": [reader],
                    "versions": { "1.0.0:0": {
                        "icon": icon,
                        "dependencyMetadata": { "dep": { "icon": icon } },
                    } },
                },
            },
        })
        .into();
        let id = "pkg".parse().unwrap();
        let as_reader = Reader {
            signer: reader,
            admin: false,
        };
        let stranger = Reader {
            signer: Guid::new(),
            admin: false,
        };
        for dep in [None, Some(&dependency)] {
            let result = package_icon(&index, None, &id, &version, dep)
                .unwrap()
                .unwrap();
            assert_eq!(result.icon.mime, "image/png");
            assert_eq!(&*result.icon.data, b"icon");
            assert!(!result.private);
        }
        index
            .as_packages_mut()
            .as_idx_mut(&id)
            .unwrap()
            .as_private_mut()
            .ser(&true)
            .unwrap();
        for dep in [None, Some(&dependency)] {
            assert!(
                package_icon(&index, None, &id, &version, dep)
                    .unwrap()
                    .is_none()
            );
            assert!(
                package_icon(&index, Some(&stranger), &id, &version, dep)
                    .unwrap()
                    .is_none()
            );
            let result = package_icon(&index, Some(&as_reader), &id, &version, dep)
                .unwrap()
                .unwrap();
            assert!(result.private);
            assert!(
                package_icon(
                    &index,
                    Some(&as_reader),
                    &"missing".parse().unwrap(),
                    &version,
                    dep
                )
                .unwrap()
                .is_none()
            );
        }
    }
}
