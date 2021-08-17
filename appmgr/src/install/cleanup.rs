use crate::db::model::InstalledPackageDataEntry;
use crate::s9pk::manifest::{Manifest, PackageId};
use crate::Error;

pub async fn cleanup(info: Result<InstalledPackageDataEntry, &Manifest>) -> Result<(), Error> {
    Ok(()) // TODO
}
