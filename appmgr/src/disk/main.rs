use tokio::process::Command;

use crate::util::Invoke;
use crate::Error;

pub async fn importable() -> Result<bool, Error> {
    todo!()
}

pub async fn create(disks: &[&str]) -> Result<(), Error> {
    todo!()
}

pub async fn load(password: &str) -> Result<(), Error> {
    todo!()
}

pub async fn create_pool(disks: &[&str]) -> Result<(), Error> {
    Command::new("zpool")
        .arg("create")
        .arg("embassy-data")
        .args(disks)
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Ok(())
}

pub async fn create_fs() -> Result<(), Error> {
    todo!()
}

pub async fn import() -> Result<(), Error> {
    Command::new("zpool")
        .arg("import")
        .arg("-f")
        .arg("embassy-data")
        .invoke(crate::ErrorKind::Zfs)
        .await?;
    Ok(())
}

pub async fn mount(password: &str) -> Result<(), Error> {
    // zfs get -H -ovalue mountpoint embassy-data
    todo!()
}
