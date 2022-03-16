use std::num::ParseIntError;
use std::path::Path;

use color_eyre::eyre::eyre;
use tokio::io::AsyncWriteExt;
use tracing::instrument;

use super::BOOT_RW_PATH;
use crate::util::AtomicFile;
use crate::Error;

pub const QUIRK_PATH: &'static str = "/sys/module/usb_storage/parameters/quirks";

pub const WHITELIST: [(VendorId, ProductId); 5] = [
    (VendorId(0x1d6b), ProductId(0x0002)), // root hub usb2
    (VendorId(0x1d6b), ProductId(0x0003)), // root hub usb3
    (VendorId(0x2109), ProductId(0x3431)),
    (VendorId(0x1058), ProductId(0x262f)), // western digital black HDD
    (VendorId(0x04e8), ProductId(0x4001)), // Samsung T7
];

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct VendorId(u16);
impl std::str::FromStr for VendorId {
    type Err = ParseIntError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        u16::from_str_radix(s.trim(), 16).map(VendorId)
    }
}
impl std::fmt::Display for VendorId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04x}", self.0)
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ProductId(u16);
impl std::str::FromStr for ProductId {
    type Err = ParseIntError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        u16::from_str_radix(s.trim(), 16).map(ProductId)
    }
}
impl std::fmt::Display for ProductId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04x}", self.0)
    }
}

#[derive(Clone, Debug)]
pub struct Quirks(Vec<(VendorId, ProductId)>);
impl Quirks {
    pub fn add(&mut self, vendor: VendorId, product: ProductId) {
        self.0.push((vendor, product));
    }
    pub fn contains(&self, vendor: VendorId, product: ProductId) -> bool {
        self.0.contains(&(vendor, product))
    }
}
impl std::fmt::Display for Quirks {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut comma = false;
        for (vendor, product) in &self.0 {
            if comma {
                write!(f, ",")?;
            } else {
                comma = true;
            }
            write!(f, "{}:{}:u", vendor, product)?;
        }
        Ok(())
    }
}
impl std::str::FromStr for Quirks {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        let mut quirks = Vec::new();
        for item in s.split(",") {
            if let [vendor, product, "u"] = item.splitn(3, ":").collect::<Vec<_>>().as_slice() {
                quirks.push((vendor.parse()?, product.parse()?));
            } else {
                return Err(Error::new(
                    eyre!("Invalid quirk: `{}`", item),
                    crate::ErrorKind::DiskManagement,
                ));
            }
        }
        Ok(Quirks(quirks))
    }
}

#[instrument]
pub async fn update_quirks(quirks: &mut Quirks) -> Result<Vec<String>, Error> {
    let mut usb_devices = tokio::fs::read_dir("/sys/bus/usb/devices/").await?;
    let mut to_reconnect = Vec::new();
    while let Some(usb_device) = usb_devices.next_entry().await? {
        if tokio::fs::metadata(usb_device.path().join("idVendor"))
            .await
            .is_err()
        {
            continue;
        }
        let vendor = tokio::fs::read_to_string(usb_device.path().join("idVendor"))
            .await?
            .parse()?;
        let product = tokio::fs::read_to_string(usb_device.path().join("idProduct"))
            .await?
            .parse()?;
        if WHITELIST.contains(&(vendor, product)) || quirks.contains(vendor, product) {
            continue;
        }
        quirks.add(vendor, product);
        {
            // write quirks to sysfs
            let mut quirk_file = tokio::fs::File::create(QUIRK_PATH).await?;
            quirk_file.write_all(quirks.to_string().as_bytes()).await?;
            quirk_file.sync_all().await?;
            drop(quirk_file);
        }

        disconnect_usb(usb_device.path()).await?;
        let (vendor_name, product_name) = tokio::try_join!(
            tokio::fs::read_to_string(usb_device.path().join("manufacturer")),
            tokio::fs::read_to_string(usb_device.path().join("product")),
        )?;
        to_reconnect.push(format!("{} {}", vendor_name, product_name));
    }
    Ok(to_reconnect)
}

#[instrument(skip(usb_device_path))]
pub async fn disconnect_usb(usb_device_path: impl AsRef<Path>) -> Result<(), Error> {
    let authorized_path = usb_device_path.as_ref().join("bConfigurationValue");
    let mut authorized_file = tokio::fs::File::create(&authorized_path).await?;
    authorized_file.write_all(b"0").await?;
    authorized_file.sync_all().await?;
    drop(authorized_file);
    Ok(())
}

#[instrument]
pub async fn fetch_quirks() -> Result<Quirks, Error> {
    Ok(tokio::fs::read_to_string(QUIRK_PATH).await?.parse()?)
}

#[instrument]
pub async fn save_quirks(quirks: &Quirks) -> Result<(), Error> {
    let orig_path = Path::new(BOOT_RW_PATH).join("cmdline.txt.orig");
    let target_path = Path::new(BOOT_RW_PATH).join("cmdline.txt");
    if tokio::fs::metadata(&orig_path).await.is_err() {
        tokio::fs::copy(&target_path, &orig_path).await?;
    }
    let cmdline = tokio::fs::read_to_string(&orig_path).await?;
    let mut target = AtomicFile::new(&target_path).await?;
    target
        .write_all(format!("usb-storage.quirks={} {}", quirks, cmdline).as_bytes())
        .await?;
    target.save().await?;

    Ok(())
}
