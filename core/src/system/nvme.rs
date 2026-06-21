use std::path::Path;
use tracing::warn;
use crate::prelude::*;

/// Status representing the state of NVMe quirks on the system.
///
/// Under sustained write load (e.g. Bitcoin Core initial block download), DRAM-less NVMe
/// controllers (like the Samsung 990 EVO Plus) that use Host Memory Buffer (HMB) tend to
/// wedge when transitioning between autonomous power states.
/// Disabling APST (Autonomous Power State Transitions) exit latency and disabling HMB resolves this.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NvmeQuirkStatus {
    /// True if at least one NVMe controller was found in the sysfs directory.
    pub nvme_detected: bool,
    /// Some(true) if HMB is successfully disabled (max_host_mem_size_mb == 0).
    /// Some(false) if HMB is enabled.
    /// None if the parameter file could not be read or does not exist.
    pub hmb_disabled: Option<bool>,
    /// Some(true) if APST exit latency limit is overridden to 0 (effectively disabling APST).
    /// Some(false) if APST exit latency limit is not 0 (power transitions are enabled).
    /// None if the parameter file could not be read or does not exist.
    pub apst_disabled: Option<bool>,
}

/// Run-time verification of NVMe APST and HMB workarounds.
///
/// This validation check runs during system startup. If NVMe devices are present but the
/// workarounds are not fully applied (e.g., if a user customized the GRUB configuration or boot
/// options), we log clear warning messages. This provides defense-in-depth and makes debugging
/// read-only filesystem issues far easier on problematic drives.
pub async fn check_nvme_quirks() -> Result<(), Error> {
    let sys_class_nvme = Path::new("/sys/class/nvme");
    let hmb_param_path = Path::new("/sys/module/nvme/parameters/max_host_mem_size_mb");
    let apst_param_path = Path::new("/sys/module/nvme_core/parameters/default_ps_max_latency_us");

    // Perform the checks using our mockable inner implementation.
    let status = check_nvme_quirks_impl(sys_class_nvme, hmb_param_path, apst_param_path).await?;

    if status.nvme_detected {
        tracing::info!("NVMe controllers detected. Verifying power transition and Host Memory Buffer quirks...");
        
        if let Some(false) = status.hmb_disabled {
            warn!(
                "NVMe Quirk Warning: Host Memory Buffer (HMB) is enabled on this system! \
                Under sustained write load (like Bitcoin sync), HMB can interact badly with power management \
                and wedge DRAM-less NVMe controllers (such as Samsung 990 EVO Plus). \
                Please ensure 'options nvme max_host_mem_size_mb=0' is set in your /etc/modprobe.d/ configurations."
            );
        } else if let Some(true) = status.hmb_disabled {
            tracing::info!("NVMe Quirk Check: Host Memory Buffer (HMB) is successfully disabled.");
        }

        if let Some(false) = status.apst_disabled {
            warn!(
                "NVMe Quirk Warning: Autonomous Power State Transitions (APST) are enabled on this system! \
                Power state transitions can wedge NVMe controllers. It is highly recommended to disable them. \
                Please ensure 'nvme_core.default_ps_max_latency_us=0' is passed to the kernel command line via GRUB."
            );
        } else if let Some(true) = status.apst_disabled {
            tracing::info!("NVMe Quirk Check: Autonomous Power State Transitions (APST) are successfully disabled (latency set to 0).");
        }
    } else {
        tracing::debug!("No NVMe controllers detected on this platform. Skipping quirks validation.");
    }

    Ok(())
}

/// The inner, mockable checking logic that queries sysfs directories.
async fn check_nvme_quirks_impl(
    sys_class_nvme: &Path,
    hmb_param_path: &Path,
    apst_param_path: &Path,
) -> Result<NvmeQuirkStatus, Error> {
    if !sys_class_nvme.exists() {
        return Ok(NvmeQuirkStatus {
            nvme_detected: false,
            hmb_disabled: None,
            apst_disabled: None,
        });
    }

    // Read the sysfs class directory to see if there are any active controllers.
    // If it's empty or cannot be read, assume no NVMe devices.
    let mut entries = match tokio::fs::read_dir(sys_class_nvme).await {
        Ok(e) => e,
        Err(err) => {
            tracing::debug!("Failed to read sysfs class nvme directory: {err}");
            return Ok(NvmeQuirkStatus {
                nvme_detected: false,
                hmb_disabled: None,
                apst_disabled: None,
            });
        }
    };

    let mut has_nvme = false;
    while let Ok(Some(entry)) = entries.next_entry().await {
        let name = entry.file_name();
        let name_str = name.to_string_lossy();
        // Standard controllers are named like nvme0, nvme1, etc.
        if name_str.starts_with("nvme") {
            has_nvme = true;
            break;
        }
    }

    if !has_nvme {
        return Ok(NvmeQuirkStatus {
            nvme_detected: false,
            hmb_disabled: None,
            apst_disabled: None,
        });
    }

    // Query HMB parameter
    let hmb_disabled = if hmb_param_path.exists() {
        match tokio::fs::read_to_string(hmb_param_path).await {
            Ok(content) => {
                let parsed = content.trim().parse::<u32>().ok();
                parsed.map(|val| val == 0)
            }
            Err(err) => {
                tracing::debug!("Failed to read max_host_mem_size_mb: {err}");
                None
            }
        }
    } else {
        None
    };

    // Query APST exit latency parameter
    let apst_disabled = if apst_param_path.exists() {
        match tokio::fs::read_to_string(apst_param_path).await {
            Ok(content) => {
                let parsed = content.trim().parse::<u64>().ok();
                parsed.map(|val| val == 0)
            }
            Err(err) => {
                tracing::debug!("Failed to read default_ps_max_latency_us: {err}");
                None
            }
        }
    } else {
        None
    };

    Ok(NvmeQuirkStatus {
        nvme_detected: true,
        hmb_disabled,
        apst_disabled,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;
    use tokio::fs::{create_dir, write};

    #[tokio::test]
    async fn test_check_nvme_quirks_no_nvme() {
        let tmp = tempdir().unwrap();
        let sys_class = tmp.path().join("sys_class_nvme");
        let hmb_path = tmp.path().join("hmb");
        let apst_path = tmp.path().join("apst");

        // When sys_class directory does not exist
        let status = check_nvme_quirks_impl(&sys_class, &hmb_path, &apst_path)
            .await
            .unwrap();

        assert_eq!(
            status,
            NvmeQuirkStatus {
                nvme_detected: false,
                hmb_disabled: None,
                apst_disabled: None,
            }
        );
    }

    #[tokio::test]
    async fn test_check_nvme_quirks_empty_class_dir() {
        let tmp = tempdir().unwrap();
        let sys_class = tmp.path().join("sys_class_nvme");
        create_dir(&sys_class).await.unwrap();

        let hmb_path = tmp.path().join("hmb");
        let apst_path = tmp.path().join("apst");

        // When sys_class directory exists but is empty
        let status = check_nvme_quirks_impl(&sys_class, &hmb_path, &apst_path)
            .await
            .unwrap();

        assert_eq!(
            status,
            NvmeQuirkStatus {
                nvme_detected: false,
                hmb_disabled: None,
                apst_disabled: None,
            }
        );
    }

    #[tokio::test]
    async fn test_check_nvme_quirks_with_quirks_disabled() {
        let tmp = tempdir().unwrap();
        let sys_class = tmp.path().join("sys_class_nvme");
        create_dir(&sys_class).await.unwrap();
        // Add a dummy controller file
        write(sys_class.join("nvme0"), "").await.unwrap();

        let hmb_path = tmp.path().join("hmb");
        write(&hmb_path, "0\n").await.unwrap();

        let apst_path = tmp.path().join("apst");
        write(&apst_path, "0").await.unwrap();

        let status = check_nvme_quirks_impl(&sys_class, &hmb_path, &apst_path)
            .await
            .unwrap();

        assert_eq!(
            status,
            NvmeQuirkStatus {
                nvme_detected: true,
                hmb_disabled: Some(true),
                apst_disabled: Some(true),
            }
        );
    }

    #[tokio::test]
    async fn test_check_nvme_quirks_with_quirks_enabled() {
        let tmp = tempdir().unwrap();
        let sys_class = tmp.path().join("sys_class_nvme");
        create_dir(&sys_class).await.unwrap();
        write(sys_class.join("nvme1"), "").await.unwrap();

        let hmb_path = tmp.path().join("hmb");
        write(&hmb_path, "64\n").await.unwrap(); // HMB has size > 0 (enabled)

        let apst_path = tmp.path().join("apst");
        write(&apst_path, "5500").await.unwrap(); // APST exit latency is non-zero (enabled)

        let status = check_nvme_quirks_impl(&sys_class, &hmb_path, &apst_path)
            .await
            .unwrap();

        assert_eq!(
            status,
            NvmeQuirkStatus {
                nvme_detected: true,
                hmb_disabled: Some(false),
                apst_disabled: Some(false),
            }
        );
    }

    #[tokio::test]
    async fn test_check_nvme_quirks_missing_files() {
        let tmp = tempdir().unwrap();
        let sys_class = tmp.path().join("sys_class_nvme");
        create_dir(&sys_class).await.unwrap();
        write(sys_class.join("nvme0"), "").await.unwrap();

        let hmb_path = tmp.path().join("hmb");
        let apst_path = tmp.path().join("apst");

        // Files do not exist
        let status = check_nvme_quirks_impl(&sys_class, &hmb_path, &apst_path)
            .await
            .unwrap();

        assert_eq!(
            status,
            NvmeQuirkStatus {
                nvme_detected: true,
                hmb_disabled: None,
                apst_disabled: None,
            }
        );
    }
}
