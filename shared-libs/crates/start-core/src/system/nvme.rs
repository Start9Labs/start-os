use std::path::Path;
use tracing::warn;

#[derive(Debug, PartialEq)]
pub struct NvmeQuirkStatus {
    pub nvme_detected: bool,
    pub hmb_disabled: Option<bool>,
    pub apst_disabled: Option<bool>,
}

pub async fn check_nvme_quirks() -> Result<(), std::io::Error> {
    let sys_class_nvme = Path::new("/sys/class/nvme");
    let cmdline_path = Path::new("/proc/cmdline");

    let status = check_nvme_quirks_impl(sys_class_nvme, cmdline_path).await?;

    if status.nvme_detected {
        tracing::info!("NVMe controllers detected. Verifying power transition and Host Memory Buffer quirks...");
        
        if let Some(false) = status.hmb_disabled {
            warn!(
                "NVMe Quirk Warning: Host Memory Buffer (HMB) might be enabled on this system! \
                Under sustained write load (like Bitcoin sync), HMB can interact badly with power management \
                and wedge DRAM-less NVMe controllers (such as Samsung 990 EVO Plus). \
                Please consider adding 'nvme.max_host_mem_size_mb=0' to your kernel command line via GRUB if you experience resets."
            );
        } else if let Some(true) = status.hmb_disabled {
            tracing::info!("NVMe Quirk Check: Host Memory Buffer (HMB) is successfully disabled via kernel cmdline.");
        }

        if let Some(false) = status.apst_disabled {
            warn!(
                "NVMe Quirk Warning: Autonomous Power State Transitions (APST) might be enabled on this system! \
                Power state transitions can wedge NVMe controllers. It is highly recommended to disable them \
                if you experience filesystem read-only errors. \
                Please consider adding 'nvme_core.default_ps_max_latency_us=0' to your kernel command line via GRUB."
            );
        } else if let Some(true) = status.apst_disabled {
            tracing::info!("NVMe Quirk Check: Autonomous Power State Transitions (APST) are successfully disabled (latency set to 0 via cmdline).");
        }
    } else {
        tracing::debug!("No NVMe controllers detected on this platform. Skipping quirks validation.");
    }

    Ok(())
}

async fn check_nvme_quirks_impl(
    sys_class_nvme: &Path,
    cmdline_path: &Path,
) -> Result<NvmeQuirkStatus, std::io::Error> {
    if !sys_class_nvme.exists() {
        return Ok(NvmeQuirkStatus {
            nvme_detected: false,
            hmb_disabled: None,
            apst_disabled: None,
        });
    }

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

    // Read /proc/cmdline to check if quirks are disabled globally
    let mut hmb_disabled = Some(false);
    let mut apst_disabled = Some(false);

    if cmdline_path.exists() {
        if let Ok(cmdline) = tokio::fs::read_to_string(cmdline_path).await {
            if cmdline.contains("nvme.max_host_mem_size_mb=0") {
                hmb_disabled = Some(true);
            }
            if cmdline.contains("nvme_core.default_ps_max_latency_us=0") {
                apst_disabled = Some(true);
            }
        } else {
            hmb_disabled = None;
            apst_disabled = None;
        }
    } else {
        hmb_disabled = None;
        apst_disabled = None;
    }

    Ok(NvmeQuirkStatus {
        nvme_detected: true,
        hmb_disabled,
        apst_disabled,
    })
}
