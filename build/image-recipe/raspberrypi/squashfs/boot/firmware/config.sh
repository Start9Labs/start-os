#!/bin/sh

cat << EOF

# Enable audio (loads snd_bcm2835)
dtparam=audio=on

# Automatically load overlays for detected cameras
camera_auto_detect=1

# Automatically load overlays for detected DSI displays
display_auto_detect=1

# Enable DRM VC4 V3D driver
dtoverlay=vc4-kms-v3d
max_framebuffers=2

# Run in 64-bit mode
arm_64bit=1

# Disable compensation for displays with overscan
disable_overscan=1

# Serial console for early boot debug
enable_uart=1
uart_2ndstage=1

# Hand off to GRUB via EDK2 UEFI firmware. EDK2 owns kernel selection
# from here; do not pass cmdline/initramfs tags down to it.
disable_commandline_tags=1

[cm4]
# Enable host mode on the 2711 built-in XHCI USB controller.
# This line should be removed if the legacy DWC2 controller is required
# (e.g. for USB device mode) or if USB support is not required.
otg_mode=1

[pi4]
arm_boost=1
enable_gic=1
armstub=RPI_EFI_RPI4.fd
device_tree_address=0x3e0000
device_tree_end=0x400000
dtoverlay=upstream-pi4

[pi5]
armstub=RPI_EFI_RPI5.fd
device_tree_address=0x1f0000
device_tree_end=0x210000
# Pi 5 NVMe via the M.2 HAT+
dtparam=pciex1

[all]
gpu_mem=16
dtoverlay=pwm-2chan,disable-bt
dtoverlay=miniuart-bt

EOF
