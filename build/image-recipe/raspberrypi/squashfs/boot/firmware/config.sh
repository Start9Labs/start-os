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

[cm4]
# Enable host mode on the 2711 built-in XHCI USB controller.
# This line should be removed if the legacy DWC2 controller is required
# (e.g. for USB device mode) or if USB support is not required.
otg_mode=1

[pi4]
# Run as fast as firmware / board allows
arm_boost=1

[all]
gpu_mem=16
dtoverlay=pwm-2chan,disable-bt

# Enable UART for U-Boot and serial console
enable_uart=1

# Load U-Boot as the bootloader (GRUB is chainloaded from U-Boot)
kernel=u-boot.bin

EOF
