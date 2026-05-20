# Raspberry Pi image notes

The Raspberry Pi target produces a **single** `.img` that is a **live
installer** for Pi 4, Pi 400, CM4, Pi 5, Pi 500, and CM5 — same UX as
the x86 `.iso`. Flash to USB or microSD; boot the Pi; the live system
runs setup mode on port 80; user picks a target disk; the installer
(in `core/src/os_install/`) writes installed StartOS to that target;
reboot from the target.

The closed-source VPU firmware reads `config.txt`, selects the right
EDK2 UEFI build per model via `[pi4]` / `[pi5]` conditional sections,
and chainloads GRUB through UEFI exactly as on any other arm64 server
board.

## Boot chain

```
VPU ROM
  └─ bootcode.bin / start*.elf / fixup*.dat   (closed Pi firmware)
       └─ RPI_EFI_RPI4.fd   on Pi 4 family    (pftf/RPi4)
       └─ RPI_EFI_RPI5.fd   on Pi 5 family    (NumberOneGit/rpi5-uefi)
             └─ GRUB EFI    (arm64-efi)
                  └─ Linux kernel + initrd
                       └─ boot=live  → live-init mounts
                          /boot/live/filesystem.squashfs as rootfs
                          (live media), or boot=startos → installed disk
```

The OS-side install path (`core/src/os_install/`) is fully shared with
the other UEFI arm64 targets — `grub-install --target=arm64-efi`. The
only Pi-specific addition is a 128 MiB firmware partition (partition 1
in the installed-disk GPT) that the installer populates by mirroring
the live media's `/boot/firmware/` (VPU blobs + EDK2 `.fd` + config.txt
+ DTBs + overlays). Pi VPU reads from that partition on every boot;
nothing else does.

## Image layout

The live installer image has three partitions:

| # | Name     | Size  | FS    | Contents                                  |
|---|----------|-------|-------|-------------------------------------------|
| 1 | firmware | 128MB | FAT32 | VPU blobs + EDK2 .fd + config.txt + DTBs  |
| 2 | efi      | 100MB | FAT32 | GRUB-EFI loader (`EFI/BOOT/BOOTAA64.EFI`) |
| 3 | boot     | ~1GB  | FAT32 | GRUB modules + `grub.cfg`, kernels, initrds, `/live/filesystem.squashfs` |

No root partition — the live system overlay-mounts the squashfs as
its rootfs via live-init. When the user runs the installer from setup
mode, `os_install` partitions the target disk with the full installed
layout (firmware + efi + boot + btrfs root + optional data).

## Sources

The VPU blobs + DTBs + overlays come from the Debian `raspi-firmware`
package on `archive.raspberrypi.com` (see `build/dpkg-deps/raspberrypi.depends`).

### Kernels

Both Pi-vendor kernel flavours from `archive.raspberrypi.com` are
installed:

- `linux-image-rpi-v8` — Cortex-A72 baseline, runs on Pi 4 / 400 / CM4
  (and on Pi 5 as a fallback).
- `linux-image-rpi-2712` — Cortex-A76 tuned, runs on Pi 5 / 500 / CM5
  only. Would trap on a Pi 4.

`update-grub` produces one menuentry per installed kernel. A custom
`/etc/grub.d/05_pi_kernel_select` (shipped in
`squashfs/etc/grub.d/`) emits a grub.cfg prologue that reads the
board model from EDK2's SMBIOS table at boot time and points
`${default}` at the matching submenu entry. Net effect: Pi 5 hardware
boots `rpi-2712`, everything else boots `rpi-v8`, all from the same
image with no first-boot rewrite.

The EDK2 UEFI binaries are downloaded at image-build time, pinned by
version + SHA-256 in `build/image-recipe/build.sh`:

- **Pi 4:** [`pftf/RPi4`](https://github.com/pftf/RPi4) — Pi Firmware
  Task Force builds of upstream
  [`tianocore/edk2-platforms`](https://github.com/tianocore/edk2-platforms/tree/master/Platform/RaspberryPi/RPi4).
  Mature, multi-distro, releases since 2020.
- **Pi 5:** [`NumberOneGit/rpi5-uefi`](https://github.com/NumberOneGit/rpi5-uefi)
  — fork of `worproject/rpi5-uefi` (the latter was archived
  2025-02-04 with no D0-stepping support). Added D0 silicon support
  + bug fixes. **Individual maintainer**; longevity is not
  guaranteed.

## Supply-chain risk

There is no equivalent of `pftf` for Pi 5 yet. Tianocore upstream has
no Pi 5 platform tree. Other distributions either ship U-Boot (which
on Pi 5 currently lacks PCIe / NVMe support upstream — openSUSE
Tumbleweed) or sidestep the bootloader question entirely by direct-
booting the kernel from VPU firmware (Ubuntu Server, Raspberry Pi OS).

We are pinning a specific NumberOneGit release by SHA-256. If the
upstream goes inactive:

1. The pinned release continues to work; nothing breaks immediately.
2. Bumping the EDK2 version on Pi 5 requires either a new
   NumberOneGit release, picking up a successor fork, or a switch
   away from EDK2 entirely (e.g. fallback to U-Boot once mainline
   gains BCM2712 PCIe — apparently imminent as of 2026-05).
3. Worst case we mirror the pinned `.zip` under `Start9Labs/` so the
   release artifact survives upstream deletion.

This trade-off was accepted in preference to:

- staying on U-Boot (which we tried; Debian's `u-boot-rpi` is BCM2711-
  era and mainline U-Boot can't see PCIe on BCM2712 yet — Pi 5 NVMe
  boots simply don't work).
- shipping separate Pi 4 / Pi 5 images.
- waiting indefinitely for an upstream-blessed Pi 5 EDK2 build.

## Pi 5 NVMe boot

Pi 5 + M.2 HAT+ + NVMe is the expected target for serious deployments.
For this to work:

1. `dtparam=pciex1` in `config.txt` (set in our `config.sh`).
2. EDK2 must enumerate PCIe early enough to expose NVMe to GRUB.
   NumberOneGit's build does this.
3. The Pi EEPROM `BOOT_ORDER` needs `0xf416` or similar to attempt
   NVMe before SD. This is user setup, not image config — covered in
   the Pi 5 + StartOS install docs in `start-docs`.
