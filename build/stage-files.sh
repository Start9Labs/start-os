#!/bin/bash
set -eo pipefail

cd "$(git rev-parse --show-toplevel)"

ARCH=${ARCH:-riscv64}
RUST_ARCH=${RUST_ARCH:-riscv64gc}
PROFILE=${PROFILE:-release}
RUST_TARGET_DIR="backend/target/${RUST_ARCH}-unknown-linux-musl/${PROFILE}"
FILES_DIR="openwrt/files"

echo "==> Staging files into ${FILES_DIR}..."

# Clean previous staging
rm -rf "${FILES_DIR}"

# Single binary + symlinks for backward compatibility
mkdir -p "${FILES_DIR}/usr/bin"
cp "${RUST_TARGET_DIR}/startwrt" "${FILES_DIR}/usr/bin/startwrt"
ln -s startwrt "${FILES_DIR}/usr/bin/startwrt-ctrld"
ln -s startwrt "${FILES_DIR}/usr/bin/startwrt-cli"

# UCI config defaults from firstboot_config/
mkdir -p "${FILES_DIR}/etc/config"
for f in backend/firstboot_config/*; do
    cp "$f" "${FILES_DIR}/etc/config/$(basename "$f")"
done

# Procd init script for startwrt-ctrld
mkdir -p "${FILES_DIR}/etc/init.d"
cat > "${FILES_DIR}/etc/init.d/startwrt" << 'INITEOF'
#!/bin/sh /etc/rc.common

START=99
STOP=10

USE_PROCD=1

start_service() {
    procd_open_instance
    procd_set_param command /usr/bin/startwrt-ctrld
    procd_set_param respawn
    procd_set_param stdout 1
    procd_set_param stderr 1
    procd_close_instance
}
INITEOF
chmod +x "${FILES_DIR}/etc/init.d/startwrt"

# Comment out distfeeds entries for repos not hosted on downloads.openwrt.org.
# The spacemit target packages and spacemit_openwrt_feeds are built locally
# but have no upstream package repository, so opkg update fails for them.
# TODO: Host our own package feeds and remove this workaround.
#       See https://github.com/DC-DeepComputing/spacemit-openwrt-feeds
mkdir -p "${FILES_DIR}/etc/uci-defaults"
cat > "${FILES_DIR}/etc/uci-defaults/99-fix-distfeeds" << 'FIXEOF'
#!/bin/sh
sed -i \
    -e '/^src\/gz.*_core.*targets/s/^/# /' \
    -e '/^src\/gz.*_spacemit_openwrt_feeds/s/^/# /' \
    /etc/opkg/distfeeds.conf
FIXEOF
chmod +x "${FILES_DIR}/etc/uci-defaults/99-fix-distfeeds"

# Serial console dispatcher — routes ttyS0 to manufacture, init, or login
mkdir -p "${FILES_DIR}/usr/sbin"
cat > "${FILES_DIR}/usr/sbin/startwrt-serial" << 'SERIALEOF'
#!/bin/sh
# Connect stdin/stdout/stderr to the console device.
# Required because procd's "respawn" action (unlike "askconsole") does not
# set up a controlling terminal for the process.
exec </dev/console >/dev/console 2>&1

# Suppress kernel messages on the serial console — printk writes directly
# to the UART and corrupts terminal I/O (echo, input) for interactive use.
dmesg -n 1

# Determine the boot block device from root= in /proc/cmdline
boot_part=$(cat /proc/cmdline | tr ' ' '\n' | sed -n 's|^root=/dev/||p')
case "$boot_part" in
    mmcblk*p[0-9]*) boot_dev="${boot_part%p[0-9]*}" ;;
    sd*[0-9]*)      boot_dev=$(echo "$boot_part" | sed 's/[0-9]*$//') ;;
    *)              boot_dev="" ;;
esac

# Check device type via sysfs: eMMC reports "MMC", SD cards report "SD"
boot_type=""
[ -n "$boot_dev" ] && [ -f "/sys/block/$boot_dev/device/type" ] && \
    boot_type=$(cat "/sys/block/$boot_dev/device/type")

if [ "$boot_type" = "MMC" ]; then
    # Booted from eMMC — normal operation.
    # Run init without exec so we always fall through to login.
    # (key_backup may not be mounted yet, so the file check can miss;
    #  startwrt-cli init handles the "already initialized" case gracefully.)
    /usr/bin/startwrt-cli init || echo "WARNING: init failed (exit $?)"
    exec /bin/login
else
    # Booted from SD card, USB, or unknown — setup mode.
    # The setup wizard runs over WiFi via startwrt-ctrld.
    echo ""
    echo "========================================"
    echo "   StartWRT Setup Mode"
    echo "========================================"
    echo ""
    echo "Connect to WiFi 'StartWRT' to run the setup wizard."
    echo "Log in below for advanced options."
    echo ""
    exec /bin/login
fi
SERIALEOF
chmod +x "${FILES_DIR}/usr/sbin/startwrt-serial"

# Override inittab to use respawnlate for the serial dispatcher.
# respawnlate runs in STATE_RUNNING (after sysinit completes), so hostname,
# ubus, and all init.d services are available before startwrt-serial starts.
# It also auto-starts without requiring Enter (unlike askconsole) and restarts
# the process on exit. askfirst on an explicit ttyS0 doesn't work on this
# hardware — Enter input is never received.
mkdir -p "${FILES_DIR}/etc"
cat > "${FILES_DIR}/etc/inittab" << 'INITTABEOF'
::sysinit:/etc/init.d/rcS S boot
::shutdown:/etc/init.d/rcS K shutdown
::respawnlate:/usr/sbin/startwrt-serial
INITTABEOF

# Hotplug script for remote access re-evaluation on WAN IP change
mkdir -p "${FILES_DIR}/etc/hotplug.d/iface"
cp backend/hotplug/99-startwrt-remote-access "${FILES_DIR}/etc/hotplug.d/iface/99-startwrt-remote-access"
chmod +x "${FILES_DIR}/etc/hotplug.d/iface/99-startwrt-remote-access"

# Key backup partition mount point
mkdir -p "${FILES_DIR}/key_backup"

echo "==> Staging complete."
