#!/bin/bash
set -e

MAX_IMG_SECTORS=7217792 # 4GB

echo "==== StartOS Image Build ===="

echo "Building for architecture: $IB_TARGET_ARCH"

base_dir="$(dirname "$(readlink -f "$0")")"
prep_results_dir="$base_dir/images-prep"
if systemd-detect-virt -qc; then
	RESULTS_DIR="/srv/artifacts"
else
	RESULTS_DIR="$base_dir/results"
fi
echo "Saving results in: $RESULTS_DIR"

IMAGE_BASENAME=startos-${VERSION_FULL}_${IB_TARGET_PLATFORM}

QEMU_ARCH=${IB_TARGET_ARCH}
BOOTLOADERS=grub-efi,syslinux
if [ "$QEMU_ARCH" = 'amd64' ]; then
	QEMU_ARCH=x86_64
elif [ "$QEMU_ARCH" = 'arm64' ]; then
	QEMU_ARCH=aarch64
	BOOTLOADERS=grub-efi
fi

# TODO: remove when util-linux is released at v2.39
cd $base_dir
git clone --depth=1 --branch=v2.39.3 https://github.com/util-linux/util-linux.git
cd util-linux
./autogen.sh
CC=$QEMU_ARCH-linux-gnu-gcc ./configure --host=$QEMU_ARCH-linux-gnu --disable-all-programs --enable-mount --enable-libmount --enable-libblkid --enable-libuuid --enable-static-programs
CC=$QEMU_ARCH-linux-gnu-gcc make -j mount.static

mkdir -p $prep_results_dir

cd $prep_results_dir

NON_FREE=
if [[ "${IB_TARGET_PLATFORM}" =~ -nonfree$ ]] || [ "${IB_TARGET_PLATFORM}" = "raspberrypi" ]; then
	NON_FREE=1
fi
IMAGE_TYPE=iso
if [ "${IB_TARGET_PLATFORM}" = "raspberrypi" ] || [ "${IB_TARGET_PLATFORM}" = "rockchip64" ]; then
	IMAGE_TYPE=img
fi

ARCHIVE_AREAS="main contrib"
if [ "$NON_FREE" = 1 ]; then
	if [ "$IB_SUITE" = "bullseye" ]; then
		ARCHIVE_AREAS="$ARCHIVE_AREAS non-free"
	elif [ "$IB_SUITE" = "bookworm" ]; then
		ARCHIVE_AREAS="$ARCHIVE_AREAS non-free-firmware"
	fi
fi

PLATFORM_CONFIG_EXTRAS=()
if [ "${IB_TARGET_PLATFORM}" = "raspberrypi" ]; then
	PLATFORM_CONFIG_EXTRAS+=( --firmware-binary false )
	PLATFORM_CONFIG_EXTRAS+=( --firmware-chroot false )
	PLATFORM_CONFIG_EXTRAS+=( --linux-packages linux-image-6.6.51+rpt )
	PLATFORM_CONFIG_EXTRAS+=( --linux-flavours "rpi-v8 rpi-2712" )
elif [ "${IB_TARGET_PLATFORM}" = "rockchip64" ]; then
	PLATFORM_CONFIG_EXTRAS+=( --linux-flavours rockchip64 )
fi


cat > /etc/wgetrc << EOF
retry_connrefused = on
tries = 100
EOF
lb config \
	--iso-application "StartOS v${VERSION_FULL} ${IB_TARGET_ARCH}" \
	--iso-volume "StartOS v${VERSION} ${IB_TARGET_ARCH}" \
	--iso-preparer "START9 LABS; HTTPS://START9.COM" \
	--iso-publisher "START9 LABS; HTTPS://START9.COM" \
	--backports true \
	--bootappend-live "boot=live noautologin" \
	--bootloaders $BOOTLOADERS \
	--mirror-bootstrap "https://deb.debian.org/debian/" \
	--mirror-chroot "https://deb.debian.org/debian/" \
	--mirror-chroot-security "https://security.debian.org/debian-security" \
	-d ${IB_SUITE} \
	-a ${IB_TARGET_ARCH} \
	--bootstrap-qemu-arch ${IB_TARGET_ARCH} \
	--bootstrap-qemu-static /usr/bin/qemu-${QEMU_ARCH}-static \
	--archive-areas "${ARCHIVE_AREAS}" \
	${PLATFORM_CONFIG_EXTRAS[@]}

# Overlays

mkdir -p config/includes.chroot/deb
cp $base_dir/deb/${IMAGE_BASENAME}.deb config/includes.chroot/deb/

mkdir -p config/includes.chroot/usr/local/bin
cp $base_dir/util-linux/mount.static config/includes.chroot/usr/local/bin/mount.next

if [ "${IB_TARGET_PLATFORM}" = "raspberrypi" ]; then
	cp -r $base_dir/raspberrypi/squashfs/* config/includes.chroot/
fi

mkdir -p config/includes.chroot/etc
echo start > config/includes.chroot/etc/hostname
cat > config/includes.chroot/etc/hosts << EOT
127.0.0.1       localhost start
::1             localhost start ip6-localhost ip6-loopback
ff02::1         ip6-allnodes
ff02::2         ip6-allrouters
EOT

# Bootloaders

rm -rf config/bootloaders
cp -r /usr/share/live/build/bootloaders config/bootloaders

cat > config/bootloaders/syslinux/syslinux.cfg << EOF
include menu.cfg
default vesamenu.c32
prompt 0
timeout 50
EOF

cat > config/bootloaders/isolinux/isolinux.cfg << EOF
include menu.cfg
default vesamenu.c32
prompt 0
timeout 50
EOF

rm config/bootloaders/syslinux_common/splash.svg
cp $base_dir/splash.png config/bootloaders/syslinux_common/splash.png
cp $base_dir/splash.png config/bootloaders/isolinux/splash.png
cp $base_dir/splash.png config/bootloaders/grub-pc/splash.png

sed -i -e '2i set timeout=5' config/bootloaders/grub-pc/config.cfg

# Archives

mkdir -p config/archives

if [ "${IB_TARGET_PLATFORM}" = "raspberrypi" ]; then
	curl -fsSL https://archive.raspberrypi.com/debian/raspberrypi.gpg.key | gpg --dearmor -o config/archives/raspi.key
	echo "deb [arch=${IB_TARGET_ARCH} signed-by=/etc/apt/trusted.gpg.d/raspi.key.gpg] https://archive.raspberrypi.com/debian/ ${IB_SUITE} main" > config/archives/raspi.list
fi

cat > config/archives/backports.pref <<- EOF
Package: linux-image-*
Pin: release n=${IB_SUITE}-backports
Pin-Priority: 500
EOF

if [ "${IB_TARGET_PLATFORM}" = "rockchip64" ]; then
	curl -fsSL https://apt.armbian.com/armbian.key | gpg --dearmor -o config/archives/armbian.key
	echo "deb https://apt.armbian.com/ ${IB_SUITE} main" > config/archives/armbian.list
fi

curl -fsSL https://deb.torproject.org/torproject.org/A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89.asc > config/archives/tor.key
echo "deb [arch=${IB_TARGET_ARCH} signed-by=/etc/apt/trusted.gpg.d/tor.key.gpg] https://deb.torproject.org/torproject.org ${IB_SUITE} main" > config/archives/tor.list

curl -fsSL https://download.docker.com/linux/debian/gpg | gpg --dearmor -o config/archives/docker.key
echo "deb [arch=${IB_TARGET_ARCH} signed-by=/etc/apt/trusted.gpg.d/docker.key.gpg] https://download.docker.com/linux/debian ${IB_SUITE} stable" > config/archives/docker.list

# Dependencies

## Base dependencies
dpkg-deb --fsys-tarfile $base_dir/deb/${IMAGE_BASENAME}.deb | tar --to-stdout -xvf - ./usr/lib/startos/depends > config/package-lists/startos-depends.list.chroot

## Firmware
if [ "$NON_FREE" = 1 ]; then
	echo 'firmware-iwlwifi firmware-misc-nonfree firmware-brcm80211 firmware-realtek firmware-atheros firmware-libertas firmware-amd-graphics' > config/package-lists/nonfree.list.chroot
fi

if [ "${IB_TARGET_PLATFORM}" = "raspberrypi" ]; then
	echo 'raspberrypi-net-mods raspberrypi-sys-mods raspi-config raspi-firmware raspi-gpio raspi-utils rpi-eeprom rpi-update rpi.gpio-common parted' > config/package-lists/bootloader.list.chroot
else
	echo 'grub-efi grub2-common' > config/package-lists/bootloader.list.chroot
fi
if [ "${IB_TARGET_ARCH}" = "amd64" ] || [ "${IB_TARGET_ARCH}" = "i386" ]; then
	echo 'grub-pc-bin' >> config/package-lists/bootloader.list.chroot
fi

cat > config/hooks/normal/9000-install-startos.hook.chroot << EOF
#!/bin/bash

set -e

apt-get install -y /deb/${IMAGE_BASENAME}.deb
rm -rf /deb

if [ "${IB_SUITE}" = bookworm ]; then
	echo 'deb https://deb.debian.org/debian/ bullseye main' > /etc/apt/sources.list.d/bullseye.list
	apt-get update
	apt-get install -y postgresql-13
	rm /etc/apt/sources.list.d/bullseye.list
	apt-get update
fi

if [ "${IB_TARGET_PLATFORM}" = "raspberrypi" ]; then
	ln -sf /usr/bin/pi-beep /usr/local/bin/beep
	SKIP_WARNING=1 SKIP_BOOTLOADER=1 SKIP_CHECK_PARTITION=1 WANT_64BIT=1 WANT_PI4=1 WANT_PI5=1 BOOT_PART=/boot rpi-update stable
	for f in /usr/lib/modules/*; do
    	v=\${f#/usr/lib/modules/}
		echo "Configuring raspi kernel '\$v'"
    	extract-ikconfig "/usr/lib/modules/\$v/kernel/kernel/configs.ko.xz" > /boot/config-\$v
	done
	mkinitramfs -c gzip -o /boot/initramfs8 6.6.74-v8+
	mkinitramfs -c gzip -o /boot/initramfs_2712 6.6.74-v8-16k+
fi

useradd --shell /bin/bash -G startos -m start9
echo start9:embassy | chpasswd
usermod -aG sudo start9

echo "start9 ALL=(ALL:ALL) NOPASSWD: ALL" | sudo tee "/etc/sudoers.d/010_start9-nopasswd"

if [ "${IB_TARGET_PLATFORM}" != "raspberrypi" ]; then
	/usr/lib/startos/scripts/enable-kiosk
fi

if ! [[ "${IB_OS_ENV}" =~ (^|-)dev($|-) ]]; then
	passwd -l start9
fi

EOF

SOURCE_DATE_EPOCH="${SOURCE_DATE_EPOCH:-$(date '+%s')}"

lb bootstrap
lb chroot
lb installer
lb binary_chroot
lb chroot_prep install all mode-apt-install-binary mode-archives-chroot
ln -sf /run/systemd/resolve/stub-resolv.conf chroot/chroot/etc/resolv.conf
lb binary_rootfs

cp $prep_results_dir/binary/live/filesystem.squashfs $RESULTS_DIR/$IMAGE_BASENAME.squashfs

if [ "${IMAGE_TYPE}" = iso ]; then

	lb binary_manifest
	lb binary_package-lists
	lb binary_linux-image
	lb binary_memtest
	lb binary_grub-legacy
	lb binary_grub-pc
	lb binary_grub_cfg
	lb binary_syslinux
	lb binary_disk
	lb binary_loadlin
	lb binary_win32-loader
	lb binary_includes
	lb binary_grub-efi
	lb binary_hooks
	lb binary_checksums
	find binary -newermt "$(date -d@${SOURCE_DATE_EPOCH} '+%Y-%m-%d %H:%M:%S')" -printf "%y %p\n" -exec touch '{}' -d@${SOURCE_DATE_EPOCH} --no-dereference ';' > binary.modified_timestamps
	lb binary_iso
	lb binary_onie
	lb binary_netboot
	lb binary_tar
	lb binary_hdd
	lb binary_zsync
	lb chroot_prep remove all mode-archives-chroot
	lb source

	mv $prep_results_dir/live-image-${IB_TARGET_ARCH}.hybrid.iso $RESULTS_DIR/$IMAGE_BASENAME.iso

elif [ "${IMAGE_TYPE}" = img ]; then

	function partition_for () {
		if [[ "$1" =~ [0-9]+$ ]]; then
			echo "$1p$2"
		else
			echo "$1$2"
		fi
	}

	ROOT_PART_END=$MAX_IMG_SECTORS
	TARGET_NAME=$prep_results_dir/${IMAGE_BASENAME}.img
	TARGET_SIZE=$[($ROOT_PART_END+1)*512]
	truncate -s $TARGET_SIZE $TARGET_NAME
	(
		echo o
		echo x
		echo i
		echo "0xcb15ae4d"
		echo r
		echo n
		echo p
		echo 1
		echo 2048
		echo 526335
		echo t
		echo c
		echo n
		echo p
		echo 2
		echo 526336
		echo $ROOT_PART_END
		echo a
		echo 1
		echo w
	) | fdisk $TARGET_NAME
	OUTPUT_DEVICE=$(losetup --show -fP $TARGET_NAME)
	mkfs.ext4 `partition_for ${OUTPUT_DEVICE} 2`
	mkfs.vfat `partition_for ${OUTPUT_DEVICE} 1`

	TMPDIR=$(mktemp -d)

	mkdir -p $TMPDIR/boot $TMPDIR/root 
	mount `partition_for ${OUTPUT_DEVICE} 2` $TMPDIR/root
	mount `partition_for ${OUTPUT_DEVICE} 1` $TMPDIR/boot
	unsquashfs -n -f -d $TMPDIR $prep_results_dir/binary/live/filesystem.squashfs boot

	mkdir $TMPDIR/root/images $TMPDIR/root/config
	B3SUM=$(b3sum $prep_results_dir/binary/live/filesystem.squashfs | head -c 16)
	cp $prep_results_dir/binary/live/filesystem.squashfs $TMPDIR/root/images/$B3SUM.rootfs
	ln -rsf $TMPDIR/root/images/$B3SUM.rootfs $TMPDIR/root/config/current.rootfs

	mkdir -p $TMPDIR/next $TMPDIR/lower $TMPDIR/root/config/work $TMPDIR/root/config/overlay
	mount $TMPDIR/root/config/current.rootfs $TMPDIR/lower

	mount -t overlay -o lowerdir=$TMPDIR/lower,workdir=$TMPDIR/root/config/work,upperdir=$TMPDIR/root/config/overlay overlay $TMPDIR/next

	if [ "${IB_TARGET_PLATFORM}" = "raspberrypi" ]; then
		sed -i 's| boot=startos| boot=startos init=/usr/lib/startos/scripts/init_resize\.sh|' $TMPDIR/boot/cmdline.txt
		rsync -a $base_dir/raspberrypi/img/ $TMPDIR/next/
	fi

	umount $TMPDIR/next
	umount $TMPDIR/lower

	umount $TMPDIR/boot
	umount $TMPDIR/root

	e2fsck -fy `partition_for ${OUTPUT_DEVICE} 2`
	resize2fs -M `partition_for ${OUTPUT_DEVICE} 2`

	BLOCK_COUNT=$(dumpe2fs -h `partition_for ${OUTPUT_DEVICE} 2` | awk '/^Block count:/ { print $3 }')
	BLOCK_SIZE=$(dumpe2fs -h `partition_for ${OUTPUT_DEVICE} 2` | awk '/^Block size:/ { print $3 }')
	SECTOR_LEN=$[$BLOCK_COUNT*$BLOCK_SIZE/512]

	losetup -d $OUTPUT_DEVICE

	(
		echo d
		echo 2
		echo n
		echo p
		echo 2
		echo 526336
		echo +$SECTOR_LEN
		echo w
	) | fdisk $TARGET_NAME

	ROOT_PART_END=$[526336+$SECTOR_LEN]
	TARGET_SIZE=$[($ROOT_PART_END+1)*512]
	truncate -s $TARGET_SIZE $TARGET_NAME

	mv $TARGET_NAME $RESULTS_DIR/$IMAGE_BASENAME.img

fi
