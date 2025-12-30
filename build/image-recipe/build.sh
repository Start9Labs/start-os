#!/bin/bash
set -e

MAX_IMG_LEN=$((4 * 1024 * 1024 * 1024)) # 4GB

echo "==== StartOS Image Build ===="

echo "Building for architecture: $IB_TARGET_ARCH"

SOURCE_DIR="$(realpath $(dirname "${BASH_SOURCE[0]}"))"

base_dir="$(pwd	-P)"
prep_results_dir="$base_dir/images-prep"
RESULTS_DIR="$base_dir/results"
echo "Saving results in: $RESULTS_DIR"

DEB_PATH="$base_dir/$1"

VERSION="$(dpkg-deb --fsys-tarfile $DEB_PATH | tar --to-stdout -xvf - ./usr/lib/startos/VERSION.txt)"
GIT_HASH="$(dpkg-deb --fsys-tarfile $DEB_PATH | tar --to-stdout -xvf - ./usr/lib/startos/GIT_HASH.txt)"
if [[ "$GIT_HASH" =~ ^@ ]]; then
  GIT_HASH="unknown"
else
  GIT_HASH="$(echo -n "$GIT_HASH" |  head -c 7)"
fi
IB_OS_ENV="$(dpkg-deb --fsys-tarfile $DEB_PATH | tar --to-stdout -xvf - ./usr/lib/startos/ENVIRONMENT.txt)"
IB_TARGET_PLATFORM="$(dpkg-deb --fsys-tarfile $DEB_PATH | tar --to-stdout -xvf - ./usr/lib/startos/PLATFORM.txt)"

VERSION_FULL="${VERSION}-${GIT_HASH}"
if [ -n "$IB_OS_ENV" ]; then
  VERSION_FULL="$VERSION_FULL~${IB_OS_ENV}"
fi

IMAGE_BASENAME=startos-${VERSION_FULL}_${IB_TARGET_PLATFORM}

BOOTLOADERS=grub-efi
if [ "$IB_TARGET_PLATFORM" = "x86_64" ] || [ "$IB_TARGET_PLATFORM" = "x86_64-nonfree" ]; then
	IB_TARGET_ARCH=amd64
	QEMU_ARCH=x86_64
	BOOTLOADERS=grub-efi,syslinux
elif [ "$IB_TARGET_PLATFORM" = "aarch64" ] || [ "$IB_TARGET_PLATFORM" = "aarch64-nonfree" ] || [ "$IB_TARGET_PLATFORM" = "raspberrypi" ]  || [ "$IB_TARGET_PLATFORM" = "rockchip64" ]; then
	IB_TARGET_ARCH=arm64
	QEMU_ARCH=aarch64
elif [ "$IB_TARGET_PLATFORM" = "riscv64" ]; then
	IB_TARGET_ARCH=riscv64
	QEMU_ARCH=riscv64
else
	IB_TARGET_ARCH="$IB_TARGET_PLATFORM"
	QEMU_ARCH="$IB_TARGET_PLATFORM"
fi

QEMU_ARGS=()
if [ "$QEMU_ARCH" != $(uname -m) ]; then
	QEMU_ARGS+=(--bootstrap-qemu-arch ${IB_TARGET_ARCH})
	QEMU_ARGS+=(--bootstrap-qemu-static /usr/bin/qemu-${QEMU_ARCH}-static)
fi

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
	else
		ARCHIVE_AREAS="$ARCHIVE_AREAS non-free non-free-firmware"
	fi
fi

PLATFORM_CONFIG_EXTRAS=()
if [ "${IB_TARGET_PLATFORM}" = "raspberrypi" ]; then
	PLATFORM_CONFIG_EXTRAS+=( --firmware-binary false )
	PLATFORM_CONFIG_EXTRAS+=( --firmware-chroot false )
	RPI_KERNEL_VERSION=6.12.47+rpt
	PLATFORM_CONFIG_EXTRAS+=( --linux-packages linux-image-$RPI_KERNEL_VERSION )
	PLATFORM_CONFIG_EXTRAS+=( --linux-flavours "rpi-v8 rpi-2712" )
elif [ "${IB_TARGET_PLATFORM}" = "rockchip64" ]; then
	PLATFORM_CONFIG_EXTRAS+=( --linux-flavours rockchip64 )
elif [ "${IB_TARGET_ARCH}" = "riscv64" ]; then
	PLATFORM_CONFIG_EXTRAS+=( --uefi-secure-boot=disable )
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
	--cache false \
	--mirror-bootstrap "https://deb.debian.org/debian/" \
	--mirror-chroot "https://deb.debian.org/debian/" \
	--mirror-chroot-security "https://security.debian.org/debian-security" \
	-d ${IB_SUITE} \
	-a ${IB_TARGET_ARCH} \
	${QEMU_ARGS[@]} \
	--archive-areas "${ARCHIVE_AREAS}" \
	${PLATFORM_CONFIG_EXTRAS[@]}

# Overlays

mkdir -p config/packages.chroot/
cp $RESULTS_DIR/$IMAGE_BASENAME.deb config/packages.chroot/
dpkg-name config/packages.chroot/*.deb

mkdir -p config/includes.chroot/etc
echo start > config/includes.chroot/etc/hostname
cat > config/includes.chroot/etc/hosts << EOT
127.0.0.1       localhost start
::1             localhost start ip6-localhost ip6-loopback
ff02::1         ip6-allnodes
ff02::2         ip6-allrouters
EOT

if [ "${IB_TARGET_PLATFORM}" = "raspberrypi" ]; then
	mkdir -p config/includes.chroot
	git clone --depth=1 --branch=stable https://github.com/raspberrypi/rpi-firmware.git config/includes.chroot/boot
	rm -rf config/includes.chroot/boot/.git config/includes.chroot/boot/modules
	rsync -rLp $SOURCE_DIR/raspberrypi/squashfs/ config/includes.chroot/
fi

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

cp $SOURCE_DIR/splash.png config/bootloaders/syslinux_common/splash.png
cp $SOURCE_DIR/splash.png config/bootloaders/isolinux/splash.png
cp $SOURCE_DIR/splash.png config/bootloaders/grub-pc/splash.png

sed -i -e '2i set timeout=5' config/bootloaders/grub-pc/config.cfg

# Archives

mkdir -p config/archives

if [ "${IB_TARGET_PLATFORM}" = "raspberrypi" ]; then
	curl -fsSL https://archive.raspberrypi.com/debian/raspberrypi.gpg.key | gpg --dearmor -o config/archives/raspi.key
	echo "deb [arch=${IB_TARGET_ARCH} signed-by=/etc/apt/trusted.gpg.d/raspi.key.gpg] https://archive.raspberrypi.com/debian/ ${IB_SUITE} main" > config/archives/raspi.list
fi

if [ "${IB_TARGET_PLATFORM}" = "rockchip64" ]; then
	curl -fsSL https://apt.armbian.com/armbian.key | gpg --dearmor -o config/archives/armbian.key
	echo "deb https://apt.armbian.com/ ${IB_SUITE} main" > config/archives/armbian.list
fi

curl -fsSL https://nvidia.github.io/libnvidia-container/gpgkey | gpg --dearmor -o config/archives/nvidia.key
curl -s -L https://nvidia.github.io/libnvidia-container/stable/deb/nvidia-container-toolkit.list | \
    sed 's#deb https://#deb [signed-by=/etc/apt/trusted.gpg.d/nvidia.key.gpg] https://#g' > config/archives/nvidia.list

cat > config/archives/backports.pref <<- EOF
Package: linux-image-*
Pin: release n=${IB_SUITE}-backports
Pin-Priority: 500
EOF

# Dependencies

## Firmware
cat > config/hooks/normal/9000-install-startos.hook.chroot << EOF
#!/bin/bash

set -e

cp /etc/resolv.conf /etc/resolv.conf.bak

if [ "${IB_SUITE}" = trixie ] && [ "${IB_TARGET_ARCH}" != riscv64 ]; then
	echo 'deb https://deb.debian.org/debian/ bookworm main' > /etc/apt/sources.list.d/bookworm.list
	apt-get update
	apt-get install -y postgresql-15
	rm /etc/apt/sources.list.d/bookworm.list
	apt-get update
	systemctl mask postgresql
fi

if [ "${IB_TARGET_PLATFORM}" = "raspberrypi" ]; then
	ln -sf /usr/bin/pi-beep /usr/local/bin/beep
	KERNEL_VERSION=${RPI_KERNEL_VERSION} sh /boot/config.sh > /boot/config.txt
	mkinitramfs -c gzip -o initrd.img-${RPI_KERNEL_VERSION}-rpi-v8 ${RPI_KERNEL_VERSION}-rpi-v8
	mkinitramfs -c gzip -o initrd.img-${RPI_KERNEL_VERSION}-rpi-2712 ${RPI_KERNEL_VERSION}-rpi-2712
fi

useradd --shell /bin/bash -G startos -m start9
echo start9:embassy | chpasswd
usermod -aG sudo start9
usermod -aG systemd-journal start9

echo "start9 ALL=(ALL:ALL) NOPASSWD: ALL" | sudo tee "/etc/sudoers.d/010_start9-nopasswd"

if [ "${IB_TARGET_PLATFORM}" != "raspberrypi" ]; then
	/usr/lib/startos/scripts/enable-kiosk
fi

if ! [[ "${IB_OS_ENV}" =~ (^|-)dev($|-) ]]; then
	passwd -l start9
fi

EOF

SOURCE_DATE_EPOCH="${SOURCE_DATE_EPOCH:-$(date '+%s')}"

if lb bootstrap; then
	true
else
	EXIT=$?
	cat ./chroot/debootstrap/debootstrap.log
	exit $EXIT
fi
lb chroot
lb installer
lb binary_chroot
lb chroot_prep install all mode-apt-install-binary mode-archives-chroot
mv chroot/chroot/etc/resolv.conf.bak chroot/chroot/etc/resolv.conf
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

	SECTOR_LEN=512
	BOOT_START=$((1024 * 1024)) # 1MiB
	BOOT_LEN=$((512 * 1024 * 1024)) # 512MiB
	BOOT_END=$((BOOT_START + BOOT_LEN - 1))
	ROOT_START=$((BOOT_END + 1))
	ROOT_LEN=$((MAX_IMG_LEN - ROOT_START))
	ROOT_END=$((MAX_IMG_LEN - 1))

	TARGET_NAME=$prep_results_dir/${IMAGE_BASENAME}.img
	truncate -s $MAX_IMG_LEN $TARGET_NAME

	sfdisk $TARGET_NAME <<-EOF
		label: dos
		label-id: 0xcb15ae4d
		unit: sectors
		sector-size: 512

		${TARGET_NAME}1 : start=$((BOOT_START / SECTOR_LEN)), size=$((BOOT_LEN / SECTOR_LEN)), type=c, bootable
		${TARGET_NAME}2 : start=$((ROOT_START / SECTOR_LEN)), size=$((ROOT_LEN / SECTOR_LEN)), type=83
	EOF

	BOOT_DEV=$(losetup --show -f --offset $BOOT_START --sizelimit $BOOT_LEN $TARGET_NAME)
	ROOT_DEV=$(losetup --show -f --offset $ROOT_START --sizelimit $ROOT_LEN $TARGET_NAME)

	mkfs.vfat -F32 $BOOT_DEV
	mkfs.ext4 $ROOT_DEV

	TMPDIR=$(mktemp -d)

	mkdir -p $TMPDIR/boot $TMPDIR/root
	mount $ROOT_DEV $TMPDIR/root
	mount $BOOT_DEV $TMPDIR/boot
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
		rsync -a $SOURCE_DIR/raspberrypi/img/ $TMPDIR/next/
	fi

	umount $TMPDIR/next
	umount $TMPDIR/lower

	umount $TMPDIR/boot
	umount $TMPDIR/root


	e2fsck -fy $ROOT_DEV
	resize2fs -M $ROOT_DEV

	BLOCK_COUNT=$(dumpe2fs -h $ROOT_DEV | awk '/^Block count:/ { print $3 }')
	BLOCK_SIZE=$(dumpe2fs -h $ROOT_DEV | awk '/^Block size:/ { print $3 }')
	ROOT_LEN=$((BLOCK_COUNT * BLOCK_SIZE))

	losetup -d $ROOT_DEV
	losetup -d $BOOT_DEV

	# Recreate partition 2 with the new size using sfdisk
	sfdisk $TARGET_NAME <<-EOF
		label: dos
		label-id: 0xcb15ae4d
		unit: sectors
		sector-size: 512

		${TARGET_NAME}1 : start=$((BOOT_START / SECTOR_LEN)), size=$((BOOT_LEN / SECTOR_LEN)), type=c, bootable
		${TARGET_NAME}2 : start=$((ROOT_START / SECTOR_LEN)), size=$((ROOT_LEN / SECTOR_LEN)), type=83
	EOF

	TARGET_SIZE=$((ROOT_START + ROOT_LEN))
	truncate -s $TARGET_SIZE $TARGET_NAME

	mv $TARGET_NAME $RESULTS_DIR/$IMAGE_BASENAME.img

fi

chown $IB_UID:$IB_UID $RESULTS_DIR/$IMAGE_BASENAME.*