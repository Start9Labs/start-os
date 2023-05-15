#!/bin/bash

get_variables () {
  ROOT_PART_DEV=$(findmnt / -o source -n)
  ROOT_PART_NAME=$(echo "$ROOT_PART_DEV" | cut -d "/" -f 3)
  ROOT_DEV_NAME=$(echo /sys/block/*/"${ROOT_PART_NAME}" | cut -d "/" -f 4)
  ROOT_DEV="/dev/${ROOT_DEV_NAME}"
  ROOT_PART_NUM=$(cat "/sys/block/${ROOT_DEV_NAME}/${ROOT_PART_NAME}/partition")

  BOOT_PART_DEV=$(findmnt /boot -o source -n)
  BOOT_PART_NAME=$(echo "$BOOT_PART_DEV" | cut -d "/" -f 3)
  BOOT_DEV_NAME=$(echo /sys/block/*/"${BOOT_PART_NAME}" | cut -d "/" -f 4)
  BOOT_PART_NUM=$(cat "/sys/block/${BOOT_DEV_NAME}/${BOOT_PART_NAME}/partition")

  OLD_DISKID=$(fdisk -l "$ROOT_DEV" | sed -n 's/Disk identifier: 0x\([^ ]*\)/\1/p')

  ROOT_DEV_SIZE=$(cat "/sys/block/${ROOT_DEV_NAME}/size")
  if [ "$ROOT_DEV_SIZE" -le 67108864 ]; then
      TARGET_END=$((ROOT_DEV_SIZE - 1))
  else
      TARGET_END=$((33554432 - 1))
      DATA_PART_START=33554432
      DATA_PART_END=$((ROOT_DEV_SIZE - 1))
  fi

  PARTITION_TABLE=$(parted -m "$ROOT_DEV" unit s print | tr -d 's')

  LAST_PART_NUM=$(echo "$PARTITION_TABLE" | tail -n 1 | cut -d ":" -f 1)

  ROOT_PART_LINE=$(echo "$PARTITION_TABLE" | grep -e "^${ROOT_PART_NUM}:")
  ROOT_PART_START=$(echo "$ROOT_PART_LINE" | cut -d ":" -f 2)
  ROOT_PART_END=$(echo "$ROOT_PART_LINE" | cut -d ":" -f 3)
}

check_variables () {
  if [ "$BOOT_DEV_NAME" != "$ROOT_DEV_NAME" ]; then
      FAIL_REASON="Boot and root partitions are on different devices"
      return 1
  fi

  if [ "$ROOT_PART_NUM" -ne "$LAST_PART_NUM" ]; then
    FAIL_REASON="Root partition should be last partition"
    return 1
  fi

  if [ "$ROOT_PART_END" -gt "$TARGET_END" ]; then
    FAIL_REASON="Root partition runs past the end of device"
    return 1
  fi

  if [ ! -b "$ROOT_DEV" ] || [ ! -b "$ROOT_PART_DEV" ] || [ ! -b "$BOOT_PART_DEV" ] ; then
    FAIL_REASON="Could not determine partitions"
    return 1
  fi
}

main () {
  get_variables

  if ! check_variables; then
    return 1
  fi

#   if [ "$ROOT_PART_END" -eq "$TARGET_END" ]; then
#     reboot_pi
#   fi

  if ! echo Yes | parted -m --align=optimal "$ROOT_DEV" ---pretend-input-tty u s resizepart "$ROOT_PART_NUM" "$TARGET_END" ; then
    FAIL_REASON="Root partition resize failed"
    return 1
  fi

  if [ -n "$DATA_PART_START" ]; then
    if ! parted -ms --align=optimal "$ROOT_DEV" u s mkpart primary "$DATA_PART_START" "$DATA_PART_END"; then
      FAIL_REASON="Data partition creation failed"
      return 1
    fi
  fi

  (
    echo x
    echo i
    echo "0xcb15ae4d"
    echo r
    echo w
  ) | fdisk $ROOT_DEV

  mount / -o remount,rw

  resize2fs $ROOT_PART_DEV

  if ! systemd-machine-id-setup; then
    FAIL_REASON="systemd-machine-id-setup failed"
    return 1
  fi

  if ! ssh-keygen -A; then
    FAIL_REASON="ssh host key generation failed"
    return 1
  fi

  echo start > /etc/hostname

  return 0
}

mount -t proc proc /proc
mount -t sysfs sys /sys
mount -t tmpfs tmp /run
mkdir -p /run/systemd
mount /boot
mount / -o remount,ro

beep

if main; then
  sed -i 's| init=/usr/lib/embassy/scripts/init_resize\.sh| boot=embassy|' /boot/cmdline.txt
  echo "Resized root filesystem. Rebooting in 5 seconds..."
  sleep 5
else
  echo -e "Could not expand filesystem.\n${FAIL_REASON}"
  sleep 5
fi

sync

umount /boot

reboot -f
