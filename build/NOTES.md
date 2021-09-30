# Creating an 0.3.0 Image

- Download [ubuntu-21.04-preinstalled-server-arm64+raspi.img.xz](https://ubuntu.com/download/raspberry-pi/thank-you?version=21.04&architecture=server-arm64+raspi)

- `unxz ubuntu-21.04-preinstalled-server-arm64+raspi.img.xz` to unzip

- `export LOOPDEV=$(sudo losetup --show -fP ubuntu-21.04-preinstalled-server-arm64+raspi.img)` to set the `.img` file as a loop device environment variable

	- Got error: `losetup: cannot find an unused loop device`
	- ran `modprobe loop` which gave: `modprobe: FATAL: Module loop not found in directory /lib/modules/5.13.13-arch1-1`
	- Fixed with reboot (an update had occurred without recent reboot)

- Plug in your 16GB microSD card. We are assuming it will be at /dev/mmcblk0

	- Find current devices with `lsblk`

(To use `partitioning.sh` - `export OUTPUT_DEVICE=mmcblk0` where mmcblk0 is our example, but be sure to use the correct disc name if yours differs)

- `sudo fdisk /dev/mmcblk0`

	- Use entire disk, not a partition (e.g. `mmcblk0`, not `mmcblk0p1`)

- Run the following (options to ‘press Enter’ are assuming a 16GB card): 

	-`o`, to create a new empty DOS partition table
	-`n`, to add a new partition
	-`p` (or press Enter), to make it a primary partition
	-`1` (or press Enter), to make it the first partition
	-`2048` (or press Enter), to set the first sector
	-`526335`, to set the last sector
	-`t`, to change the partition type
	-`c`, to select `W95 FAT32 (LBA)`
	-`n`, to add a new partition
	-`p` (or press Enter), to make it a primary partition
	-`2` (or press Enter), to make it the second partition
	-`526336` (or press Enter), to set the first sector
	-`1050623`, to set the last sector
	-`t`, to change the partition type
	-`2`, to select the partition
	-`c`, to select `W95 FAT32 (LBA)`
	-`n`, to add a new partition
	-`p` (or press Enter), to make it a primary partition
	-`3` (or press Enter), to make it the third partition
	-`1050624` (or press Enter), to set the first sector
	-`16083455`, to set the last sector
	-`n`, to add a new partition
	-`p`, to make it a primary partition
	-`16083456`, to set the first sector
	- `31116287` (or press Enter), to set the last sector
	-`a`, to toggle the bootable flag
	-`1`, to select the first partition
	-`w`, to save and write the changes to the card

		- Got errors `Failed to add partition 1 (and 2,3,4) to system: Device or resource busy
		- Had to `sudo umount /dev/mmcblk0p1` then
		- `sudo fdisk /dev/mmcblk0` then `w` to write
		- Fucking Arch

*******************************************

(Can use `filesystems.sh` here)

- `sudo dd if=${LOOPDEV}p1 of=/dev/mmcblk0p1` to write partition 1 of the Ubuntu image to the partition 1 of the sd card
	
	- You may add ` status=progress` if you’d like progress feedback

- `sudo mkfs.vfat /dev/mmcblk0p2` to make the FAT filesystem on the partition 2

- `sudo dd if=${LOOPDEV}p2 of=/dev/mmcblk0p3` to write partition 2 of the Ubuntu image to partition 3 of the sd card

- `sudo mkfs.ext4 /dev/mmcblk0p4` to make the linux filesystem on partition 4

- `sudo losetup -d $LOOPDEV` to detach the loop device

- Now, we will label the filesystems:

	- `sudo fatlabel /dev/mmcblk0p1 system-boot` Ignore warning

	- `sudo fatlabel /dev/mmcblk0p2 EMBASSY`

	- `sudo e2label /dev/mmcblk0p3 writable`

	- `sudo e2label /dev/mmcblk0p4 reserved`

- `sudo mount /dev/mmcblk0p1 /mnt` to mount the boot partition

- `cat "/mnt/config.txt" | grep -v "dtoverlay=" | sudo tee "/mnt/config.txt.tmp"` This copies everything from the `config.txt` file except the dtoverlay option into the `config.txt.tmp` file

- `echo "dtoverlay=pwm-2chan" | sudo tee -a "/mnt/config.txt.tmp"` This writes a dtoverlay option into the `config.txt.tmp` file

- `sudo mv "/mnt/config.txt.tmp" "/mnt/config.txt"` Overwrites the `config.txt` file with the new input

- `sudo umount /mnt` to unmount the filesystem

- `sudo mount /dev/mmcblk0p2 /mnt` Mount the EMBASSY filesystem

***

- Store a product key as an environment variable in $PRODUCT_KEY, with `export PRODUCT_KEY=123456`, obviously, this number is made up, and then:

- `echo $PRODUCT_KEY | sudo tee /mnt/product_key.txt` to add it to the `product_key.txt` file.

- `sudo umount /mnt` to unmount again

- `sudo mount /dev/mmcblk0p3 /mnt` to mount the writable filesystem

- Build embassy-os (LINK OR UPDATE, this step ridic – PULL LATEST CODE!!!) (for now, `docker run --rm --privileged linuxkit/binfmt:v0.8`, get rust-arm-cross.img and `docker load < rust-arm-cross.img`, have latest dev branch for patch, yajrc, and master for rpc-toolkit, then from appmgr dir: `./build-prod.sh`), then, also from the appmgr dir:

(Can use `copy.sh` here)

	- `sudo cp target/aarch64-unknown-linux-gnu/release/embassy-init /mnt/usr/local/bin` to copy `embassy-init` over to the new filesystem

	- `sudo cp target/aarch64-unknown-linux-gnu/release/embassyd /mnt/usr/local/bin` to copy `embassyd` over to the new filesystem

	- `sudo cp target/aarch64-unknown-linux-gnu/release/embassy-cli /mnt/usr/local/bin` to copy `embassy-cli` over to the new filesystem

	- `sudo cp *.service /mnt/etc/systemd/system/` to copy over the systemd service files

	- `echo "application/wasm		wasm;" | sudo tee -a "/mnt/etc/nginx/mime.types"`

	- `sudo mkdir -p /mnt/root/.ssh` create the `ssh` folder on the new filesystem

*******************************************

- `cat ~/.ssh/id_ed25519.pub | sudo tee -a /mnt/root/.ssh/authorized_keys` copy your ssh key over (assuming it is ~/.ssh/id_ed25519.pub)

- `sudo umount /mnt` unmount once again

Time to remove your SD card and insert it into your hardware!!  See our DIY guide here (LINK REQUIRED) if you have not yet built your Embassy.

- SSH in (find by hacking) `ssh root@whateverIP` and run:

(Can use `/setup/initialization.sh` here)

```sh
#!/bin/bash

apt update
apt install -y \
  docker.io \
  tor \
  nginx \
  libavahi-client3 \
  avahi-daemon \
  iotop \
  bmon \
  zfsutils-linux \
  exfat-utils \
  sqlite3
sed -i 's/"1"/"0"/g' /etc/apt/apt.conf.d/20auto-upgrades
sed -i 's/Restart=on-failure/Restart=always/g' /lib/systemd/system/tor@default.service
docker run --privileged --rm tonistiigi/binfmt --install all
docker network create -d bridge --subnet 172.18.0.1/16 start9
echo '{ "storage-driver": "zfs" }' > /etc/docker/daemon.json
mkdir /etc/embassy
hostnamectl set-hostname "embassy"
systemctl enable embassyd.service embassy-init.service
echo 'overlayroot="tmpfs"' > /etc/overlayroot.local.conf
cat << EOF > /etc/tor/torrc
SocksPort 0.0.0.0:9050
SocksPolicy accept 127.0.0.1
SocksPolicy accept 172.18.0.0/16
SocksPolicy reject *
ControlPort 9051
CookieAuthentication 1
EOF
```

*******************************************

- Do a `reboot`

- Have Aiden fix code for a while

Do the setup!!

- 

- Then initialize ZFS (don’t actually though):

```sh
#!/bin/bash

zpool create embassy-data /dev/sda
zpool get -H -ovalue guid embassy-data > /embassy-os/disk.guid
echo password > /etc/embassy/password
zfs create -o reservation=5G -o encryption=on -o keylocation=file:///etc/embassy/password -o keyformat=passphrase embassy-data/main
zfs create -o reservation=5G embassy-data/updates
zfs create -o encryption=on -o keylocation=file:///etc/embassy/password -o keyformat=passphrase embassy-data/package-data
zfs create -o encryption=on -o keylocation=file:///etc/embassy/password -o keyformat=passphrase embassy-data/tmp
rm /etc/embassy/password
zpool export embassy-data
```

- Enable services
- `systemctl enable embassyd.service embassy-init.service`
- Reboot
- `reboot`
- Once you have rebooted in overlay mode, DO NOT INSTALL ANYTHING
- Then seed the db with an account:
- `sqlite3 /embassy-data/secrets.db`
- `insert into account (id, password, tor_key) VALUES (0, '[your_password_hash]', x'[your_tor_key]');`
- You should hear a chime
- Now go to your onion address and enjoy


Automation:

1. partitioning.sh - done
2. filesystems.sh - done
3. copy.sh - done
4. initialization.sh - done
5. zfs.sh??
6. Final steps?
