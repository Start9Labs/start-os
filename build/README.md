# Creating an 0.3.0 Image (Scripted version)
### This guide assumes a linux environment
### `Text in this format are commands to run`

1. Download [ubuntu-21.04-preinstalled-server-arm64+raspi.img.xz](https://ubuntu.com/download/raspberry-pi/thank-you?version=21.04&architecture=server-arm64+raspi)

1a. `unxz ubuntu-21.04-preinstalled-server-arm64+raspi.img.xz` to unzip

1b. `export LOOPDEV=$(sudo losetup --show -fP ubuntu-21.04-preinstalled-server-arm64+raspi.img)` to set the `.img` file as a loop device environment variable

2. Run `git clone https://github.com/Start9Labs/embassy-os` and then `cd embassy-os` to download and enter the embassy-os repository directory

2a. Plug in your 16GB microSD card. We are assuming it will be at /dev/mmcblk0

	- Find current devices with `lsblk`

3. Run `export OUTPUT_DEVICE=mmcblk0` where mmcblk0 is the sd card’s device name, be sure to change if yours differs

4. Run `sudo ./setup/partitioning.sh` You should see confirmation of write to disk

5. Run `sudo ./setup/filesystems.sh` You will see write progression twice, ignore the warning about lowercase labels

6. Store a product key as an environment variable in $PRODUCT_KEY, with `export PRODUCT_KEY=123456`, obviously, this number is made up, and then:

6a. `echo $PRODUCT_KEY | sudo tee /mnt/product_key.txt` to add it to the `product_key.txt` file.

6b. `sudo umount /mnt` to unmount again

6c. `sudo mount /dev/mmcblk0p3 /mnt` to mount the writable filesystem

7. Build embassy-os (LINK OR UPDATE, this step ridic – PULL LATEST CODE!!!) (for now, `docker run --rm --privileged linuxkit/binfmt:v0.8`, get rust-arm-cross.img and `docker load < rust-arm-cross.img`, have latest dev branch for patch, yajrc, and master for rpc-toolkit, then from appmgr dir: `./build-prod.sh`)

8. Run `sudo ./setup/copy.sh`

8a. `cat ~/.ssh/id_ed25519.pub | sudo tee -a /mnt/root/.ssh/authorized_keys` copy your ssh key over (assuming it is ~/.ssh/id_ed25519.pub)

8b. `sudo umount /mnt` unmount once again

## Time to remove your SD card and insert it into your hardware!!  See our DIY guide here (LINK REQUIRED) if you have not yet built your Embassy.

9 SSH in (find by hacking) `ssh root@whateverIP` and run `sudo ./setup/initialization.sh` which you currently have to scp over from embassy-os dir

9a. Run `reboot`

10. Do the setup
