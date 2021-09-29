# Creating an 0.3.0 Image (Scripted version)
### This guide assumes a linux environment
### `Text in this format are commands to run`

1. a)  Download [ubuntu-21.04-preinstalled-server-arm64+raspi.img.xz](https://ubuntu.com/download/raspberry-pi/thank-you?version=21.04&architecture=server-arm64+raspi)

1. b) `unxz ubuntu-21.04-preinstalled-server-arm64+raspi.img.xz` to unzip

2. a) Run `git clone https://github.com/Start9Labs/embassy-os` to download the embassy-os repository

2. b)  Plug in your 16GB microSD card. In this example, we are assuming it will be at /dev/mmcblk0

	- Find the location of your device with `lsblk` or `fdisk -l`

3. a) Run `export OUTPUT_DEVICE=/dev/mmcblk0` where `/dev/mmcblk0` is the sd card’s device name, be sure to change if yours differs

3. b) Run `export LOOPDEV=$(sudo losetup --show -fP ubuntu-21.04-preinstalled-server-arm64+raspi.img)` to set the `.img` file as a loop device environment variable

4. Run `./build/partitioning.sh` You should see confirmation of write to disk

5. Run `./build/filesystems.sh` You will see write progression twice, ignore the warning about lowercase labels

6. a) Store a product key as an environment variable in $PRODUCT_KEY, with `export PRODUCT_KEY=test1234`, obviously, this number is made up, but must be 8 alphanumeric characters, then:

6. b) `echo $PRODUCT_KEY | sudo tee /mnt/product_key.txt` to add it to the `product_key.txt` file.

6. c) `sudo umount /mnt` to unmount again

6. d) `sudo mount /dev/mmcblk0p3 /mnt` to mount the writable filesystem

7. a) Build EmbassyOS: Move into the EmbassyOS directory with `cd embassy-os` and Build embassy-os (NEEDS UPDATE – PULL LATEST CODE!!!) (for now, `docker run --rm --privileged linuxkit/binfmt:v0.8`, get rust-arm-cross.img and `docker load < rust-arm-cross.img`, then from appmgr dir: `./build-prod.sh`)

7. b) Build UI: First, make sure you have `git`, `node`, and `npm` installed.  Then, `cd ui` to enter ui dir, and run `npm i -g @ionic/cli` to install Ionic, `npm i` to install, then `cd ..` to return to the `embassy-os` directory

8. a) Run `sudo ./build/copy.sh`

8. b) `cat ~/.ssh/id_ed25519.pub | sudo tee -a /mnt/root/.ssh/authorized_keys` copy your ssh key over (assuming it is ~/.ssh/id_ed25519.pub)

8. c) `sudo umount /mnt` unmount once again

## Time to remove your SD card and insert it into your hardware!!  See our [DIY guide](https://docs.start9.com/getting-started/diy.html) if you have not yet put built your device.

9. a) Get IP (find by hacking) and visit x.x.x.x:80
9. b) `ssh root@whateverIP` and run `sudo ./build/initialization.sh` which you currently have to scp over from the `embassy-os` dir

10. Do the setup
