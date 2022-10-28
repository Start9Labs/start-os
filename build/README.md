# Building Embassy OS

⚠️ The commands given assume a Debian or Ubuntu-based environment. _Building in
a VM is NOT yet supported_ ⚠️

## Prerequisites

1. Install dependencies

- Avahi
  - `sudo apt install -y avahi-daemon`
  - Installed by default on most Debian systems - https://avahi.org
- Build Essentials (needed to run `make`)
  - `sudo apt install -y build-essential`
- Docker
  - `curl -fsSL https://get.docker.com | sh`
  - https://docs.docker.com/get-docker
  - Add your user to the docker group: `sudo usermod -a -G docker $USER`
  - Reload user environment `exec sudo su -l $USER`
- Prepare Docker environment
  - Setup buildx (https://docs.docker.com/buildx/working-with-buildx/)
  - Create a builder: `docker buildx create --use`
  - Add multi-arch build ability:
    `docker run --rm --privileged linuxkit/binfmt:v0.8`
- Node Version 12+
  - snap: `sudo snap install node`
  - [nvm](https://github.com/nvm-sh/nvm#installing-and-updating):
    `nvm install --lts`
  - https://nodejs.org/en/docs
- NPM Version 7+
  - apt: `sudo apt install -y npm`
  - [nvm](https://github.com/nvm-sh/nvm#installing-and-updating):
    `nvm install --lts`
  - https://docs.npmjs.com/downloading-and-installing-node-js-and-npm
- jq
  - `sudo apt install -y jq`
  - https://stedolan.github.io/jq
- yq
  - snap: `sudo snap install yq`
  - binaries: https://github.com/mikefarah/yq/releases/
  - https://mikefarah.gitbook.io/yq

2. Clone the latest repo with required submodules
   > :information_source: You chan check latest available version
   > [here](https://github.com/Start9Labs/embassy-os/releases)
   ```
   git clone --recursive https://github.com/Start9Labs/embassy-os.git --branch latest
   ```

## Build Raspberry Pi Image

```
cd embassy-os
make embassyos-raspi.img ARCH=aarch64
```

## Flash

Flash the resulting `embassyos-raspi.img` to your SD Card

We recommend [Balena Etcher](https://www.balena.io/etcher/)

## Setup

Visit http://embassy.local from any web browser - We recommend
[Firefox](https://www.mozilla.org/firefox/browsers)

Enter your product key. This is generated during the build process and can be
found in `product_key.txt`, located in the root directory.

## Troubleshooting

1. I just flashed my SD card, fired up my Embassy, bootup sounds and all, but my
   browser is saying "Unable to connect" with embassy.local.

- Try doing a hard refresh on your browser, or opening the url in a
  private/incognito window. If you've ran an instance of Embassy before,
  sometimes you can have a stale cache that will block you from navigating to
  the page.

2. Flashing the image isn't working with balenaEtcher. I'm getting
   `Cannot read property 'message' of null` when I try.

- The latest versions of Balena may not flash properly. This version here:
  https://github.com/balena-io/etcher/releases/tag/v1.5.122 should work
  properly.

3. Startup isn't working properly and I'm curious as to why. How can I view logs
   regarding startup for debugging?

- Find the IP of your device
- Run `nc <ip> 8080` and it will print the logs

4. I need to ssh into my Embassy to fix something, but I cannot get to the
   console to add ssh keys normally.

- During the Build step, instead of running just
  `make embassyos-raspi.img ARCH=aarch64` run
  `ENVIRONMENT=dev make embassyos-raspi.img ARCH=aarch64`. Flash like normal,
  and insert into your Embassy. Boot up your Embassy, and on another computer on
  the same network, ssh into the Embassy with the username `start9` password
  `embassy`.

4. I need to reset my password, how can I do that?

- You will need to reflash your device. Select "Use Existing Drive" once you are
  in setup, and it will prompt you to set a new password.
