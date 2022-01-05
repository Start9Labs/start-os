# Building Embassy OS

 ⚠️ The commands given assume a Debian or Ubuntu-based environment.  *Building in a VM is NOT yet supported* ⚠️

## Prerequisites

1. Install dependences

- Avahi
	- `sudo apt install -y avahi-daemon`
	- Installed by default on most Debian systems - https://avahi.org
- Docker
	- `sudo apt install -y docker.io`
	- https://docs.docker.com/get-docker
	- Add your user to the docker group: `sudo usermod -a -G docker $USER`
	- Reload user environment `exec sudo su -l $USER`
- Prepare Docker environment
	- Setup buildx (https://docs.docker.com/buildx/working-with-buildx/)
	- Create a builder: `docker buildx create --use`
	- Add multi-arch build ability: `docker run --rm --privileged linuxkit/binfmt:v0.8`
- Node Version 12+
	- `sudo snap install node`
	- https://nodejs.org/en/docs
- NPM Version 7+
	- `sudo apt install -y npm`
	- https://docs.npmjs.com/downloading-and-installing-node-js-and-npm
- jq
	- `sudo apt install -y jq`
	- https://stedolan.github.io/jq

2. Clone the repo, move into it, and bring in required submodules

	```
	git clone --recursive https://github.com/Start9Labs/embassy-os.git
	cd embassy-os
	git submodule update --init --recursive
	```

## Build

```
make
```

## Flash

Flash the resulting `eos.img` to your SD Card (16GB required, any larger is neither necessary, nor advantageous)

We recommend [Balena Etcher](https://www.balena.io/etcher/)

## Setup

Visit http://embassy.local from any web browser

We recommend [Firefox](https://www.mozilla.org/firefox/browsers)

## Troubleshooting

1. I just flashed my SD card, fired up my Embassy, bootup sounds and all, but my browser is saying "Unable to connect" with embassy.local.

- Try doing a hard refresh on your browser, or opening the url in a private/incognito window. If you've ran an instance 
  of Embassy before, sometimes you can have a stale cache that will block you from navigating to the page.

2. Flashing the image isn't working with balenaEtcher. I'm getting `Cannot read property 'message' of null` when I try.
- The latest versions of Balena may not flash properly. This version here: https://github.com/balena-io/etcher/releases/tag/v1.5.122 should work properly.

3. Startup isn't working properly and I'm curious as to why. How can I view logs regarding startup for debugging? 
- During the Build step, instead of running just `make` run `ENVIRONMENT=dev make`. Flash like normal, and insert into your Embassy. Boot up your Embassy, and on another computer
on the same network, ssh into the Embassy with the username/password `ubuntu`.  After logging in and changing the password, run `journalctl -u initialization.service -ef` to view the start up logs.

4. I need to reset my password, how can I do that?
- At the time of writing, there is no way to do that in 0.3.0 cleanly. You'll need to reflash your device unfortunately.
