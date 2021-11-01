# Building Embassy OS

 The commands given assume a Debian or Ubuntu-based environment.

## Prerequisites

1. Install dependences

- Avahi
	- `sudo apt install -y avahi-daemon`
	- Installed by default on most Debian systems - https://avahi.org
- Docker
	- `sudo apt install -y docker.io`
	- https://docs.docker.com/get-docker
	- Add your user to the docker group: `sudo usermod -a -G docker $USER`
	- If you have issues here, try to logout or reboot your system
- Prepare Docker environment
	- Setup buildx (https://docs.docker.com/buildx/working-with-buildx/)
	- Create a builder: `docker buildx create --use`
	- Add multi-arch build ability: `docker run --rm --privileged linuxkit/binfmt:v0.8`
- Node
	- `sudo apt install -y nodejs`
	- https://nodejs.org/en/docs
- NPM
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
