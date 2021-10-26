# Building Embassy OS

## Prerequisites

1. Install dependences - The commands given assume a Debian or Ubuntu-based environment

- Rust
	`curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`
	https://rustup.rs
- Avahi
	`apt install avahi`
	Installed by default on most Debian systems - https://avahi.org
- Docker
	`apt install docker.io`
	https://docs.docker.com/get-docker
- Node
	`apt install nodejs`
	https://nodejs.org/en/docs
- NPM
	`apt install npm`
	https://docs.npmjs.com/downloading-and-installing-node-js-and-npm
- jq
	`apt install jq`
	https://stedolan.github.io/jq

2. Clone the repo and move into it

```
git clone --recursive https://github.com/Start9Labs/embassy-os.git`
cd embassy-os
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

We recommend [Firefox](https://www.mozilla.org/en-US/firefox/browsers
