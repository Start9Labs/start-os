APPMGR_SRC := $(shell find appmgr/src) appmgr/Cargo.toml appmgr/Cargo.lock
LIFELINE_SRC := $(shell find lifeline/src) lifeline/Cargo.toml lifeline/Cargo.lock
AGENT_SRC := $(shell find agent/src) $(shell find agent/config) agent/stack.yaml agent/package.yaml agent/build.sh
UI_SRC := $(shell find ui/src) \
			ui/angular.json \
			ui/browserslist \
			ui/client-manifest.yaml \
			ui/ionic.config.json \
			ui/postprocess.ts \
			ui/tsconfig.json \
			ui/tslint.json \
			ui/use-mocks.json

all: embassy.img

embassy.img: buster.img product_key appmgr/target/armv7-unknown-linux-musleabihf/release/appmgr ui/www agent/dist/agent agent/config/agent.service lifeline/target/armv7-unknown-linux-gnueabihf/release/lifeline lifeline/lifeline.service setup.sh setup.service docker-daemon.json
	sudo ./make_image.sh

buster.img:
	wget -O buster.zip https://downloads.raspberrypi.org/raspios_lite_armhf/images/raspios_lite_armhf-2020-08-24/2020-08-20-raspios-buster-armhf-lite.zip
	unzip buster.zip
	rm buster.zip
	mv 2020-08-20-raspios-buster-armhf-lite.img buster.img

product_key:
	echo "X\c" > product_key
	cat /dev/random | base32 | head -c11 | tr '[:upper:]' '[:lower:]' >> product_key

appmgr/target/armv7-unknown-linux-gnueabihf/release/appmgr: $(APPMGR_SRC)
	docker run --rm -it -v ~/.cargo/registry:/root/.cargo/registry -v "$(shell pwd)":/home/rust/src start9/rust-arm-cross:latest sh -c "(cd appmgr && cargo build --release --features=production)"
	docker run --rm -it -v ~/.cargo/registry:/root/.cargo/registry -v "$(shell pwd)":/home/rust/src start9/rust-arm-cross:latest arm-linux-gnueabi-strip appmgr/target/armv7-unknown-linux-gnueabihf/release/appmgr

appmgr: appmgr/target/armv7-unknown-linux-gnueabihf/release/appmgr

agent/dist/agent: $(AGENT_SRC)
	(cd agent && ./build.sh)

agent: agent/dist/agent

ui/node_modules: ui/package.json
	npm --prefix ui install

ui/www: $(UI_SRC) ui/node_modules
	npm --prefix ui run build-prod

ui: ui/www

lifeline/target/armv7-unknown-linux-gnueabihf/release/lifeline: $(LIFELINE_SRC)
	docker run --rm -it -v ~/.cargo/registry:/root/.cargo/registry -v "$(shell pwd)":/home/rust/src start9/rust-arm-cross:latest sh -c "(cd lifeline && cargo build --release)"
	docker run --rm -it -v ~/.cargo/registry:/root/.cargo/registry -v "$(shell pwd)":/home/rust/src start9/rust-arm-cross:latest arm-linux-gnueabi-strip lifeline/target/armv7-unknown-linux-gnueabihf/release/lifeline

lifeline: lifeline/target/armv7-unknown-linux-gnueabihf/release/lifeline

