# Top-level orchestrator. Shared vars/macros live in build/common.mk; each
# product owns its targets in <project>/build.mk. One make process, one DAG —
# includes (not recursive make) so cross-project prerequisites resolve.
.DEFAULT_GOAL := all

include build/common.mk
include shared-libs/crates/start-core/build.mk
include shared-libs/ts-modules/build.mk
include projects/start-sdk/build.mk
include projects/start-cli/build.mk
include projects/start-registry/build.mk
include projects/start-tunnel/build.mk
include projects/start-os/build.mk
include projects/start-docs/build.mk

.PHONY: all startos metadata install-startos clean format format-check install-cli cli cli-deb uis ui startos-emulate-reflash startos-deb startos-$(IMAGE_TYPE) startos-squashfs startos-wormhole startos-wormhole-deb startos-update test test-core test-sdk test-container-runtime registry install-registry tunnel install-tunnel ts-bindings

all: startos

touch:
	touch $(STARTOS_TARGETS)

metadata: $(VERSION_FILE) $(PLATFORM_FILE) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE)

clean:
	rm -rf target
	rm -rf shared-libs/crates/start-core/bindings
	rm -rf .angular
	rm -f config.json
	rm -rf node_modules
	rm -rf projects/start-os/web/dist
	rm -rf projects/start-tunnel/web/dist
	rm -rf projects/brochure-marketplace/dist
	rm -rf shared-libs/crates/patch-db/client/node_modules
	rm -rf shared-libs/crates/patch-db/client/dist
	rm -rf shared-libs/crates/patch-db/target
	rm -rf target
	rm -rf dpkg-workdir
	rm -rf image-recipe/deb
	rm -rf results
	rm -rf projects/start-os/build/lib/firmware
	rm -rf projects/start-os/container-runtime/dist
	rm -rf projects/start-os/container-runtime/node_modules
	rm -f projects/start-os/container-runtime/*.squashfs
	(cd projects/start-sdk && make clean)
	rm -rf projects/start-os/build/lib/migration-images
	rm -f env/*.txt

format:
	cd shared-libs/crates/start-core && cargo +nightly fmt
	npm --prefix . run format
	cd projects/start-sdk && make fmt

# Read-only formatting verification (prettier --check for web + sdk). Used by CI.
format-check:
	npm --prefix . run format:check
	cd projects/start-sdk && make check-fmt

test: | test-core test-sdk test-container-runtime
