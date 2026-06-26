# Top-level orchestrator. Shared vars/macros live in build/common.mk; each
# product owns its targets in <project>/build.mk. One make process, one DAG —
# includes (not recursive make) so cross-project prerequisites resolve.
.DEFAULT_GOAL := all

include build/common.mk
include shared/crates/start-core/build.mk
include shared/web/build.mk
include start-sdk/build.mk
include start-cli/build.mk
include start-registry/build.mk
include start-tunnel/build.mk
include start-os/build.mk
include docs/build.mk

.PHONY: all startos metadata install-startos clean format format-check install-cli cli uis ui startos-emulate-reflash startos-deb startos-$(IMAGE_TYPE) startos-squashfs startos-wormhole startos-wormhole-deb startos-update test test-core test-sdk test-container-runtime registry install-registry tunnel install-tunnel ts-bindings

all: startos

touch:
	touch $(STARTOS_TARGETS)

metadata: $(VERSION_FILE) $(PLATFORM_FILE) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE)

clean:
	rm -rf target
	rm -rf shared/crates/start-core/bindings
	rm -rf .angular
	rm -f config.json
	rm -rf node_modules
	rm -rf start-os/web/dist
	rm -rf start-tunnel/web/dist
	rm -rf brochure/dist
	rm -rf vendor/patch-db/client/node_modules
	rm -rf vendor/patch-db/client/dist
	rm -rf vendor/patch-db/target
	rm -rf target
	rm -rf dpkg-workdir
	rm -rf image-recipe/deb
	rm -rf results
	rm -rf start-os/build/lib/firmware
	rm -rf start-os/container-runtime/dist
	rm -rf start-os/container-runtime/node_modules
	rm -f start-os/container-runtime/*.squashfs
	(cd start-sdk && make clean)
	rm -rf start-os/build/lib/migration-images
	rm -f env/*.txt

format:
	cd shared/crates/start-core && cargo +nightly fmt
	npm --prefix . run format
	cd start-sdk && make fmt

# Read-only formatting verification (prettier --check for web + sdk). Used by CI.
format-check:
	npm --prefix . run format:check
	cd start-sdk && make check-fmt

test: | test-core test-sdk test-container-runtime
