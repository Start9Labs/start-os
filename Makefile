# Top-level orchestrator. Shared vars/macros live in build/common.mk; each
# product owns its targets in <project>/build.mk. One make process, one DAG —
# includes (not recursive make) so cross-project prerequisites resolve.
# No default build target — bare `make` prints `help`; specify a target explicitly.
.DEFAULT_GOAL := help

include build/common.mk
include shared-libs/crates/start-core/build.mk
include shared-libs/ts-modules/build.mk
include projects/start-sdk/build.mk
include projects/start-cli/build.mk
include projects/start-registry/build.mk
include projects/start-tunnel/build.mk
include projects/start-os/build.mk
include projects/start-docs/build.mk

.PHONY: help startos metadata install-startos clean format format-check install-cli cli cli-deb startos-uis startos-ui startos-emulate-reflash startos-deb startos-$(IMAGE_TYPE) startos-squashfs startos-wormhole startos-wormhole-deb startos-update test test-core test-sdk test-container-runtime registry install-registry tunnel install-tunnel ts-bindings

help:
	@echo "No default target — specify one. Common targets:"
	@echo "  startos startos-deb startos-squashfs startos-ui startos-uis install-startos   (StartOS)"
	@echo "  cli cli-deb registry tunnel                                                   (other products)"
	@echo "  test test-core test-sdk test-container-runtime                                (tests)"
	@echo "  format format-check ts-bindings clean                                         (tooling)"
	@echo "See CONTRIBUTING.md for the full list."

touch:
	touch $(STARTOS_TARGETS)

metadata: $(VERSION_FILE) $(PLATFORM_FILE) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE)

# Per-project cleans live in each build.mk; this only aggregates them.
clean: clean-core clean-web clean-sdk clean-cli clean-registry clean-tunnel clean-startos clean-docs

format:
	cd shared-libs/crates/start-core && cargo +nightly fmt
	npm --prefix . run format
	cd projects/start-sdk && make fmt

# Read-only formatting verification (prettier --check for web + sdk). Used by CI.
format-check:
	npm --prefix . run format:check
	cd projects/start-sdk && make check-fmt

test: | test-core test-sdk test-container-runtime
