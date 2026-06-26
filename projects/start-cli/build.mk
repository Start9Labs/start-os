# When this product's build inputs change, mirror them into the `paths:` filter
# of .github/workflows/start-cli.yaml (see root AGENTS.md "Coupled changes").

CLI_TARGETS := target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/start-cli

cli: $(GIT_HASH_FILE)
	./shared-libs/crates/start-core/build/build-cli.sh

# Build into a workspace-relative path for packaging (musl release).
$(CLI_TARGETS): $(CORE_SRC) $(ENVIRONMENT_FILE)
	ARCH=$(ARCH) PROFILE=$(PROFILE) ./shared-libs/crates/start-core/build/build-cli.sh
	touch $@

# Stage the binary into DESTDIR (used by debian/build.sh and for a system install).
# For a local PATH install instead, run build-cli.sh --install.
install-cli: $(CLI_TARGETS)
	$(call mkdir,$(DESTDIR)/usr/bin)
	$(call cp,$(CLI_TARGETS),$(DESTDIR)/usr/bin/start-cli)

cli-deb: results/$(CLI_BASENAME).deb

results/$(CLI_BASENAME).deb: debian/build.sh $(CLI_TARGETS)
	PROJECT=start-cli PLATFORM=$(ARCH) REQUIRES=debian ./build/os-compat/run-compat.sh ./debian/build.sh
