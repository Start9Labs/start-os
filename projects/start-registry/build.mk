# When this product's build inputs change, mirror them into the `paths:` filter
# of .github/workflows/start-registry.yaml (see root AGENTS.md "Coupled changes").

REGISTRY_TARGETS := target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/registrybox projects/start-registry/start-registryd.service

registry: target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/registrybox

install-registry: $(REGISTRY_TARGETS)
	$(call mkdir,$(DESTDIR)/usr/bin)
	$(call cp,target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/registrybox,$(DESTDIR)/usr/bin/start-registrybox)
	$(call ln,/usr/bin/start-registrybox,$(DESTDIR)/usr/bin/start-registryd)
	$(call ln,/usr/bin/start-registrybox,$(DESTDIR)/usr/bin/start-registry)

	$(call mkdir,$(DESTDIR)/lib/systemd/system)
	$(call cp,projects/start-registry/start-registryd.service,$(DESTDIR)/lib/systemd/system/start-registryd.service)

target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/registrybox: $(CORE_SRC) $(ENVIRONMENT_FILE)
	ARCH=$(ARCH) PROFILE=$(PROFILE) ./shared-libs/crates/start-core/build/build-registrybox.sh

registry-deb: results/$(REGISTRY_BASENAME).deb

results/$(REGISTRY_BASENAME).deb: debian/build.sh $(call ls-files,projects/start-registry/debian) $(REGISTRY_TARGETS)
	PROJECT=start-registry PLATFORM=$(ARCH) REQUIRES=debian DEPENDS=ca-certificates ./build/os-compat/run-compat.sh ./debian/build.sh

.PHONY: clean-registry
clean-registry:
	rm -f results/start-registry-*.deb
	rm -rf dpkg-workdir/start-registry-*
