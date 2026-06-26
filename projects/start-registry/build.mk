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

results/$(REGISTRY_BASENAME).deb: debian/dpkg-build.sh $(call ls-files,debian/start-registry) $(REGISTRY_TARGETS)
	PROJECT=start-registry PLATFORM=$(ARCH) REQUIRES=debian DEPENDS=ca-certificates ./build/os-compat/run-compat.sh ./debian/dpkg-build.sh
