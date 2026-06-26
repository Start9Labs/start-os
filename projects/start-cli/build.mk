install-cli: $(GIT_HASH_FILE)
	./shared-libs/crates/start-core/build/build-cli.sh --install

cli: $(GIT_HASH_FILE)
	./shared-libs/crates/start-core/build/build-cli.sh
