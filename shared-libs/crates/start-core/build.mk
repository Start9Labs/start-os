test-core: $(CORE_SRC) $(ENVIRONMENT_FILE) 
	./shared-libs/crates/start-core/run-tests.sh

ts-bindings: shared-libs/crates/start-core/bindings/index.ts
	mkdir -p projects/start-sdk/base/lib/osBindings
	rsync -ac --delete shared-libs/crates/start-core/bindings/ projects/start-sdk/base/lib/osBindings/

shared-libs/crates/start-core/bindings/index.ts: $(call ls-files, shared-libs/crates/start-core) $(ENVIRONMENT_FILE)
	rm -rf shared-libs/crates/start-core/bindings
	./shared-libs/crates/start-core/build/build-ts.sh
	ls shared-libs/crates/start-core/bindings/*.ts | sed 's|.*/bindings/\([^.]*\)\.ts|export { \1 } from "./\1";|g' | grep -v '"./index"' | tee shared-libs/crates/start-core/bindings/index.ts
	if [ -d shared-libs/crates/start-core/bindings/tunnel ]; then \
		ls shared-libs/crates/start-core/bindings/tunnel/*.ts | sed 's|.*/bindings/tunnel/\([^.]*\)\.ts|export { \1 } from "./\1";|g' | grep -v '"./index"' > shared-libs/crates/start-core/bindings/tunnel/index.ts; \
		echo 'export * as Tunnel from "./tunnel";' >> shared-libs/crates/start-core/bindings/index.ts; \
	fi
	npm --prefix projects/start-sdk/base exec -- prettier --config=./projects/start-sdk/base/package.json -w './shared-libs/crates/start-core/bindings/**/*.ts'
	touch shared-libs/crates/start-core/bindings/index.ts

.PHONY: clean-core
# Owns the shared Rust build root, generated bindings, and global build metadata.
clean-core:
	rm -rf target shared-libs/crates/start-core/bindings shared-libs/crates/patch-db/target
	rm -f build/env/*.txt

# Formats every Rust crate under shared-libs/crates/ (the product crates have their own format targets).
SHARED_CRATE_PKGS := -p start-core -p exver -p imbl-value -p yasi -p rpc-toolkit -p jsonpath_lib -p pi-beep -p patch-db -p patch-db-macro -p patch-db-macro-internals -p patch-db-util -p json-patch -p json-ptr

.PHONY: format-core format-check-core
format-core:
	cargo +nightly fmt $(SHARED_CRATE_PKGS)

format-check-core:
	cargo +nightly fmt --check $(SHARED_CRATE_PKGS)
