test-core: $(CORE_SRC) $(ENVIRONMENT_FILE) 
	./shared/crates/start-core/run-tests.sh

ts-bindings: shared/crates/start-core/bindings/index.ts
	mkdir -p start-sdk/base/lib/osBindings
	rsync -ac --delete shared/crates/start-core/bindings/ start-sdk/base/lib/osBindings/

shared/crates/start-core/bindings/index.ts: $(call ls-files, shared/crates/start-core) $(ENVIRONMENT_FILE)
	rm -rf shared/crates/start-core/bindings
	./shared/crates/start-core/build/build-ts.sh
	ls shared/crates/start-core/bindings/*.ts | sed 's|.*/bindings/\([^.]*\)\.ts|export { \1 } from "./\1";|g' | grep -v '"./index"' | tee shared/crates/start-core/bindings/index.ts
	if [ -d shared/crates/start-core/bindings/tunnel ]; then \
		ls shared/crates/start-core/bindings/tunnel/*.ts | sed 's|.*/bindings/tunnel/\([^.]*\)\.ts|export { \1 } from "./\1";|g' | grep -v '"./index"' > shared/crates/start-core/bindings/tunnel/index.ts; \
		echo 'export * as Tunnel from "./tunnel";' >> shared/crates/start-core/bindings/index.ts; \
	fi
	npm --prefix start-sdk/base exec -- prettier --config=./start-sdk/base/package.json -w './shared/crates/start-core/bindings/**/*.ts'
	touch shared/crates/start-core/bindings/index.ts
