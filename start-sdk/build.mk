test-sdk: $(call ls-files, start-sdk) start-sdk/base/lib/osBindings/index.ts
	cd start-sdk && make test

start-sdk/dist/package.json start-sdk/baseDist/package.json: $(call ls-files, start-sdk) start-sdk/base/lib/osBindings/index.ts
	(cd start-sdk && make bundle)
	touch start-sdk/dist/package.json
	touch start-sdk/baseDist/package.json
