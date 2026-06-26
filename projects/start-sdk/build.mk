test-sdk: $(call ls-files, start-sdk) projects/start-sdk/base/lib/osBindings/index.ts
	cd projects/start-sdk && make test

projects/start-sdk/dist/package.json projects/start-sdk/baseDist/package.json: $(call ls-files, start-sdk) projects/start-sdk/base/lib/osBindings/index.ts
	(cd projects/start-sdk && make bundle)
	touch projects/start-sdk/dist/package.json
	touch projects/start-sdk/baseDist/package.json
