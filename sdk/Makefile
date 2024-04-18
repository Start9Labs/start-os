TS_FILES := $(shell find ./**/*.ts )
version = $(shell git tag --sort=committerdate | tail -1)

.PHONY: test clean bundle fmt buildOutput check

test: $(TS_FILES) lib/test/output.ts
	npm test

clean:
	rm -rf dist
	
lib/test/output.ts: lib/test/makeOutput.ts scripts/oldSpecToBuilder.ts
	npm run buildOutput

bundle: dist | test fmt
	touch dist

dist: $(TS_FILES) package.json node_modules README.md LICENSE
	npx tsc
	npx tsc --project tsconfig-cjs.json
	cp package.json dist/package.json
	cp README.md dist/README.md
	cp LICENSE dist/LICENSE
	touch dist

full-bundle: clean bundle

check:
	npm run check

fmt: node_modules
	npx prettier --write "**/*.ts"

node_modules: package.json
	npm install

publish: clean bundle package.json README.md LICENSE
	cd dist && npm publish --access=public

link: bundle
	cd dist && npm link
