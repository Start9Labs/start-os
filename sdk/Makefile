TS_FILES := $(shell git ls-files lib) lib/test/output.ts
version = $(shell git tag --sort=committerdate | tail -1)

.PHONY: test clean bundle fmt buildOutput check

test: $(TS_FILES) lib/test/output.ts
	npm test

clean:
	rm -rf dist
	rm -f lib/test/output.ts
	rm -rf node_modules
	
lib/test/output.ts: node_modules lib/test/makeOutput.ts scripts/oldSpecToBuilder.ts
	npm run buildOutput

bundle: dist | test fmt
	touch dist

lib/exver/exver.ts: node_modules lib/exver/exver.pegjs
	npx peggy --allowed-start-rules '*' --plugin ./node_modules/ts-pegjs/dist/tspegjs -o lib/exver/exver.ts lib/exver/exver.pegjs

dist: $(TS_FILES) package.json node_modules README.md LICENSE
	npx tsc
	npx tsc --project tsconfig-cjs.json
	cp package.json dist/package.json
	cp README.md dist/README.md
	cp LICENSE dist/LICENSE
	touch dist

full-bundle: bundle

check:
	npm run check

fmt: node_modules
	npx prettier --write "**/*.ts"

node_modules: package.json
	npm ci

publish: bundle package.json README.md LICENSE
	cd dist && npm publish --access=public

link: bundle
	cd dist && npm link
