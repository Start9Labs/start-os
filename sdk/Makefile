PACKAGE_TS_FILES := $(shell git ls-files package/lib) package/lib/test/output.ts
BASE_TS_FILES := $(shell git ls-files base/lib) package/lib/test/output.ts
version = $(shell git tag --sort=committerdate | tail -1)

.PHONY: test clean bundle fmt buildOutput check

all: bundle

package/test: $(PACKAGE_TS_FILES) package/lib/test/output.ts package/node_modules
	cd package && npm test

base/test: $(BASE_TS_FILES) base/node_modules
	cd base && npm test

test: package/test base/test

clean:
	rm -rf base/node_modules
	rm -rf dist
	rm -f package/lib/test/output.ts
	rm -rf package/node_modules
	
package/lib/test/output.ts: package/node_modules package/lib/test/makeOutput.ts package/scripts/oldSpecToBuilder.ts
	cd package && npm run buildOutput

bundle: dist | test fmt
	touch dist

base/lib/exver/exver.ts: base/node_modules base/lib/exver/exver.pegjs
	cd base && npm run peggy

# Need a dist for browser

dist: $(PACKAGE_TS_FILES) $(BASE_TS_FILES) package/package.json package/node_modules package/README.md package/LICENSE	
	(cd package && npm run tsc)
	(cd package && npm run tsc -- --project tsconfig-cjs.json)
	cp package/package.json dist/package.json
	cp package/README.md dist/README.md
	cp package/LICENSE dist/LICENSE
	touch dist

full-bundle: bundle

check:
	cd package
	npm run check
	cd ../base
	npm run check

fmt: package/node_modules base/node_modules
	npx prettier . "**/*.ts" --write


package/node_modules: package/package.json
	cd package && npm ci

base/node_modules: base/package.json
	cd base && npm ci

node_modules: package/node_modules base/node_modules

publish: bundle package/package.json README.md LICENSE
	cd dist
	npm publish --access=public

link: bundle
	cd dist
	npm link
