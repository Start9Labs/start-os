PACKAGE_TS_FILES := $(shell git ls-files package/lib)
BASE_TS_FILES := $(shell git ls-files base/lib)
version = $(shell git tag --sort=committerdate | tail -1)

.PHONY: test base/test package/test clean bundle fmt buildOutput check

all: bundle

package/test: $(PACKAGE_TS_FILES) package/node_modules base/node_modules
	cd package && npm test

base/test: $(BASE_TS_FILES) base/node_modules
	cd base && npm test

test: base/test package/test

clean:
	rm -rf base/node_modules
	rm -rf dist
	rm -rf baseDist
	rm -f package/lib/test/output.ts
	rm -rf package/node_modules
	
bundle: baseDist dist | test fmt
	touch dist

base/lib/exver/exver.ts: base/node_modules base/lib/exver/exver.pegjs
	cd base && npm run peggy

baseDist: $(PACKAGE_TS_FILES) $(BASE_TS_FILES) base/package.json base/node_modules base/README.md base/LICENSE
	(cd base && npm run tsc)
	# Copy hand-written .js/.d.ts pairs (no corresponding .ts source) into the output.
	cd base/lib && find . -name '*.js' | while read f; do \
		base="$${f%.js}"; \
		if [ -f "$$base.d.ts" ] && [ ! -f "$$base.ts" ]; then \
			mkdir -p "../../baseDist/$$(dirname "$$f")"; \
			cp "$$f" "../../baseDist/$$f"; \
			cp "$$base.d.ts" "../../baseDist/$$base.d.ts"; \
		fi; \
	done
	rsync -ac base/node_modules baseDist/
	cp base/package.json baseDist/package.json
	cp base/README.md baseDist/README.md
	cp base/LICENSE baseDist/LICENSE
	touch baseDist

dist: $(PACKAGE_TS_FILES) $(BASE_TS_FILES) package/package.json package/.npmignore package/node_modules package/README.md package/LICENSE
	(cd package && npm run tsc)
	cd base/lib && find . -name '*.js' | while read f; do \
		base="$${f%.js}"; \
		if [ -f "$$base.d.ts" ] && [ ! -f "$$base.ts" ]; then \
			mkdir -p "../../dist/base/lib/$$(dirname "$$f")"; \
			cp "$$f" "../../dist/base/lib/$$f"; \
			cp "$$base.d.ts" "../../dist/base/lib/$$base.d.ts"; \
		fi; \
	done
	rsync -ac package/node_modules dist/
	cp package/.npmignore dist/.npmignore
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
	npx --prefix base prettier "**/*.ts" --write

package/package-lock.json: package/package.json
	cd package && npm i

base/package-lock.json: base/package.json
	cd base && npm i

package/node_modules: package/package-lock.json
	cd package && npm ci

base/node_modules: base/package-lock.json
	cd base && npm ci

node_modules: package/node_modules base/node_modules

publish: bundle package/package.json package/README.md package/LICENSE
	cd dist && npm publish --access=public --tag=latest

link: bundle
	cd dist && npm link
