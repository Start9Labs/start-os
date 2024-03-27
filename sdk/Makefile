TS_FILES := $(shell find ./**/*.ts )
version = $(shell git tag --sort=committerdate | tail -1)
test: $(TS_FILES) lib/test/output.ts
	npm test

clean:
	rm -rf dist/* | true
	
lib/test/output.ts: lib/test/makeOutput.ts scripts/oldSpecToBuilder.ts
	npm run buildOutput

buildOutput: lib/test/output.ts fmt
	echo 'done'


bundle: $(TS_FILES) package.json .FORCE node_modules test fmt
	npx tsc
	npx tsc --project tsconfig-cjs.json
	cp package.json dist/package.json
	cp README.md dist/README.md
	cp LICENSE dist/LICENSE
	touch dist

full-bundle:
	make clean
	make bundle

check:
	npm run check

fmt: node_modules
	npx prettier --write "**/*.ts"

node_modules: package.json
	npm install

publish: clean bundle	 package.json README.md LICENSE
	cd dist && npm publish --access=public
link: bundle	
	cp package.json dist/package.json
	cp README.md dist/README.md
	cp LICENSE dist/LICENSE
	cd dist && npm link
.FORCE: 
