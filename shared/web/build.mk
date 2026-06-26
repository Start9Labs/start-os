WEB_SHARED_SRC := $(call ls-files, shared/web/shared) $(call ls-files, shared/web/marketplace) $(shell ls -p shared/web/ | grep -v / | sed 's|^|shared/web/|g') package.json angular.json tsconfig.json tsconfig.lib.json node_modules/.package-lock.json config.json vendor/patch-db/client/dist/index.js start-sdk/baseDist/package.json start-os/web/patchdb-ui-seed.json start-sdk/dist/package.json
WEB_UI_SRC := $(call ls-files, start-os/web/ui)
WEB_SETUP_WIZARD_SRC := $(call ls-files, start-os/web/setup-wizard)
WEB_START_TUNNEL_SRC := $(call ls-files, start-tunnel/web)
WEB_UIS := start-os/web/dist/raw/ui/index.html start-os/web/dist/raw/setup-wizard/index.html
COMPRESSED_WEB_UIS := start-os/web/dist/static/ui/index.html start-os/web/dist/static/setup-wizard/index.html

package-lock.json: package.json start-sdk/baseDist/package.json
	npm --prefix . i
	touch package-lock.json

node_modules/.package-lock.json: package-lock.json
	npm --prefix . ci
	touch node_modules/.package-lock.json

.angular/.updated: vendor/patch-db/client/dist/index.js start-sdk/baseDist/package.json node_modules/.package-lock.json
	rm -rf .angular
	mkdir -p .angular
	touch .angular/.updated

.i18n-checked: $(WEB_SHARED_SRC) $(WEB_UI_SRC) $(WEB_SETUP_WIZARD_SRC) $(WEB_START_TUNNEL_SRC)
	npm --prefix . run check:i18n
	touch .i18n-checked

start-os/web/dist/raw/ui/index.html: $(WEB_UI_SRC) $(WEB_SHARED_SRC) .angular/.updated .i18n-checked
	npm --prefix . run build:ui
	touch start-os/web/dist/raw/ui/index.html

start-os/web/dist/raw/setup-wizard/index.html: $(WEB_SETUP_WIZARD_SRC) $(WEB_SHARED_SRC) .angular/.updated .i18n-checked
	npm --prefix . run build:setup
	touch start-os/web/dist/raw/setup-wizard/index.html

start-tunnel/web/dist/raw/start-tunnel/index.html: $(WEB_START_TUNNEL_SRC) $(WEB_SHARED_SRC) .angular/.updated .i18n-checked
	npm --prefix . run build:tunnel
	touch start-tunnel/web/dist/raw/start-tunnel/index.html

start-os/web/dist/static/%/index.html: start-os/web/dist/raw/%/index.html
	./shared/web/compress-uis.sh $* start-os/web

start-tunnel/web/dist/static/%/index.html: start-tunnel/web/dist/raw/%/index.html
	./shared/web/compress-uis.sh $* start-tunnel/web

config.json: $(GIT_HASH_FILE) $(ENVIRONMENT_FILE) shared/web/config-sample.json shared/web/update-config.sh
	./shared/web/update-config.sh	

# this is a convenience step to build all web uis - it is not referenced elsewhere in this file
uis: $(WEB_UIS) 

# this is a convenience step to build the UI
ui: start-os/web/dist/raw/ui
