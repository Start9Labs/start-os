WEB_SHARED_SRC := $(call ls-files, shared-libs/web/shared) $(call ls-files, shared-libs/web/marketplace) $(shell ls -p shared-libs/web/ | grep -v / | sed 's|^|shared-libs/web/|g') package.json angular.json tsconfig.json tsconfig.lib.json node_modules/.package-lock.json config.json shared-libs/crates/patch-db/client/dist/index.js projects/start-sdk/baseDist/package.json projects/start-os/web/patchdb-ui-seed.json projects/start-sdk/dist/package.json
WEB_UI_SRC := $(call ls-files, projects/start-os/web/ui)
WEB_SETUP_WIZARD_SRC := $(call ls-files, projects/start-os/web/setup-wizard)
WEB_START_TUNNEL_SRC := $(call ls-files, projects/start-tunnel/web)
WEB_UIS := projects/start-os/web/dist/raw/ui/index.html projects/start-os/web/dist/raw/setup-wizard/index.html
COMPRESSED_WEB_UIS := projects/start-os/web/dist/static/ui/index.html projects/start-os/web/dist/static/setup-wizard/index.html

package-lock.json: package.json projects/start-sdk/baseDist/package.json
	npm --prefix . i
	touch package-lock.json

node_modules/.package-lock.json: package-lock.json
	npm --prefix . ci
	touch node_modules/.package-lock.json

.angular/.updated: shared-libs/crates/patch-db/client/dist/index.js projects/start-sdk/baseDist/package.json node_modules/.package-lock.json
	rm -rf .angular
	mkdir -p .angular
	touch .angular/.updated

.i18n-checked: $(WEB_SHARED_SRC) $(WEB_UI_SRC) $(WEB_SETUP_WIZARD_SRC) $(WEB_START_TUNNEL_SRC)
	npm --prefix . run check:i18n
	touch .i18n-checked

projects/start-os/web/dist/raw/ui/index.html: $(WEB_UI_SRC) $(WEB_SHARED_SRC) .angular/.updated .i18n-checked
	npm --prefix . run build:ui
	touch projects/start-os/web/dist/raw/ui/index.html

projects/start-os/web/dist/raw/setup-wizard/index.html: $(WEB_SETUP_WIZARD_SRC) $(WEB_SHARED_SRC) .angular/.updated .i18n-checked
	npm --prefix . run build:setup
	touch projects/start-os/web/dist/raw/setup-wizard/index.html

projects/start-tunnel/web/dist/raw/start-tunnel/index.html: $(WEB_START_TUNNEL_SRC) $(WEB_SHARED_SRC) .angular/.updated .i18n-checked
	npm --prefix . run build:tunnel
	touch projects/start-tunnel/web/dist/raw/start-tunnel/index.html

projects/start-os/web/dist/static/%/index.html: projects/start-os/web/dist/raw/%/index.html
	./shared-libs/web/compress-uis.sh $* projects/start-os/web

projects/start-tunnel/web/dist/static/%/index.html: projects/start-tunnel/web/dist/raw/%/index.html
	./shared-libs/web/compress-uis.sh $* projects/start-tunnel/web

config.json: $(GIT_HASH_FILE) $(ENVIRONMENT_FILE) shared-libs/web/config-sample.json shared-libs/web/update-config.sh
	./shared-libs/web/update-config.sh	

# this is a convenience step to build all web uis - it is not referenced elsewhere in this file
uis: $(WEB_UIS) 

# this is a convenience step to build the UI
ui: projects/start-os/web/dist/raw/ui
