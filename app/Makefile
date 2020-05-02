CORDOVA := npx cordova
GULP := npx gulp

PLUGINS := cordova-plugin-whitelist cordova-plugin-dialogs

init_npm:
	npm install

$(PLUGINS):
	$(CORDOVA) plugin add $@

init_plugins: $(PLUGINS)

init_platforms:
	$(CORDOVA) platform add android

init: init_npm init_platforms init_plugins

distclean:
	rm -fr node_modules platforms plugins

build:
	dune build
	$(GULP) build

run_android: build
	$(CORDOVA) run android
