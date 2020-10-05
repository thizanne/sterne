CORDOVA := npx cordova
GULP := npx gulp
DUNE := dune

PLUGINS := cordova-plugin-whitelist cordova-plugin-dialogs

VERSION := $$(git describe --always --dirty)

default: build_android

config.xml: config.xml.in
	sed "s/%%VERSION%%/${VERSION}/" $< > $@

init_npm:
	npm install

$(PLUGINS): config.xml
	$(CORDOVA) plugin add $@

init_plugins: $(PLUGINS)

init_platforms: config.xml
	$(CORDOVA) platform add android

init: init_npm init_platforms init_plugins

distclean:
	rm -fr node_modules platforms plugins

prebuild:
	$(DUNE) build
	$(GULP) build

build_android: config.xml prebuild
	$(CORDOVA) build android

run_android: build_android
	$(CORDOVA) run android

clean:
	$(DUNE) clean
	$(GULP) clean
	$(CORDOVA) clean
