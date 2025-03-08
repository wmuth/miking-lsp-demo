.PHONY: app clean

JS_FILES := $(shell find . -name "*.js" -not -path "./client/out/*" -not -path "./node_modules/*")
JSON_FILES := $(shell find . -name "*.json" -not -path "./client/out/*" -not -path "./node_modules/*")
TS_FILES := $(shell find . -name "*.ts" -not -path "./client/out/*" -not -path "./node_modules/*")
JSON_FILES := $(shell find . -name "*.json" -not -path "./client/out/*" -not -path "./node_modules/*")

all: client/out/extension.js

clean:
	npm run clean
	rm -rf out lsp-server probtime stdlib

client/out/extension.js: $(JS_FILES) $(TS_FILES) $(JSON_FILES) $(JSON_FILES)
	mkdir -p probtime out
	cp -r ../../{lsp-server,lib} probtime
	mkdir -p probtime/lsps/probtime
	cp -r ../../lsps/probtime/*.mc probtime/lsps/probtime

	rm -rf stdlib
	mkdir -p stdlib
	cp -r ../../../miking/stdlib .

	npm install && npm run compile