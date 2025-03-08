.PHONY: app clean

JS_FILES := $(shell find . -name "*.js" -not -path "./client/out/*" -not -path "./node_modules/*")
JSON_FILES := $(shell find . -name "*.json" -not -path "./client/out/*" -not -path "./node_modules/*")
TS_FILES := $(shell find . -name "*.ts" -not -path "./client/out/*" -not -path "./node_modules/*")

all: client/out/extension.js

clean:
	npm run clean
	rm -rf out lsp-server probtime stdlib probtime-lib coreppl-lib

client/out/extension.js: $(JS_FILES) $(TS_FILES) $(JSON_FILES) clean
	mkdir -p probtime out
	cp -r ../../{lsp-server,lib} probtime
	mkdir -p probtime/lsps/probtime
	cp -r ../../lsps/probtime/*.mc probtime/lsps/probtime

	rm -rf stdlib && cp -r ../../../miking/stdlib .
	rm -rf probtime-lib && cp -r ../../../probtime-lib . && rm -rf probtime-lib/.git
	rm -rf coreppl-lib && cp -r ../../../miking-dppl/coreppl/src ./coreppl-lib

	npm install && npm run compile