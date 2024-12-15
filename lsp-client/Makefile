.PHONY: app clean

JS_FILES := $(shell find . -name "*.js" -not -path "./client/out/*" -not -path "./node_modules/*")
TS_FILES := $(shell find . -name "*.ts" -not -path "./client/out/*" -not -path "./node_modules/*")
JSON_FILES := $(shell find . -name "*.json" -not -path "./client/out/*" -not -path "./node_modules/*")

all: client/out/extension.js

clean:
	npm run clean
	rm -rf lsp-server mcore

client/out/extension.js: $(JS_FILES) $(TS_FILES) $(JSON_FILES)
	npm install && npm run compile