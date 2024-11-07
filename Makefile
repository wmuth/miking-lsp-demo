
.PHONY: all clean playground dsl

all: miking-lsp/dsl/ast-gen.mc miking-lsp/dsl/lsp-server lsp-client/client/out/extension.js

clean:
	rm -f miking-lsp/dsl/lsp-server
	rm -f miking-lsp/dsl/ast-gen.mc
	cd lsp-client && npm run clean

lsp-client/client/out/extension.js: lsp-client/client/src/extension.ts
	cd lsp-client && npm install && npm run compile

# Generating the parser and AST from the '.syn' file
miking-lsp/dsl/ast-gen.mc: miking-lsp/dsl/ast.syn
	mi syn miking-lsp/dsl/ast.syn miking-lsp/dsl/ast-gen.mc

miking-lsp/dsl/lsp-server: miking-lsp/dsl/*.mc miking-lsp/dsl/**/*.mc
	mi compile miking-lsp/dsl/lsp-server.mc --output miking-lsp/dsl/lsp-server

miking-lsp/dsl/dsl: miking-lsp/dsl/*.mc miking-lsp/dsl/**/*.mc
	mi compile miking-lsp/dsl/dsl.mc --output miking-lsp/dsl/dsl

miking-lsp/dsl/playground: miking-lsp/dsl/*.mc miking-lsp/dsl/**/*.mc
	mi compile miking-lsp/dsl/playground.mc --output miking-lsp/dsl/playground

playground: miking-lsp/dsl/playground miking-lsp/dsl/ast-gen.mc
	./miking-lsp/dsl/playground

dsl: miking-lsp/dsl/dsl miking-lsp/dsl/ast-gen.mc
	./miking-lsp/dsl/dsl