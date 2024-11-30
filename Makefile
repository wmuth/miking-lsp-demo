
.PHONY: all clean playground dsl

all: miking-lsp/dsl/ast-gen.mc miking-lsp/dsl/lsp-server lsp-client/client/out/extension.js mcore-lsp/lsp-server

clean:
	rm -f miking-lsp/dsl/lsp-server
	rm -f mcore-lsp/lsp-server
	rm -f miking-lsp/dsl/ast-gen.mc
	cd lsp-client && npm run clean

lsp-client/client/out/extension.js: lsp-client/client/src/**/* lsp-client/client/syntaxes/**/* lsp-client/client/package.json
	cd lsp-client && npm install && npm run compile

# Generating the parser and AST from the '.syn' file
miking-lsp/dsl/ast-gen.mc: miking-lsp/dsl/ast.syn
	mi syn miking-lsp/dsl/ast.syn miking-lsp/dsl/ast-gen.mc

miking-lsp/dsl/lsp-server: miking-lsp/dsl/*.mc miking-lsp/dsl/**/*.mc
	mi compile miking-lsp/dsl/lsp-server.mc --output miking-lsp/dsl/lsp-server

mcore-lsp/compile-mcore: miking-lsp/dsl/**/*.mc mcore-lsp/*.mc
	mi compile mcore-lsp/compile-mcore.mc --output mcore-lsp/compile-mcore

mcore-lsp/lsp-server: mcore-lsp/compile-mcore miking-lsp/dsl/*.mc miking-lsp/dsl/**/*.mc mcore-lsp/*.mc
	mi compile mcore-lsp/lsp.mc --output mcore-lsp/lsp-server

miking-lsp/dsl/dsl: miking-lsp/dsl/*.mc miking-lsp/dsl/**/*.mc
	mi compile miking-lsp/dsl/dsl.mc --output miking-lsp/dsl/dsl

miking-lsp/dsl/playground: miking-lsp/dsl/*.mc miking-lsp/dsl/**/*.mc
	mi compile miking-lsp/dsl/playground.mc --output miking-lsp/dsl/playground

playground: miking-lsp/dsl/playground miking-lsp/dsl/ast-gen.mc
	./miking-lsp/dsl/playground

dsl: miking-lsp/dsl/dsl miking-lsp/dsl/ast-gen.mc
	./miking-lsp/dsl/dsl