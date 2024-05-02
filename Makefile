
.PHONY: all clean

all: miking-lsp/dsl/dsl rpc-wrapper/target/debug/rpc-wrapper lsp-sample/client/out/extension.js

clean:
	rm -f miking-lsp/dsl/dsl
	rm -f miking-lsp/dsl/ast-gen.mc
	rm -f rpc-wrapper/target/debug/rpc-wrapper
	cd lsp-sample && npm run clean

lsp-sample/client/out/extension.js: lsp-sample/client/src/extension.ts
	cd lsp-sample && npm install && npm run compile

rpc-wrapper/target/debug/rpc-wrapper: rpc-wrapper/src/main.rs rpc-wrapper/Cargo.toml rpc-wrapper/Cargo.lock
	cd rpc-wrapper && cargo build

# Generating the parser and AST from the '.syn' file
miking-lsp/dsl/ast-gen.mc: miking-lsp/dsl/ast.syn
	mi syn miking-lsp/dsl/ast.syn miking-lsp/dsl/ast-gen.mc

miking-lsp/dsl/dsl: miking-lsp/dsl/ast-gen.mc miking-lsp/dsl/dsl.mc
	mi compile miking-lsp/dsl/dsl.mc --output miking-lsp/dsl/dsl