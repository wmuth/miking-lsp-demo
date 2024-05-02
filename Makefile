
.PHONY: all clean

all: miking-lsp/dsl/dsl rust-server/target/debug/rust-server
	cd lsp-sample && npm install

clean:
	rm -f miking-lsp/dsl/dsl
	rm -f miking-lsp/dsl/ast-gen.mc
	rm -f rust-server/target/debug/rust-server
	cd lsp-sample && npm run clean

rust-server/target/debug/rust-server: rust-server/src/main.rs rust-server/Cargo.toml rust-server/Cargo.lock
	cd rust-server && cargo build

# Generating the parser and AST from the '.syn' file
miking-lsp/dsl/ast-gen.mc: miking-lsp/dsl/ast.syn
	mi syn miking-lsp/dsl/ast.syn miking-lsp/dsl/ast-gen.mc

miking-lsp/dsl/dsl: miking-lsp/dsl/ast-gen.mc miking-lsp/dsl/dsl.mc
	mi compile miking-lsp/dsl/dsl.mc --output miking-lsp/dsl/dsl