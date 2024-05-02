MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

.PHONY: all
all: dsl

.PHONY: clean
clean:
	rm -f dsl/ast-gen.mc dsl/dsl

.PHONY: dsl
dsl: dsl/ast-gen.mc dsl/dsl.mc
	mi compile dsl/dsl.mc --output dsl/$@
	./dsl/$@

# Generating the parser and AST from the '.syn' file
dsl/ast-gen.mc: dsl/ast.syn
	mi syn dsl/ast.syn dsl/ast-gen.mc