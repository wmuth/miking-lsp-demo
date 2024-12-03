.PHONY: all clean client

MC_FILES := $(shell find . -name "*.mc")

all: ast-gen.mc dsl

clean:
	rm -f dsl ast-gen.mc

ast-gen.mc: ast.syn
	mi syn ast.syn ast-gen.mc

dsl: $(MC_FILES) ast-gen.mc
	mi compile dsl.mc --output dsl