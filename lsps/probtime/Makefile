.PHONY: all clean

MC_FILES := $(shell find . -name "*.mc")
LSP_SERVER_FILES = $(shell find ../../lsp-server -name "*.mc")
LIB_SERVER_FILES = $(shell find ../../lib -name "*.mc")
STDLIB_FILES = $(shell find ../../miking/stdlib -name "*.mc")
PROBTIME_FILES = $(shell find ./probtime-lib -type f \( -name "*.mc" -o -name "*.syn" \))

all: lsp-server

clean:
	rm -f lsp-server

probtime-lib/src/ast.mc: $(PROBTIME_FILES)
	mi syn probtime-lib/src/ast.syn probtime-lib/src/ast.mc

lsp-server: probtime-lib/src/ast.mc $(MC_FILES) $(LSP_SERVER_FILES) $(LIB_SERVER_FILES) $(STDLIB_FILES)
	mi compile lsp-server.mc --output lsp-server