.PHONY: all clean

MC_FILES := $(shell find . -name "*.mc")
LSP_SERVER_FILES = $(shell find ../../lsp-server -name "*.mc")
LIB_SERVER_FILES = $(shell find ../../lib -name "*.mc")
STDLIB_FILES = $(shell find ../../../miking/stdlib -name "*.mc")

all: lsp-server

clean:
	rm -f lsp-server

lsp-server: $(MC_FILES) $(LSP_SERVER_FILES) $(LIB_SERVER_FILES) $(STDLIB_FILES)
	mi compile lsp-server.mc --output lsp-server