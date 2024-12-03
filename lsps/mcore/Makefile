.PHONY: all clean

MC_FILES := $(shell find . -name "*.mc")
LSP_SERVER_FILES = $(shell find ../../lsp-server -name "*.mc")

all: compile-mcore lsp-server

clean:
	rm -f compile-mcore lsp-server

compile-mcore: $(MC_FILES) $(LSP_SERVER_FILES)
	mi compile compile-mcore.mc --output compile-mcore

lsp-server: $(MC_FILES) $(LSP_SERVER_FILES)
	mi compile lsp-server.mc --output lsp-server