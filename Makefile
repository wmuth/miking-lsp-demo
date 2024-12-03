DSLS := $(wildcard dsls/*)
LSPS := $(wildcard lsps/*)

.PHONY: all clean vscode-client $(DSLS) $(LSPS)

all: $(DSLS) $(LSPS) vscode-client

clean:
	@for d in $(DSLS); do \
		echo "-- [Cleaning DSL $$d] --"; \
		make -C $$d clean; \
	done

	@for l in $(LSPS); do \
		echo "-- [Cleaning LSP $$l] --"; \
		make -C $$l clean; \
	done

	@echo "-- [Cleaning VSCode extension] --"
	@make -C lsp-client clean

# -- DSLs --

$(DSLS):
	@echo "-- [Building DSL $@] --"
	@make -C $@

# -- LSPs --

$(LSPS):
	@echo "-- [Building LSP $@] --"
	@make -C $@

# -- VSCode extension --

vscode-client:
	@echo "-- [Building VSCode extension] --"
	@make -C lsp-client