DSLS := $(wildcard dsls/*)
LSPS := $(wildcard lsps/*)
CLIENTS := $(wildcard clients/*)

DSL ?=

.PHONY: all clean $(DSLS) $(LSPS) $(CLIENTS)

# all: $(DSLS) $(LSPS) $(CLIENTS)

all: build-lsp build-client

clean:
	@for d in $(DSLS); do \
		echo "-- [Cleaning DSL $$d] --"; \
		make -C $$d clean; \
	done

	@for l in $(LSPS); do \
		echo "-- [Cleaning LSP $$l] --"; \
		make -C $$l clean; \
	done

	@for l in $(CLIENTS); do \
		echo "-- [Cleaning CLIENT $$l] --"; \
		make -C $$l clean; \
	done

# -- DSLs --

$(DSLS):
	@echo "-- [Building DSL $@] --"
	make -C $@

# -- LSPs --

build-lsp:
ifneq ($(DSL),)
	@echo "-- [Building LSP $(DSL)] --"
	make -C lsps/$(DSL)
else
	@echo "Error: Specify DSL=<name> to build a specific LSP."
	exit 1
endif

$(LSPS):
	@echo "-- [Building DSL $@] --"
	make -C $@

# -- CLIENTs --

build-client:
ifneq ($(DSL),)
	@echo "-- [Building CLIENT $(DSL)] --"
	make -C clients/$(DSL)
else
	@echo "Error: Specify DSL=<name> to build a specific client."
	exit 1
endif

$(CLIENTS):
	@echo "-- [Building CLIENT $@] --"
	make -B -C $@