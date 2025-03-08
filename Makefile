DSLS := $(wildcard dsls/*)
LSPS := $(wildcard lsps/*)
CLIENTS := $(wildcard clients/*)

.PHONY: all clean $(DSLS) $(LSPS) $(CLIENTS)

all: $(DSLS) $(LSPS) $(CLIENTS)

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

$(LSPS):
	@echo "-- [Building LSP $@] --"
	make -C $@

# -- CLIENTs --

$(CLIENTS):
	@echo "-- [Building CLIENT $@] --"
	make -C $@