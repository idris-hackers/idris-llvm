.PHONY: build configure install lib_clean test

include config.mk
-include custom.mk

install:
	$(CABAL) install $(CABALFLAGS)

build: dist/setup-config
	$(CABAL) build $(CABALFLAGS)

test:
	test/idris001/run

lib_clean:
	$(MAKE) -C src/rts clean

dist/setup-config:
	$(CABAL) configure $(CABALFLAGS)
