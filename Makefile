SHELL := /bin/bash
OCB   := ocamlbuild

# don't create symlinks into the _build directory
OCB   += -no-links

# specify the sources
OCB   += -I src

EXEC  := main

all: native

run: native
	./$(EXEC)

native:
	$(OCB) main.native
	cp _build/src/main.native $(EXEC)

clean:
	$(OCB) -clean
	rm -f $(EXEC)

.PHONY: all native clean
