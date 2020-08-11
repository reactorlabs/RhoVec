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

debug: byte
	ocamldebug _build/src/main.byte

native:
	$(OCB) main.native
	cp _build/src/main.native $(EXEC)

byte:
	$(OCB) main.byte

clean:
	$(OCB) -clean
	rm -f $(EXEC)

.PHONY: all native byte clean
