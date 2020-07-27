SHELL := /bin/bash
OCB   := ocamlbuild

# don't create symlinks into the _build directory
OCB   += -no-links

# dump detailed info (in binary format); used by tools like Merlin
OCB   += -tag bin_annot

# specify the sources
OCB   += -I src

EXEC  := main

all: native

native:
	$(OCB) main.native
	cp _build/src/main.native $(EXEC)

clean:
	$(OCB) -clean
	rm -f $(EXEC)

.PHONY: all native clean
