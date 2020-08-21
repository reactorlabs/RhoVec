SHELL := /bin/bash

EXEC  := rhovec

all: exe

run: exe
	./$(EXEC)

test:
	dune runtest

utop:
	dune utop lib

debug: bc
	ocamldebug _build/default/src/main.bc

exe:
	dune build src/main.exe
	cp _build/default/src/main.exe $(EXEC)
	@chmod 755 $(EXEC)

bc:
	dune build src/main.bc

fmt:
	@# force this command to always return true
	dune build @fmt --auto-promote || true

deps:
	opam install . --deps-only

clean:
	dune clean
	rm -f $(EXEC)

.PHONY: all run test utop debug exe bc fmt deps clean
