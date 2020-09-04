SHELL := /bin/bash

EXEC  := rhovec

all: exe

run: exe
	./$(EXEC)

test:
	dune runtest --force

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

coverage: clean
	BISECT_ENABLE=yes dune build
	dune runtest --force
	bisect-ppx-report html
	bisect-ppx-report summary

deps:
	opam install . --deps-only --with-test

clean:
	dune clean
	rm -rf _coverage
	rm -f $(EXEC)

.PHONY: all run test utop debug exe bc fmt coverage deps clean
