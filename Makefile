SHELL := /bin/bash

EXEC  := rhovec

all: exe

run: exe
	./$(EXEC)

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

clean:
	dune clean
	rm -f $(EXEC)

.PHONY: all run utop debug exe bc fmt clean
