SHELL    := /bin/bash

LATEX    := pdflatex
BIBTEX   := bibtex
LATEXMK  := $(shell command -v latexmk 2> /dev/null)
OTT      := $(shell command -v ott 2> /dev/null)

OTTFLAGS := -tex_colour true -tex_wrap false -signal_parse_errors true

OTTFILE  := ds
TEXFILE  := main

all: $(TEXFILE).pdf

ifdef OTT
$(OTTFILE)_.tex: $(OTTFILE).ott
	ott $(OTTFLAGS) -i $< -o $@
endif

$(TEXFILE).pdf: $(TEXFILE).tex $(OTTFILE)_.tex
ifdef LATEXMK
	latexmk -pdf $<
else
	$(LATEX) $<
	bibtex $(subst .tex,.aux,$<)
	$(LATEX) $<
	$(LATEX) $<
endif

halfclean:
	rm -rf *.log *.aux *.blg *~ *.out *.fdb_latexmk *.fls *.vtc

clean: halfclean
	rm -rf $(TEXFILE).pdf *.bbl
ifdef OTT
	rm -rf $(OTTFILE)_.tex
endif

.PHONY: all halfclean clean
