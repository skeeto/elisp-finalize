EMACS   ?= emacs
CASK    ?= cask
VIRTUAL := $(CASK) exec $(EMACS)
BATCH   := $(VIRTUAL) -batch -Q -L .

PACKAGE := finalize
VERSION := $(shell git describe)

EL = finalize.el finalizable.el
ELC = $(EL:.el=.elc)
EXTRA_DIST = README.md UNLICENSE

.PHONY : all compile package test clean

all : compile package

compile: $(ELC)

package : $(PACKAGE)-$(VERSION).tar

$(PACKAGE)-$(VERSION).tar : $(EL) $(PACKAGE)-pkg.el $(EXTRA_DIST)
	tar -cf $@ --transform "s,^,$(PACKAGE)-$(VERSION)/," $^

clean:
	$(RM) *.tar *.elc

%.elc: %.el
	$(BATCH) -f batch-byte-compile $<
