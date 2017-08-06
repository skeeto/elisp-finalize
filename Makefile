.POSIX:
.SUFFIXES: .el .elc
EMACS   = emacs
VERSION = 2.0.0

EL = finalize.el finalizable.el
ELC = $(EL:.el=.elc)
EXTRA_DIST = README.md UNLICENSE

compile: $(ELC)

package: finalize-$(VERSION).tar

finalize-$(VERSION).tar: $(EL) finalize-pkg.el $(EXTRA_DIST)
	tar -cf $@ --transform "s,^,finalize-$(VERSION)/," $^

clean:
	rm -f finalize-$(VERSION).tar $(ELC)

.el.elc:
	$(EMACS) -batch -Q -L . -f batch-byte-compile $<
