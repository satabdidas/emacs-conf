EXTRA=README VERSION TODO COPYING Makefile rng-auto.el \
  test.valid.xml test.invalid.xml

SRC=nxml-mode.el rng-dt.el rng-ns.el rng-valid.el xsd-regexp.el \
  rng-maint.el rng-pttrn.el rng-xsd.el rng-cmpct.el rng-match.el \
  rng-util.el xmltok.el

ELC=nxml-mode.elc rng-dt.elc rng-ns.elc rng-valid.elc xsd-regexp.elc \
  rng-maint.elc rng-pttrn.elc rng-xsd.elc rng-cmpct.elc rng-match.elc \
  rng-util.elc xmltok.elc

EMACS=emacs
PACKAGE=nxml-mode

stamp-byte-compile: $(SRC)
	-rm -f $(ELC)
	$(EMACS) -batch -l rng-auto.el -f rng-byte-compile-load
	touch $(@)

dist: stamp-byte-compile
	@version=`cat VERSION`; \
	set -e; \
	echo Making $(PACKAGE)-$$version.tar.gz; \
	rm -fr $(PACKAGE)-$$version; \
	mkdir $(PACKAGE)-$$version; \
	cd $(PACKAGE)-$$version; \
	for f in $(EXTRA) $(SRC) $(ELC); do \
	  ln -s ../$$f .; \
	done; \
	mkdir schema; \
	cd schema; \
	for f in ../../schema/*.rnc; do \
	  ln -s $$f .; \
	done; \
	cd ../..; \
	tar cfhz $(PACKAGE)-$$version.tar.gz $(PACKAGE)-$$version; \
	rm -fr $(PACKAGE)-$$version

clean:
	-rm -f stamp-byte-compile $(PACKAGE)-`cat VERSION`.tar.gz
