## $Id$
## process this file with automake to create Makefile.in

info_TEXINFOS=scwm.texi
man_MANS=scwm.1 scwmexec.1 scwmrepl.1
dist_EXTRA=options scwmexec.proto
pkgdata_DATA=scwm-procedures.txt scwm-variables.txt scwm.sgml

# Use extract-docs to build the documentation files
scwm.sgml scwm-procedures.txt: $(scwm_SOURCES)
	outdir=$$PWD/$(top_builddir)/$(subdir); \
	echo Outputting to directory: $$outdir; \
	cd $(top_srcdir) ; \
	perl utilities/dev/scwmdoc -o $$outdir/scwm.sgml -O $$outdir/scwm-variables.txt -V scheme/user-options.scm </dev/null scwm/*.c modules/*/*.c scheme/*.scm > $$outdir/scwm-procedures.txt

html: scwm.sgml
	mkdir -p $@ && cd $@ && \
	jade -t sgml -d $(DOCBOOK_HOME)/html/docbook.dsl ../$<


.PHONY: html
