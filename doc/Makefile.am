## $Id$
## process this file with automake to create Makefile.in

info_TEXINFOS=scwm.texi
man_MANS=scwm.1 scwmexec.1 scwmrepl.1
dist_EXTRA=options scwmexec.proto
pkgdata_DATA=scwm-procedures.txt scwm-variables.txt scwm.sgml

# Use extract-docs to build the documentation files
scwm.sgml scwm-procedures.txt: $(scwm_SOURCES)
	perl ../utilities/dev/scwmdoc -o scwm.sgml -O scwm-variables.txt -V ../scheme/user-options.scm -d $(top_srcdir) </dev/null $(top_srcdir)/scwm/*.c $(top_srcdir)/scheme/*.scm > scwm-procedures.txt

html: scwm.sgml
	mkdir -p $@ && cd $@ && \
	jade -t sgml -d $(DOCBOOK_HOME)/html/docbook.dsl ../$<


.PHONY: html
