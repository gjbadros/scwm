## $Id$
## process this file with automake to create Makefile.in

info_TEXINFOS=scwm.texi
man_MANS=scwm.1 scwmexec.1 scwmrepl.1
dist_EXTRA=options scwmexec.proto
pkgdata_DATA=scwm-procedures.txt scwm-variables.txt scwm.sgml

# Use extract-docs to build the documentation files
scwm.sgml scwm-procedures.txt: $(scwm_SOURCES)
	cd ..; perl ./utilities/dev/extract-docs -o doc/scwm.sgml -O doc/scwm-variables.txt -V scheme/user-options.scm -d `pwd` </dev/null scwm/*.c scheme/*.scm > doc/scwm-procedures.txt

html: scwm.sgml
	mkdir -p $@ && cd $@ && \
	jade -t sgml -d $(DOCBOOK_HOME)/html/docbook.dsl ../$<


.PHONY: html
