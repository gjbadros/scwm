## $Id$
## process this file with automake to create Makefile.in

info_TEXINFOS=scwm.texi
man_MANS=scwm.1 scwmexec.1 scwmrepl.1
dist_EXTRA=options scwmexec.proto
pkgdata_DATA=scwm-procedures.txt scwm-variables.txt scwm.sgml

# Use extract-docs to build the documentation files
scwm.sgml scwm-procedures.txt: $(scwm_SOURCES)
	abs_builddir_doc=`pwd`; \
	outdir=$$abs_builddir_doc; \
	echo Outputting to directory: $$outdir; \
	cd $(top_builddir); abs_top_builddir=`pwd`; \
	cd $$abs_builddir_doc; cd $(top_srcdir); \
	perl $$abs_top_builddir/utilities/dev/scwmdoc -o $$outdir/scwm.sgml -O $$outdir/scwm-variables.txt -V scheme/user-options.scm </dev/null scwm/*.c modules/*/*.c scheme/*.scm > $$outdir/scwm-procedures.txt

html: scwm.sgml
	mkdir -p $@ && cd $@ && \
	jade -t sgml -d $(DOCBOOK_HOME)/html/docbook.dsl ../$<


.PHONY: html
