## $Id$
## process this file with automake to create Makefile.in

info_TEXINFOS=scwm.texi
man_MANS=scwm.1 scwmexec.1 scwmrepl.1
pkgdata_DATA=scwm-procedures.txt scwm-variables.txt scwm-hooks.txt scwm-concepts.txt scwm.sgml
EXTRA_DIST=scwm-faq scwm-intro-tutorial.scm session-management smproxy.patch \
	theme-howto scwm.1 scwmexec.1 scwmrepl.1 $(pkgdata_DATA)

# Use scwmdoc to build the documentation files
scwm.sgml scwm-procedures.txt scwm-variables.txt scwm-hooks.txt scwm-concepts.txt : $(scwm_SOURCES)
	abs_builddir_doc=`pwd`; \
	outdir=$$abs_builddir_doc; \
	echo Outputting to directory: $$outdir; \
	cd $(top_builddir); abs_top_builddir=`pwd`; \
	cd $$abs_builddir_doc; cd $(top_srcdir); \
	abs_top_srcdir=`pwd`; \
	echo 'make[99]: Entering directory `'$$abs_top_srcdir"'"; \
	perl $$abs_top_builddir/utilities/dev/scwmdoc -o $$outdir/scwm.sgml -O $$outdir/scwm-variables.txt -H $$outdir/scwm-hooks.txt -N $$outdir/scwm-concepts.txt -V scheme/user-options.scm -d $${SCWMDIR:-$$abs_top_srcdir} </dev/null scwm/*.c modules/*/*.c scheme/*.scm > $$outdir/scwm-procedures.txt; \
	echo 'make[99]: Leaving directory `'$$abs_top_srcdir"'"

html: scwm.sgml
	mkdir -p $@ && cd $@ && \
	jade -t sgml -d $(DOCBOOK_HOME)/html/docbook.dsl ../$<


.PHONY: html
