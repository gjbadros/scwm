## $Id$
## process this file with automake to create Makefile.in
## (You should run ./autogen.sh from the top level directory)

info_TEXINFOS=scwm.texi
man_MANS=scwm.1 scwmexec.1 scwmrepl.1
pkgdata_DATA=scwm-procedures.txt scwm-variables.txt scwm-hooks.txt scwm-concepts.txt scwm.sgml
EXTRA_DIST=scwm-faq scwm-intro-tutorial.scm session-management smproxy.patch \
	theme-howto scwm.1 scwmexec.1 scwmrepl.1 $(pkgdata_DATA)

# Use scwmdoc to build the documentation files
# Set envvar $SCWMDOC_TEST_OPTIONS to  "-F" for a faster test
scwm.sgml scwm-procedures.txt scwm-variables.txt scwm-hooks.txt scwm-concepts.txt : $(scwm_SOURCES)
	abs_builddir_doc=`pwd`; \
	outdir=$$abs_builddir_doc; \
	echo Outputting to directory: $$outdir; \
	cd $(top_builddir); abs_top_builddir=`pwd`; \
	cd $$abs_builddir_doc; cd $(top_srcdir); \
	abs_top_srcdir=`pwd`; \
	echo 'make[99]: Entering directory `'$$abs_top_srcdir"'"; \
	perl $$abs_top_builddir/utilities/dev/scwmdoc $$SCWMDOC_TEST_OPTIONS -o $$outdir/scwm.sgml -O $$outdir/scwm-variables.txt -H $$outdir/scwm-hooks.txt -N $$outdir/scwm-concepts.txt </dev/null $$SCWMDOC_ARGS scwm/*.c modules/*/*.c scheme/*.scm > $$outdir/scwm-procedures.txt; \
	echo 'make[99]: Leaving directory `'$$abs_top_srcdir"'"

html: scwm.sgml
	mkdir -p html && cd html && \
	jade -t sgml -d $(DOCBOOK_HOME)/html/docbook.dsl ../$<

tex scwm.tex: scwm.sgml
	jade -t tex -d $(DOCBOOK_HOME)/print/docbook.dsl $< || true

dvi scwm.dvi: scwm.tex
	jadetex $<

ps scwm.ps: scwm.dvi
	dvicmps -o scwm.ps $<

pdf scwm.pdf: scwm.sgml
	db2pdf $<

.PHONY: html tex dvi ps pdf

