## $Id$
## process this file with automake to create Makefile.in

info_TEXINFOS=scwm.texi
man_MANS=scwm.1 scwmexec.1 scwmrepl.1
dist_EXTRA=options scwmexec.proto
pkgdata_DATA=scwm-procedures.txt scwm.sgml

# Use extract-docs to build the documentation files
scwm.sgml scwm-procedures.txt: $(scwm_SOURCES)
	cd ..; ./utilities/dev/extract-docs -o doc/scwm.sgml scwm/*.c > doc/scwm-procedures.txt
