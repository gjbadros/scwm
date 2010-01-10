## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)
extensionsdir = $(GUILE_EXTENSIONS_DIR)

BUILT_SOURCES = background.x

extensions_LTLIBRARIES = scwm-background.la

scwm_background_la_SOURCES = background.x background.c
scwm_background_la_LDFLAGS = -module -export-dynamic -shared

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ \
	   @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@


EXTRA_DIST = background.scm

MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)

snarfcppopts = $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)

#SUFFIXES = .x
#.c.x:
#	$(GUILE_SNARF) -o $@ $< $(snarfcppopts)

CLEANFILES = *.x
