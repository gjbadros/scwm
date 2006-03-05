## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

BUILT_SOURCES = background.x

scwm_module_LTLIBRARIES = libscwm_background.la

libscwm_background_la_SOURCES = background.x background.c
libscwm_background_la_LDFLAGS = -export-dynamic

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ \
	   @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@


EXTRA_DIST = background.scm

MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)

snarfcppopts = $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)

#SUFFIXES = .x
#.c.x:
#	$(GUILE_SNARF) -o $@ $< $(snarfcppopts)

CLEANFILES = *.x
