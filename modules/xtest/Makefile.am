## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)
extensionsdir = $(GUILE_EXTENSIONS_DIR)

if HAVE_XTEST
extensions_LTLIBRARIES = scwm-xtest.la

scwm_xtest_la_SOURCES = scwmxtest.c
scwm_xtest_la_LDFLAGS = -module -export-dynamic -shared
scwm_xtest_snarfs = scwmxtest.x

BUILT_SOURCES = $(scwm_xtest_snarfs)

endif

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
