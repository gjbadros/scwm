## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

if HAVE_XTEST
scwm_module_LTLIBRARIES = libscwmxtest.la

libscwmxtest_la_SOURCES = scwmxtest.c
libscwmxtest_snarfs = scwmxtest.x

BUILT_SOURCES = $(libscwmxtest_snarfs)

endif

libscwmxtest_la_LDFLAGS = -export-dynamic

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
