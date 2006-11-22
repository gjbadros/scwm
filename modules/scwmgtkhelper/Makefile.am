## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

if HAVE_GTK
scwm_module_LTLIBRARIES = libscwmgtkhelper.la
endif

libscwmgtkhelper_la_SOURCES = scwmgtkhelper.c
libscwmgtkhelper_snarfs = scwmgtkhelper.x

libscwmgtkhelper_la_LDFLAGS = -export-dynamic @GTK_LIBS@

AM_CFLAGS = @GTK_CFLAGS@ @CFLAGS@

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
BUILT_SOURCES = $(libscwmgtkhelper_snarfs)

scwmgtkhelper.x: CFLAGS += AM_FLAGS
