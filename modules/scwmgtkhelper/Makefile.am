## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)
extensionsdir = $(GUILE_EXTENSIONS_DIR)

if HAVE_GTK
extensions_LTLIBRARIES = scwm-gtkhelper.la
endif

scwm_gtkhelper_la_SOURCES = scwmgtkhelper.c
scwm_gtkhelper_la_LDFLAGS = -module -export-dynamic -shared @GTK_LIBS@
scwm_gtkhelper_snarfs = scwmgtkhelper.x

AM_CFLAGS = @GTK_CFLAGS@ @CFLAGS@

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
BUILT_SOURCES = $(libscwmgtkhelper_snarfs)
CLEANFILES = $(libscwmgtkhelper_snarfs)

scwmgtkhelper.x: CFLAGS += AM_FLAGS
