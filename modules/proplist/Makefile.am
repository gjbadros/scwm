## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

if HAVE_PROPLIST
scwm_module_LTLIBRARIES = libscwmproplist.la

libscwmproplist_la_SOURCES = scwmproplist.c
libscwmproplist_snarfs = scwmproplist.x

BUILT_SOURCES = $(libscwmproplist_snarfs)
endif

libscwmproplist_la_LDFLAGS = -export-dynamic
libscwmproplist_la_LIBADD = -L/usr/contrib/lib/ -lPropList

INCLUDES = @PROPLIST_INCLUDES@ -I$(SCWM_SRC_DIR) @x_cflags@ \
	@GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
