## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)
extensionsdir = $(GUILE_EXTENSIONS_DIR)

if HAVE_PROPLIST
extensions_LTLIBRARIES = scwm-proplist.la

scwm_proplist_la_SOURCES = scwmproplist.c
scwm_proplist_snarfs = scwmproplist.x

BUILT_SOURCES = $(scwm_proplist_snarfs)
endif

scwm_proplist_la_LDFLAGS = -module -export-dynamic -shared
scwm_proplist_la_LIBADD = -L/usr/contrib/lib/ -lPropList

INCLUDES = @PROPLIST_INCLUDES@ -I$(SCWM_SRC_DIR) @x_cflags@ \
	@GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
