## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

if HAVE_PROPLIST
scwm_module_LTLIBRARIES = libscwmproplist.la
endif

libscwmproplist_la_SOURCES = scwmproplist.c
libscwmproplist_snarfs = scwmproplist.x

libscwmproplist_la_LDFLAGS = -export-dynamic
libscwmproplist_la_LIBADD = -L/usr/contrib/lib/ -lPropList
#@PROPLIST_LIBS@

INCLUDES = @PROPLIST_INCLUDES@ -I$(top_srcdir)/scwm @x_cflags@ \
	@GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
BUILT_SOURCES = $(libscwmproplist_snarfs)
