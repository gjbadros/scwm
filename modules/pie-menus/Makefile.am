## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

scwm_module_LTLIBRARIES = libpie-menus.la

libpie_menus_la_SOURCES = draw-pie-menu.c
libpie_menus_la_LDFLAGS = -export-dynamic

libpie_menus_snarfs = draw-pie-menu.x

INCLUDES = -I$(top_srcdir)/scwm -I$(top_srcdir)/include @x_cflags@ \
	   @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
BUILT_SOURCES = $(libpie_menus_snarfs)
