## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)
extensionsdir = $(GUILE_EXTENSIONS_DIR)

extensions_LTLIBRARIES = scwm-pie-menus.la

scwm_pie_menus_la_SOURCES = draw-pie-menu.c
scwm_pie_menus_la_LDFLAGS = -module -export-dynamic -shared
scwm_pie_menus_snarfs = draw-pie-menu.x

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ \
	   @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
BUILT_SOURCES = $(scwm_pie_menus_snarfs)
CLEANFILES = $(scwm_pie_menus_snarfs)
