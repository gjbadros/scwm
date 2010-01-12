## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)
extensionsdir = $(GUILE_EXTENSIONS_DIR)

extensions_LTLIBRARIES = scwm-xpm-menus.la

scwm_xpm_menus_la_SOURCES = draw-xpm-menu.c
scwm_xpm_menus_la_LDFLAGS = -module -export-dynamic -shared

scwm_xpm_menus_snarfs = draw-xpm-menu.x

BUILT_SOURCES = $(scwm_xpm_menus_snarfs)
CLEANFILES = $(scwm_xpm_menus_snarfs)

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ \
	   @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@

