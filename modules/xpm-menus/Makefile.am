## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

BUILT_SOURCES = draw-xpm-menu.x

scwm_module_LTLIBRARIES = libscwm-xpm-menus.la

libscwm_xpm_menus_la_SOURCES = draw-xpm-menu.c
libscwm_xpm_menus_la_LDFLAGS = -export-dynamic

libxpm_menus_snarfs = 

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ \
	   @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@

