## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

scwm_module_LTLIBRARIES = libxpm-menus.la

libxpm_menus_la_SOURCES = draw-xpm-menu.c
libxpm_menus_la_LDFLAGS = -export-dynamic

libxpm_menus_snarfs = draw-xpm-menu.x

INCLUDES = -I$(top_srcdir)/scwm -I$(top_srcdir)/include @x_cflags@ \
	   @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
BUILT_SOURCES = $(libxpm_menus_snarfs)

SUFFIXES = .x
