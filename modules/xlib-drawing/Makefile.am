# $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

scwm_module_LTLIBRARIES = libxlib-drawing.la
libxlib_drawing_la_SOURCES = drawing.c drawing.h
libxlib_drawing_la_LDFLAGS = -export-dynamic
libxlib_drawing_snarfs = drawing.x

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ \
	   @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
BUILT_SOURCES = $(libxlib_drawing_snarfs)
