# $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

scwm_module_LTLIBRARIES = libscwm_xlib-drawing.la
libscwm_xlib_drawing_la_SOURCES = drawing.c drawing.h
libscwm_xlib_drawing_la_LDFLAGS = -export-dynamic
libscwm_xlib_drawing_snarfs = drawing.x

EXTRA_DIST = drawing.scm

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ \
	   @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
BUILT_SOURCES = $(libxlib_drawing_snarfs)
