# $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)
extensionsdir = $(GUILE_EXTENSIONS_DIR)

BUILT_SOURCES = drawing.x

extensions_LTLIBRARIES = scwm-xlib-drawing.la
scwm_xlib_drawing_la_SOURCES = drawing.c drawing.h
scwm_xlib_drawing_la_LDFLAGS = -module -export-dynamic -shared

EXTRA_DIST = drawing.scm

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ \
	   @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@

CLEANFILES = drawing.x
