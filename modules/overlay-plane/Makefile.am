## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)
extensionsdir = $(GUILE_EXTENSIONS_DIR)

extensions_LTLIBRARIES = scwm-overlay-plane.la

scwm_overlay_plane_la_SOURCES = overlay-plane.c get_color.c
scwm_overlay_plane_la_LDFLAGS = -module -export-dynamic -shared
scwm_overlay_plane_snarfs = overlay-plane.x

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ \
	   @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
BUILT_SOURCES = $(scwm_overlay_plane_snarfs)
