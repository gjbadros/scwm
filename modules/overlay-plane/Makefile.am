## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

scwm_module_LTLIBRARIES = liboverlay-plane.la

liboverlay_plane_la_SOURCES = overlay-plane.c get_color.c
liboverlay_plane_la_LDFLAGS = -export-dynamic

liboverlay_plane_snarfs = overlay-plane.x

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ \
	   @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
BUILT_SOURCES = $(liboverlay_plane_snarfs)
