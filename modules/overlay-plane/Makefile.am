## $Id$
## process this file with automake to create Makefile.in

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

scwm_module_LTLIBRARIES = liboverlay-plane.la

liboverlay_plane_la_SOURCES = overlay-plane.c get_color.c
liboverlay_plane_la_LDFLAGS = -export-dynamic

liboverlay_plane_snarfs = overlay-plane.x

INCLUDES = -I$(top_srcdir)/scwm @x_cflags@ @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
BUILT_SOURCES = $(liboverlay_plane_snarfs)

SUFFIXES = .x

# just use `make CC=g++' and the revised guile-snarf which uses ${CC} will work
# for C++ prototype extraction
%.x: %.c
	guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@

%.x: %.cc
	CC=$(CXX) \
	guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CXXFLAGS) $< > $@

ETAGS_ARGS = --regex='/[ \t]*SCM_SYMBOL[ \t]*(\([^,]*\)/\1/' --regex='/[ \t]*SCWM_PROC[ \t]*(\([^,]*\),[^,]*/\1/'
