## $Id$
## process this file with automake to create Makefile.in

scwm_module_LTLIBRARIES = libxpm-menus.la

libxpm_menus_la_SOURCES = draw-xpm-menu.c
libxpm_menus_la_LDFLAGS = -export-dynamic

libxpm_menus_snarfs = draw-xpm-menu.x

INCLUDES = -I$(top_srcdir)/scwm @x_cflags@ @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@
BUILT_SOURCES = $(libxpm_menus_snarfs)

SUFFIXES = .x

# just use `make CC=g++' and the revised guile-snarf which uses ${CC} will work
# for C++ prototype extraction
%.x: %.c
	guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@

%.x: %.cc
	CC=$(CXX) \
	guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CXXFLAGS) $< > $@

ETAGS_ARGS = --regex='/[ \t]*SCM_SYMBOL[ \t]*(\([^,]*\)/\1/' --regex='/[ \t]*SCWM_PROC[ \t]*(\([^,]*\),[^,]*/\1/'
