# $Id$
## process this file with automake to create Makefile.in

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

scwm_module_LTLIBRARIES = libesdsound.la
libesdsound_la_SOURCES = sound.c
libesdsound_la_LIBADD = @ESD_LIBS@
libesdsound_la_LDFLAGS = -export-dynamic
libesdsound_snarfs = sound.x

INCLUDES = -I$(top_srcdir)/scwm @x_cflags@ @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@
BUILT_SOURCES = $(libesdsound_snarfs)

SUFFIXES = .x

# just use `make CC=g++' and the revised guile-snarf which uses ${CC} will work
# for C++ prototype extraction
%.x: %.c
	guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@

%.x: %.cc
	CC=$(CXX) guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CXXFLAGS) $< > $@

ETAGS_ARGS = --regex='/[ \t]*SCM_SYMBOL[ \t]*(\([^,]*\)/\1/' --regex='/[ \t]*SCWM_PROC[ \t]*(\([^,]*\),[^,]*/\1/'
