## $Id$
## process this file with automake to create Makefile.in

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

scwm_module_LTLIBRARIES = libc-animation.la

libc_animation_la_SOURCES = c-animation.c
libc_animation_la_LDFLAGS = -export-dynamic

libc_animation_snarfs = c-animation.x

INCLUDES = -I$(top_srcdir)/scwm @x_cflags@ @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
BUILT_SOURCES = $(libc_animation_snarfs)

SUFFIXES = .x

# just use `make CC=g++' and the revised guile-snarf which uses ${CC} will work
# for C++ prototype extraction
%.x: %.c
	guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@

%.x: %.cc
	CC=$(CXX) \
	guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CXXFLAGS) $< > $@

ETAGS_ARGS = --regex='/[ \t]*SCM_SYMBOL[ \t]*(\([^,]*\)/\1/' --regex='/[ \t]*SCWM_PROC[ \t]*(\([^,]*\),[^,]*/\1/'
