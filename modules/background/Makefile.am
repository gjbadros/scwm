## $Id$
## process this file with automake to create Makefile.in

pkglib_LTLIBRARIES = libbackground.la

libbackground_la_SOURCES = background.c
libbackground_la_LDFLAGS = -export-dynamic

libbackground_snarfs = background.x

INCLUDES = -I$(top_srcdir)/scwm @x_cflags@ @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@
BUILT_SOURCES = $(libbackground_snarfs)

SUFFIXES = .x

# just use `make CC=g++' and the revised guile-snarf which uses ${CC} will work
# for C++ prototype extraction
%.x: %.c
	guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@

%.x: %.cc
	CC=$(CXX) \
	guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CXXFLAGS) $< > $@

ETAGS_ARGS = --regex='/[ \t]*SCM_SYMBOL[ \t]*(\([^,]*\)/\1/' --regex='/[ \t]*SCWM_PROC[ \t]*(\([^,]*\),[^,]*/\1/'

install-data-hook:
	$(mkinstalldirs) $(scwm_schemedir)
	rm -f $(scwm_schemedir)/libbackground.la
	$(LN_S) $(pkglibdir)/libbackground.la $(scwm_schemedir)/libbackground.la

uninstall-local:
	rm -f $(scwm_schemedir)/libbackground.la
