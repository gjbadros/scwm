## $Id$
## process this file with automake to create Makefile.in

pkglib_LTLIBRARIES = libpie-menus.la

libpie_menus_la_SOURCES = draw-pie-menu.c
libpie_menus_la_LDFLAGS = -export-dynamic

libpie_menus_snarfs = draw-pie-menu.x

INCLUDES = -I$(top_srcdir)/scwm @x_cflags@ @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@
BUILT_SOURCES = $(libpie_menus_snarfs)

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
	$(mkinstalldirs) $(scwm_moduledir)
	rm -f  $(scwm_moduledir)/liboverlay-plane.la
	$(LN_S) $(pkglibdir)/liboverlay-plane.la $(scwm_moduledir)/liboverlay-plane.la

uninstall-local:
	rm -f $(scwm_moduledir)/liboverlay-plane.la
