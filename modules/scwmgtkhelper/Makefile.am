## $Id$
## process this file with automake to create Makefile.in

if HAVE_GTK
pkglib_LTLIBRARIES = libscwmgtkhelper.la

libscwmgtkhelper_la_SOURCES = scwmgtkhelper.c
libscwmgtkhelper_la_LDFLAGS = -export-dynamic @GTK_LIBS@
CFLAGS = @CFLAGS@ @GTK_CFLAGS@
libscwmgtkhelper_snarfs = scwmgtkhelper.x

INCLUDES = -I$(top_srcdir)/scwm @x_cflags@ @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@
BUILT_SOURCES = $(libscwmgtkhelper_snarfs)
endif

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
	rm -f $(scwm_moduledir)/libscwmgtkhelper.la
	$(LN_S) $(pkglibdir)/libscwmgtkhelper.la $(scwm_moduledir)/libscwmgtkhelper.la

uninstall-local:
	rm -f $(scwm_moduledir)/libscwmgtkhelper.la
