## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

EXTRA_DIST = scwmgrammar.fsg

VVLIB=-lsmapi
VTBNFC = /usr/lib/ViaVoice/bin/vtbnfc
VVINC = -I/usr/lib/ViaVoice/include

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

if HAVE_VIAVOICE
scwm_module_LTLIBRARIES = libscwmviavoice.la
BUILT_SOURCES = $(libscwmviavoice_snarfs) $(GRAMMARS)
scwm_viavoice_grammar_DATA = $(GRAMMARS)
endif

libscwmviavoice_snarfs = scwmviavoice.x
libscwmviavoice_la_SOURCES = scwmviavoice.c

libscwmviavoice_la_LDFLAGS = -export-dynamic $(VVLIB) @GTK_LIBS@

CFLAGS = @GTK_CFLAGS@ @CFLAGS@


GRAMMARS = scwmgrammar.fsg

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_builddir)/src -I$(top_srcdir)/include @x_cflags@ @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@ $(VVINC)


%.fsg: %.bnf
	$(VTBNFC) -o $@ $<
