## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

VVLIB=-lsmapi
VTBNFC = /usr/lib/ViaVoice/bin/vtbnfc
VVINC = -I/usr/lib/ViaVoice/include

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

if HAVE_VIAVOICE
scwm_module_LTLIBRARIES = libscwmviavoice.la
endif

libscwmviavoice_la_SOURCES = scwmviavoice.c
libscwmviavoice_snarfs = scwmviavoice.x

libscwmviavoice_la_LDFLAGS = -export-dynamic $(VVLIB) @GTK_LIBS@

CFLAGS = @GTK_CFLAGS@ @CFLAGS@


GRAMMARS = scwmgrammar.fsg

INCLUDES = -I$(top_srcdir)/scwm -I$(top_srcdir)/include @x_cflags@ @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@ $(VVINC)
BUILT_SOURCES = $(libscwmviavoice_snarfs) $(GRAMMARS)


%.fsg: %.bnf
	$(VTBNFC) -o $@ $<
