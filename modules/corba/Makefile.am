## $Id$
## process this file with automake to create Makefile.in
## (You should run ./autogen.sh from the top level directory)

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

if HAVE_GNOME_CORBA
bin_PROGRAMS = scwm-corba-exec 
scwm_module_LTLIBRARIES = libscwmcorba.la
libscwmcorba_snarfs = scwmcorba.x

endif

libscwmcorba_la_SOURCES = \
	scwmcorba.c \
	scwm_scheme_evaluator-common.c \
	scwm_scheme_evaluator-skels.c

libscwmcorba_la_LDFLAGS = \
 -lgnorba -lORBitCosNaming -lORBit -lIIOP -lORBitutil -lgnomeui -lart_lgpl -lgdk_imlib \
 -lSM -lICE -lgtk -lgdk -lgmodule -lXext -lX11  \
 -lgnome -lgnomesupport -lesd -laudiofile -lm -ldb1 -lglib -ldl


idl_output_source = \
	scwm_scheme_evaluator-common.c \
	scwm_scheme_evaluator-stubs.c \
	scwm_scheme_evaluator-skels.c

$(idl_output_source): scwm_scheme_evaluator.idl
	orbit-idl $<

CFLAGS = -Wall -W @GTK_CFLAGS@ @CFLAGS@ `gnome-config --cflags gnome`

INCLUDES = -I$(top_srcdir)/scwm @x_cflags@ @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
BUILT_SOURCES = $(libscwmcorba_snarfs) $(idl_output_source)

SUFFIXES = .x

# just use `make CC=g++' and the revised guile-snarf which uses ${CC} will work
# for C++ prototype extraction
%.x: %.c
	guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@

%.x: %.cc
	CC=$(CXX) \
	guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CXXFLAGS) $< > $@

ETAGS_ARGS = --regex='/[ \t]*SCM_SYMBOL[ \t]*(\([^,]*\)/\1/' --regex='/[ \t]*SCWM_PROC[ \t]*(\([^,]*\),[^,]*/\1/'

scwm_corba_exec_SOURCES = \
	scwm-corba-exec.c \
	scwm_scheme_evaluator-common.c \
	scwm_scheme_evaluator-stubs.c

scwm_corba_exec_LDADD =  -lgnorba -lORBitCosNaming -lORBit -lIIOP -lORBitutil -lgnomeui -lart_lgpl -lgdk_imlib -lSM -lICE -lgtk -lgdk -lgmodule -lXext -lX11 -lgnome -lgnomesupport -lesd -laudiofile -lm -ldb1 -lglib -ldl


