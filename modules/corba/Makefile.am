## $Id$
## process this file with automake to create Makefile.in
## (You should run ./autogen.sh from the top level directory)

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

EXTRA_DIST=scwm_scheme_evaluator.idl

if HAVE_GNOME_CORBA
bin_PROGRAMS = scwm-corba-exec 
scwm_module_LTLIBRARIES = libscwmcorba.la
libscwmcorba_snarfs = scwmcorba.x

idl_output_source = \
	scwm_scheme_evaluator-common.c \
	scwm_scheme_evaluator-stubs.c \
	scwm_scheme_evaluator-skels.c

idl_output_headers = \
	scwm_scheme_evaluator.h

scwm_corba_exec_SOURCES = \
	scwm-corba-exec.c \
	scwm_scheme_evaluator-common.c \
	scwm_scheme_evaluator-stubs.c
endif

$(idl_output_source) $(idl_output_headers): scwm_scheme_evaluator.idl
	orbit-idl $<

libscwmcorba_la_SOURCES = \
	scwmcorba.c \
	scwm_scheme_evaluator-impl.c \
	scwm_scheme_evaluator-common.c \
	scwm_scheme_evaluator-skels.c

libscwmcorba_la_LDFLAGS = \
 -lgnorba -lORBitCosNaming -lORBit -lIIOP -lORBitutil -lgnomeui -lart_lgpl -lgdk_imlib \
 -lSM -lICE -lgtk -lgdk -lgmodule -lXext -lX11  \
 -lgnome -lgnomesupport -lesd -laudiofile -lm -ldb1 -lglib -ldl


CFLAGS = -Wall -W @GTK_CFLAGS@ @CFLAGS@ `gnome-config --cflags gnome`

INCLUDES = -I$(top_srcdir)/include -I$(SCWM_SRC_DIR) @x_cflags@ @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
BUILT_SOURCES = $(libscwmcorba_snarfs) $(idl_output_source)

scwm_corba_exec_LDADD = -lgnorba -lORBitCosNaming -lORBit -lIIOP -lORBitutil -lgnomeui -lart_lgpl -lgdk_imlib -lSM -lICE -lgtk -lgdk -lgmodule -lXext -lX11 -lgnome -lgnomesupport -lesd -laudiofile -lm -ldb1 -lglib -ldl

