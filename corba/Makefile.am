## $Id$
## process this file with automake to create Makefile.in
## (You should run ./autogen.sh from the top level directory)

bin_PROGRAMS = scwm-corba-exec scwm_scheme_evaluator-server

idl_output_source = \
	scwm_scheme_evaluator-common.c \
	scwm_scheme_evaluator-stubs.c \
	scwm_scheme_evaluator-skels.c

#	scwm_scheme_evaluator-impl.c

$(idl_output_source): scwm_scheme_evaluator.idl
	orbit-idl $<

#	orbit-idl --skeleton_impl $<

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

CFLAGS = @GTK_CFLAGS@ @CFLAGS@ `gnome-config --cflags gnome`

BUILT_SOURCES = $(idl_output_source)

scwm_corba_exec_SOURCES = \
	scwm-corba-exec.c \
	scwm_scheme_evaluator-common.c \
	scwm_scheme_evaluator-stubs.c

scwm_corba_exec_LDADD =  -lgnorba -lORBitCosNaming -lORBit -lIIOP -lORBitutil -lgnomeui -lart_lgpl -lgdk_imlib -lSM -lICE -lgtk -lgdk -lgmodule -lXext -lX11 -lgnome -lgnomesupport -lesd -laudiofile -lm -ldb1 -lglib -ldl


scwm_scheme_evaluator_server_SOURCES = \
	scwm_scheme_evaluator-server.c \
	scwm_scheme_evaluator-common.c \
	scwm_scheme_evaluator-skels.c

scwm_scheme_evaluator_server_LDADD =  -lgnorba -lORBitCosNaming -lORBit -lIIOP -lORBitutil -lgnomeui -lart_lgpl -lgdk_imlib -lSM -lICE -lgtk -lgdk -lgmodule -lXext -lX11 -lgnome -lgnomesupport -lesd -laudiofile -lm -ldb1 -lglib -ldl

#-lgnorba -lgnome -lORBitCosNaming @GTK_LIBS@ -lIIOP -lORBit -lORBitutil -lgnomesupport -lesd

