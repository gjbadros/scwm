# $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

if HAVE_ESD
scwm_module_LTLIBRARIES = libesdsound.la

libesdsound_la_SOURCES = sound.c
libesdsound_snarfs = sound.x

BUILT_SOURCES = $(libesdsound_snarfs)
endif

libesdsound_la_LIBADD = @ESD_LIBS@
libesdsound_la_LDFLAGS = -export-dynamic

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ \
	   @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
