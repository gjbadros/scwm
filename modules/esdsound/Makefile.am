# $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)
extensionsdir = $(GUILE_EXTENSIONS_DIR)

if HAVE_ESD
extensions_LTLIBRARIES = scwm-esdsound.la
scwm_esdsound_la_SOURCES = sound.c
scwm_esdsound_snarfs = sound.x

BUILT_SOURCES = $(scwm_esdsound_snarfs)
CLEANFILES = $(scwm_esdsound_snarfs)
endif

scwm_esdsound_la_LIBADD = @ESD_LIBS@
scwm_esdsound_la_LDFLAGS = -module -export-dynamic -shared

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ \
	   @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
