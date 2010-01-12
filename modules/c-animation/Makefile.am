## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)
extensionsdir = $(GUILE_EXTENSIONS_DIR)

extensions_LTLIBRARIES = scwm-c-animation.la

scwm_c_animation_la_SOURCES = c-animation.c
scwm_c_animation_la_LDFLAGS = -module -export-dynamic -shared
scwm_c_animation_snarfs = c-animation.x

BUILT_SOURCES = $(scwm_c_animation_snarfs)
CLEANFILES = $(scwm_c_animation_snarfs)

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ \
	   @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
