## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

scwm_module_LTLIBRARIES = libc-animation.la

libc_animation_la_SOURCES = c-animation.c
libc_animation_la_LDFLAGS = -export-dynamic

libc_animation_snarfs = c-animation.x

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ \
	   @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
BUILT_SOURCES = $(libc_animation_snarfs)
