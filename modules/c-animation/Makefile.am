## $Id$
## process this file with automake to create Makefile.in

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)

BUILT_SOURCES = c-animation.x

scwm_module_LTLIBRARIES = libscwm-c-animation.la

libscwm_c_animation_la_SOURCES = c-animation.c
libscwm_c_animation_la_LDFLAGS = -export-dynamic

libc_animation_snarfs = 

INCLUDES = -I$(SCWM_SRC_DIR) -I$(top_srcdir)/include @x_cflags@ \
	   @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@
