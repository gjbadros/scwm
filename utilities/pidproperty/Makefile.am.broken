## $Id$
## process this file with automake to create Makefile.in

AM_CPPFLAGS = $(CPPEXTRAFLAGS) -nostdlib

scwm_util_LTLIBRARIES = libpidproperty.la

libpidproperty_la_SOURCES = scwm_set_pid_property.c
libpidproperty_la_LDFLAGS = -nostdlib -export-dynamic

INCLUDES = -I$(top_srcdir)/scwm @x_cflags@ @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@
