## $Id$
## process this file with automake to create Makefile.in
## (You should run ./autogen.sh from the top level directory)

EXTRA_DIST = ChangeLog

lib_LTLIBRARIES = libscwmexec.la
include_HEADERS = scwmexec.h

libscwmexec_la_SOURCES = libmain.c
libscwmexec_la_LDFLAGS = -version-info 0:0:0 @x_ldflags@ @x_libs@

INCLUDES = @x_cflags@
