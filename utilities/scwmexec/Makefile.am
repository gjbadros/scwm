## $Id$
## process this file with automake to create Makefile.in
## (You should run ./autogen.sh from the top level directory)

EXTRA_DIST = ChangeLog

bin_PROGRAMS = scwmexec
scwmexec_SOURCES = scwmexec.c
scwmexec_LDADD = $(top_builddir)/utilities/libscwmexec/libscwmexec.la \
                  @x_ldflags@ @x_libs@
INCLUDES = @x_cflags@
