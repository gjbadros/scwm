## $Id$
## process this file with automake to create Makefile.in
## (You should run ./autogen.sh from the top level directory)

EXTRA_DIST = ChangeLog

bin_PROGRAMS = scwmrepl
scwmrepl_SOURCES = scwmrepl.c
scwmrepl_LDADD = $(top_builddir)/utilities/libscwmexec/libscwmexec.la \
                  @x_ldflags@ @x_libs@ @READLINE_LIB@
INCLUDES = @x_cflags@ @GUILE_INCLUDES@
