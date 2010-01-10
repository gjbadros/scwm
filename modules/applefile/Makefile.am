## $Id$
## process this file with automake to create Makefile.in
## (You should run ./autogen.sh from the top level directory)

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS = $(CPPEXTRAFLAGS)
extensionsdir = $(GUILE_EXTENSIONS_DIR)

pkglib_LTLIBRARIES = libapplefile.la
libapplefile_la_SOURCES = applefile.c applesingledouble.c macbinary.c crc.c \
			  resource.c
noinst_PROGRAMS = make_crc_table #macbinary
make_crc_table_SOURCES = make_crc_table.c
BUILT_SOURCES = crc.c
noinst_HEADERS = applefile.h applefile_int.h applesingledouble.h \
                 macbinary.h crc.h resource.h
INCLUDES = -I$(SCWM_SRC_DIR) @x_cflags@ @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@ @PREFIX_INCLUDES@

#macbinary_SOURCES = macbinary.c crc.c

crc.c: make_crc_table
	./make_crc_table > crc.c

install-data-hook:
	$(mkinstalldirs) $(DESTDIR)$(scwm_schemedir)
	rm -f $(DESTDIR)$(scwm_schemedir)/libapplefile.la
	$(LN_S) $(pkglibdir)/libapplefile.la $(DESTDIR)$(scwm_schemedir)/libapplefile.la

uninstall-local:
	rm -f $(scwm_schemedir)/libapplefile.la
