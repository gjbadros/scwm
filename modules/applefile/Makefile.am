pkglib_LTLIBRARIES = libapplefile.la
libapplefile_la_SOURCES = applefile.c applesingledouble.c macbinary.c crc.c \
			  resource.c
noinst_PROGRAMS = make_crc_table #macbinary
make_crc_table_SOURCES = make_crc_table.c
BUILT_SOURCES = crc.c
noinst_HEADERS = applefile.h applefile_int.h applesingledouble.h \
                 macbinary.h crc.h resource.h
INCLUDES = -I$(top_srcdir)/scwm @x_cflags@ @GUILE_INCLUDES@ @CASSOWARY_INCLUDES@

#macbinary_SOURCES = macbinary.c crc.c

crc.c: make_crc_table
	./make_crc_table > crc.c

install-data-hook:
	$(mkinstalldirs) $(scwm_moduledir)
	rm -f $(scwm_moduledir)/libapplefile.la
	$(LN_S) $(pkglibdir)/libapplefile.la $(scwm_moduledir)/libapplefile.la

uninstall-local:
	rm -f $(scwm_moduledir)/libapplefile.la
