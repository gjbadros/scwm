## $Id$
## process this file with automake to create Makefile.in

## Distribute and install
scwm_theme_DATA = fvwm2.@TAR_SUFFIX@ mwm.@TAR_SUFFIX@ win95.@TAR_SUFFIX@
EXTRA_DIST=$(srcdir)/fvwm2/* $(srcdir)/mwm/* $(srcdir)/win95/*

fvwm2.@TAR_SUFFIX@: $(srcdir)/fvwm2 $(srcdir)/fvwm2/*
	abs_builddir_themes=`pwd`; \
	cd $(srcdir); \
	abs_srcdir_themes=`pwd`; \
	echo 'make[99]: Entering directory `'$$abs_srcdir_themes"'"; \
	$(TAR_CF) $${abs_builddir_themes}/fvwm2.@TAR_SUFFIX@ fvwm2
	echo 'make[99]: Leaving directory `'$$abs_top_srcdir"'"	

mwm.@TAR_SUFFIX@: $(srcdir)/mwm $(srcdir)/mwm/*
	abs_builddir_themes=`pwd`; \
	cd $(srcdir); \
	abs_srcdir_themes=`pwd`; \
	echo 'make[99]: Entering directory `'$$abs_srcdir_themes"'"; \
	$(TAR_CF) $${abs_builddir_themes}/mwm.@TAR_SUFFIX@ mwm
	echo 'make[99]: Leaving directory `'$$abs_top_srcdir"'"	

win95.@TAR_SUFFIX@: $(srcdir)/win95 $(srcdir)/win95/*
	abs_builddir_themes=`pwd`; \
	cd $(srcdir); \
	abs_srcdir_themes=`pwd`; \
	echo 'make[99]: Entering directory `'$$abs_srcdir_themes"'"; \
	$(TAR_CF) $${abs_builddir_themes}/win95.@TAR_SUFFIX@ win95
	echo 'make[99]: Leaving directory `'$$abs_top_srcdir"'"	


