## $Id$
## process this file with automake to create Makefile.in
## (You should run ./autogen.sh from the top level directory)

## GJB:FIXME:: adding a new theme here is error-prone

## Distribute and install
scwm_theme_DATA = fvwm2.@TAR_SUFFIX@ mwm.@TAR_SUFFIX@ win95.@TAR_SUFFIX@ \
                  woven.@TAR_SUFFIX@ gjb.@TAR_SUFFIX@ twm.@TAR_SUFFIX@ afterstep.@TAR_SUFFIX@ \
		  darkside.@TAR_SUFFIX@
EXTRA_DIST= ChangeLog $(srcdir)/fvwm2/* $(srcdir)/mwm/* $(srcdir)/win95/* \
           $(srcdir)/woven/* $(srcdir)/gjb/* $(srcdir)/twm/* $(srcdir)/afterstep/* \
	   $(srcdir)/darkside/*

## Themes must be removed by make distclean
## WARNING: this assume that the only files ending with
## .@TAR_SUFFIX@ are the themes files
DISTCLEANFILES=*.@TAR_SUFFIX@


fvwm2.@TAR_SUFFIX@: $(srcdir)/fvwm2 $(srcdir)/fvwm2/theme.scm
	abs_builddir_themes=`pwd`; \
	cd $(srcdir); \
	abs_srcdir_themes=`pwd`; \
	echo 'make[99]: Entering directory `'$$abs_srcdir_themes"'"; \
	$(TAR_CF) $${abs_builddir_themes}/fvwm2.@TAR_SUFFIX@ fvwm2
	echo 'make[99]: Leaving directory `'$$abs_top_srcdir"'"	

mwm.@TAR_SUFFIX@: $(srcdir)/mwm $(srcdir)/mwm/theme.scm
	abs_builddir_themes=`pwd`; \
	cd $(srcdir); \
	abs_srcdir_themes=`pwd`; \
	echo 'make[99]: Entering directory `'$$abs_srcdir_themes"'"; \
	$(TAR_CF) $${abs_builddir_themes}/mwm.@TAR_SUFFIX@ mwm
	echo 'make[99]: Leaving directory `'$$abs_top_srcdir"'"	

win95.@TAR_SUFFIX@: $(srcdir)/win95 $(srcdir)/win95/theme.scm
	abs_builddir_themes=`pwd`; \
	cd $(srcdir); \
	abs_srcdir_themes=`pwd`; \
	echo 'make[99]: Entering directory `'$$abs_srcdir_themes"'"; \
	$(TAR_CF) $${abs_builddir_themes}/win95.@TAR_SUFFIX@ win95
	echo 'make[99]: Leaving directory `'$$abs_top_srcdir"'"	

woven.@TAR_SUFFIX@: $(srcdir)/woven $(srcdir)/woven/theme.scm
	abs_builddir_themes=`pwd`; \
	cd $(srcdir); \
	abs_srcdir_themes=`pwd`; \
	echo 'make[99]: Entering directory `'$$abs_srcdir_themes"'"; \
	$(TAR_CF) $${abs_builddir_themes}/woven.@TAR_SUFFIX@ woven
	echo 'make[99]: Leaving directory `'$$abs_top_srcdir"'"	

gjb.@TAR_SUFFIX@: $(srcdir)/gjb $(srcdir)/gjb/theme.scm
	abs_builddir_themes=`pwd`; \
	cd $(srcdir); \
	abs_srcdir_themes=`pwd`; \
	echo 'make[99]: Entering directory `'$$abs_srcdir_themes"'"; \
	$(TAR_CF) $${abs_builddir_themes}/gjb.@TAR_SUFFIX@ gjb
	echo 'make[99]: Leaving directory `'$$abs_top_srcdir"'"	

twm.@TAR_SUFFIX@: $(srcdir)/twm $(srcdir)/twm/theme.scm
	abs_builddir_themes=`pwd`; \
	cd $(srcdir); \
	abs_srcdir_themes=`pwd`; \
	echo 'make[99]: Entering directory `'$$abs_srcdir_themes"'"; \
	$(TAR_CF) $${abs_builddir_themes}/twm.@TAR_SUFFIX@ twm
	echo 'make[99]: Leaving directory `'$$abs_top_srcdir"'"	

afterstep.@TAR_SUFFIX@: $(srcdir)/afterstep $(srcdir)/afterstep/theme.scm
	abs_builddir_themes=`pwd`; \
	cd $(srcdir); \
	abs_srcdir_themes=`pwd`; \
	echo 'make[99]: Entering directory `'$$abs_srcdir_themes"'"; \
	$(TAR_CF) $${abs_builddir_themes}/afterstep.@TAR_SUFFIX@ afterstep
	echo 'make[99]: Leaving directory `'$$abs_top_srcdir"'"	

darkside.@TAR_SUFFIX@: $(srcdir)/darkside $(srcdir)/darkside/theme.scm
	abs_builddir_themes=`pwd`; \
	cd $(srcdir); \
	abs_srcdir_themes=`pwd`; \
	echo 'make[99]: Entering directory `'$$abs_srcdir_themes"'"; \
	$(TAR_CF) $${abs_builddir_themes}/darkside.@TAR_SUFFIX@ darkside
	echo 'make[99]: Leaving directory `'$$abs_top_srcdir"'"	
