## $Id$
## process this file with automake to create Makefile.in

## Distribute and install
scwm_scheme_DATA = auto-raise.scm base.scm bincomm.scm decor.scm doc.scm \
	 face.scm flux.scm fvwm-compat.scm fvwm-eval.scm fvwm-module.scm \
	 image-loaders.scm module-types.scm optargs.scm prefs-menu.scm \
	 sort.scm std-menus.scm style-options.scm style.scm \
	 wininfo.scm winlist.scm winops.scm wmconfig-menu.scm \
	 user-options.scm cached-program-exists.scm listops.scm file.scm \
	 animation.scm themes.scm theme-impl.scm

## Distribute only
EXTRA_DIST = minimal.scm
ETAGS_ARGS = $(scwm_scheme_DATA) $(EXTRA_DIST)
TAGS_DEPENDENCIES = $(scwm_scheme_DATA) $(EXTRA_DIST)
