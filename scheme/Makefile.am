## $Id$
## process this file with automake to create Makefile.in

scwm_common_scheme = auto-raise.scm base.scm bincomm.scm decor.scm doc.scm \
	 face.scm flux.scm fvwm-compat.scm fvwm-eval.scm fvwm-module.scm \
	 image-loaders.scm module-types.scm optargs.scm prefs-menu.scm \
	 sort.scm std-menus.scm style-options.scm style.scm \
	 wininfo.scm winlist.scm winops.scm wmconfig-menu.scm \
	 user-options.scm cached-program-exists.scm listops.scm file.scm \
	 animation.scm themes.scm theme-impl.scm report-usage.scm 
	 shutdown-opts.scm

scwm_gtk_scheme = gtk.scm ScwmClock.scm ScwmDeskButtons.scm

## Distribute and install
if HAVE_GTK
scwm_scheme_DATA = $(scwm_common_scheme) $(scwm_gtk_scheme)
else
scwm_scheme_DATA = $(scwm_common_scheme)
endif


## Distribute only
EXTRA_DIST = minimal.scm $(scwm_common_scheme) $(scwm_gtk_scheme)
ETAGS_ARGS = $(EXTRA_DIST)
TAGS_DEPENDENCIES = $(EXTRA_DIST)
