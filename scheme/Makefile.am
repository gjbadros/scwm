## $Id$
## process this file with automake to create Makefile.in

scwm_common_scheme = advice.scm animation.scm auto-raise.scm base.scm \
    bincomm.scm cached-program-exists.scm cascade.scm decor.scm \
    desk-background.scm doc.scm face.scm file.scm flux.scm fvwm-compat.scm \
    fvwm-eval.scm fvwm-module.scm image-loaders.scm listops.scm \
    message-window.scm minimal.scm module-types.scm optargs.scm \
    prefs-menu.scm report-usage.scm shutdown-opts.scm sort.scm stacking.scm \
    std-menus.scm style-options.scm style.scm theme-impl.scm themes.scm \
    tile.scm user-options.scm wininfo.scm winlist.scm winops.scm \
    wmconfig-menu.scm placement.scm virtual.scm

scwm_gtk_scheme = gtk.scm ScwmClock.scm ScwmDeskButtons.scm

## To install
if HAVE_GTK
scwm_scheme_DATA = $(scwm_common_scheme) $(scwm_gtk_scheme)
else
scwm_scheme_DATA = $(scwm_common_scheme)
endif

## To distribute
EXTRA_DIST = minimal.scm $(scwm_common_scheme) $(scwm_gtk_scheme)


ETAGS_ARGS = $(EXTRA_DIST)
TAGS_DEPENDENCIES = $(EXTRA_DIST)
