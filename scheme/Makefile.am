## $Id$
## process this file with automake to create Makefile.in

scwm_common_scheme = \
ScwmClock.scm ScwmDeskButtons.scm advice.scm animate-iconify-window.scm animation.scm ask-string.scm auto-raise.scm base.scm bincomm.scm cached-program-exists.scm cascade.scm constraints.scm decor.scm desk-background.scm doc.scm face.scm file.scm flux.scm fvwm-compat.scm fvwm-eval.scm fvwm-module.scm gtk.scm image-loaders.scm kwm-emulation.scm listops.scm message-window.scm minimal.scm modifier-key-bindings.scm module-types.scm optargs.scm placement.scm prefs-menu.scm report-usage.scm shutdown-opts.scm simple-constraints.scm sort.scm stacking.scm std-menus.scm string-prompt.scm style-options.scm style.scm theme-impl.scm themes.scm tile.scm ui-constraints-buttons.scm ui-constraints-classes.scm ui-constraints-toggle-menu.scm ui-constraints-toolbar.scm ui-constraints.scm user-options.scm virtual.scm window-locations.scm wininfo.scm winlist.scm winops.scm wmconfig-menu.scm

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
