## $Id$
## process this file with automake to create Makefile.in

scwm_common_scheme = \
advice.scm animated-iconify.scm animation.scm auto-raise.scm \
base.scm bincomm.scm cascade.scm constraints.scm decor.scm \
defoption.scm desk-background.scm dir-focus.scm doc.scm edge-ops.scm \
face.scm file.scm flash-window.scm flux.scm \
fvwm-compat.scm fvwm-eval.scm fvwm-module.scm hover-focus.scm \
highlight-current-window.scm gnome-hints.scm \
group.scm image-loaders.scm key-mover-xtest.scm \
kde-hints.scm key-mover.scm listops.scm message-window.scm \
minimal.scm modifier-key-bindings.scm module-types.scm menus-extras.scm \
nonants.scm netscape.scm number-convert.scm optargs.scm \
path-cache.scm placement.scm preferences.scm prefs-menu.scm primopts.scm \
property-respond.scm quote.scm report-usage.scm \
rgb-database.scm send-string.scm \
shutdown-opts.scm simplebiff.scm simple-constraints.scm snap.scm sort.scm stacking.scm \
std-menus.scm stringops.scm style-options.scm style.scm stylist.scm \
theme-impl.scm themes.scm test-case.scm tile.scm time-convert.scm \
ui-constraints-buttons.scm ui-constraints-classes.scm ui-constraints-toggle-menu.scm \
ui-constraints-composition.scm ui-constraints.scm \
undo.scm virtual.scm window-locations.scm window-selection.scm \
window-configuration.scm \
wininfo.scm winlist.scm winops.scm wmconfig-menu.scm xprop-extras.scm

scwm_gtk_scheme = \
gtk.scm ScwmButtons.scm ScwmBiff.scm ScwmClock.scm ScwmDeskButtons.scm \
prompt-bool.scm prompt-color.scm prompt-file.scm prompt-font.scm \
prompt-range.scm prompt-string.scm prompt-proc.scm flux-gtk.scm \
ui-constraints-gdk-drawing.scm \
ui-constraints-gtk-toggle-menu.scm 

check:
	echo $(scwm_common_scheme) $(scwm_gtk_scheme) | perl -pe 's/\s+/\n/g' | sort > make.to-install
	echo *.scm | perl -pe 's/\s+/\n/g' | sort > make.exists
	diff make.exists make.to-install
	rm -f make.exists make.to-install

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
