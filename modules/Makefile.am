## $Id$
## prcoess this file with automake to create Makefile.in

if HAVE_GTK
GTK_SUBDIRS = scwmgtkhelper
else
GTK_SUBDIRS =
endif

if HAVE_ESD
ESD_SUBDIRS = esdsound
else
ESD_SUBDIRS =
endif

if HAVE_PROPLIST
PROPLIST_SUBDIRS = proplist
else
PROPLIST_SUBDIRS =
endif

SUBDIRS = background \
	c-animation \
	overlay-plane \
	pie-menus \
	xlib-drawing \
	xpm-menus \
	$(ESD_SUBDIRS) \
	$(PROPLIST_SUBDIRS) \
	$(GTK_SUBDIRS)

DIST_SUBDIRS = $(SUBDIRS)
