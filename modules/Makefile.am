## $Id$
## process this file with automake to create Makefile.in

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

SUBDIRS = background \
	c-animation \
	$(ESD_SUBDIRS) \
	overlay-plane \
	pie-menus \
	xlib-drawing \
	xpm-menus \
	$(GTK_SUBDIRS)

DIST_SUBDIRS = $(SUBDIRS)
