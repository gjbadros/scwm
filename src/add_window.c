/* $Id$
 * add_window.c
 * (C) 1999 Greg J. Badros and Maciej Stachowiak
 */

/* This module is derived from code based on fvwm2 which was in turn
 * based on code derived from Twm.  It was significantly modified by
 * Rob Nation for fvwm2 */

/* An old copyright that we may still be required to have around... --gjb */
/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* #define SCWM_DEBUG_MSGS */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xresource.h>
#include <X11/Xutil.h>
#ifdef HAVE_SHAPE
#include <X11/extensions/shape.h>
#endif

#include <guile/gh.h>

#define ADD_WINDOW_IMPLEMENTATION
#include "add_window.h"

#include "scwm.h"
#include "screen.h"
#include "binding.h"
#include "window.h"
#include "cursor.h"
#include "decorations.h"
#include "Grab.h"
#include "colors.h"
#include "events.h"
#include "borders.h"
#include "resize.h"
#include "focus.h"
#include "colormaps.h"
#include "image.h"
#include "module-interface.h"
#include "icons.h"
#include "placement.h"
#include "callbacks.h"
#include "cursor.h"
#include "decor.h"
#ifdef HAVE_LIBSM_LIBICE
#include "session-manager.h"
#endif
#include "xmisc.h"
#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

#undef SCWM_DEBUG_ADDWINDOW
#undef SCWM_DEBUG_ADDWINDOW_VERBOSE

#ifdef SCWM_DEBUG_ADDWINDOW
#  define DBUG_ADDWINDOW(X) scwm_msg X
#else
#  define DBUG_ADDWINDOW(X)		/* no messages */
#endif

#ifdef SCWM_DEBUG_ADDWINDOW_VERBOSE
#  define DBUG_ADDWINDOW_VERBOSE(X) scwm_msg X
#else
#  define DBUG_ADDWINDOW_VERBOSE(X)		/* no messages */
#endif



SCWM_HOOK(before_new_window_hook, "before-new-window-hook", 1,
"This hook is invoked when first creating a new window object.
Its procedures are each called with a single argument, WIN, the new
window.  Only a subset of the usual window parameters should be set
here, in particular, those that control what hints will be respected
for this window, and those that control how the window will be placed.

This hook does not typically need to be used directly by the user;
`window-style' from the \"(app scwm style)\" module provides a convenient
interface to setting the relevant parameters when a new window is
created.

See also `before-place-new-window-hook' and `after-new-window-hook'. ");

SCWM_HOOK(before_place_new_window_hook, "before-place-new-window-hook", 1,
"This hook is invoked just before placing a new window.
It comes after `before-new-window-hook', but before `after-new-window-hook'. 
Its procedures are each called with a single argument, WIN, the new window.
This hook may be removed later since it is mostly redundant with the other
two hooks, despite being invoked at a slightly different time. 

See also `before-new-window-hook' and `after-new-window-hook'.");

SCWM_HOOK(after_new_window_hook, "after-new-window-hook", 1,
"This hook is invoked after a window is created and placed.
Its procedures are each called with a single argument, WIN, the new window.
Any window operations may be performed at this time. However, it is
recommended that placement-related operations, such as setting the
position, desk, viewport location and z-ordering of a window be done
in the placement procedure instead.  It should be used for setting
window styles, as the window geometry needs to be fully and correctly
specified before the window is placed.  The `window-style' mechanism
from the \"(app scwm style)\" module provides a convenient interface to
setting the relevant parameters when a new window is created. 

See also `before-new-window-hook' and `before-place-new-window-hook'.");
       
SCWM_HOOK(window_close_hook,"window-close-hook",1,
"This hook is invoked whenever a scwm-managed window is closed.
It is invoked on deletes, destroys, or for any reason that a window
is closed. The hook procedures are invoked with one argument,
WIN, the window being closed.  The WIN is still valid during the hook
procedures.");


/* This global is True iff Cassowary's resolve hook
   is being processed;  when that's happening, we don't
   want to use Cassowary to move windows, but want to move
   them directly, e.g., in animate-windows of c-animation.c */
Bool fInResolveHook = False;


/* GJB:FIXME:: instead of placeholder empty functions,
   pointers to functions should be used, and init_constraint_primitives should
   set the pointers to point to functions that it dynamically loads */
#ifndef USE_CASSOWARY
void CassowarySetCValuesAndSolve(ScwmWindow *psw, int fSolve)  { /* empty */ }
void CassowaryInitClVarsInPsw(ScwmWindow *psw) { /* empty */ }
void CassowaryInitClVarsInPscreen(ScreenInfo *pscreen) { /* empty */ }
void CassowaryNewWindow(ScwmWindow *psw) { /* empty */ }
void CassowaryCloseWindow(ScwmWindow *psw) { /* empty */ }
void CassowaryEditPosition(ScwmWindow *psw) { /* empty */ }
void CassowaryEditSize(ScwmWindow *psw) { /* empty */ }
/* x,y are virtual positions */
void SuggestMoveWindowTo(ScwmWindow *psw, int x, int y, Bool fOpaque) {
  SetScwmWindowPosition(psw,x,y,fOpaque);
}
/* x,y are virtual positions */
Bool SuggestSizeWindowTo(ScwmWindow *psw, int x, int y, int w, int h, Bool fOpaque) {
  return SetScwmWindowGeometry(psw,x,y,w,h, fOpaque);
}
/* from virtual.h */
void MoveViewport_internal(int newx, int newy);

void ChangeVirtualPosition(int vx, int vy) {
  MoveViewport_internal(vx,vy);
}

void CassowaryModifyOpaqueFlag(Bool *pfOpaque) { /* empty */ }

void CassowaryEndEdit(ScwmWindow *psw) {
  if (psw) ResizePswToCurrentSize(psw);
}
#endif


/* Used by Xrm */
static XrmOptionDescRec table[] =
{
  /* Want to accept "-workspace N" or -xrm "scwm*desk:N" as options
   * to specify the desktop. I have to include dummy options that
   * are meaningless since Xrm seems to allow -w to match -workspace
   * if there would be no ambiguity. */
  {"-workspacf", "*junk", XrmoptionSepArg, (caddr_t) NULL},
  {"-workspace", "*desk", XrmoptionSepArg, (caddr_t) NULL},
  {"-xrn", NULL, XrmoptionResArg, (caddr_t) NULL},
  {"-xrm", NULL, XrmoptionResArg, (caddr_t) NULL},
};

extern int restart_vp_offset_x, restart_vp_offset_y;

/*
 * AddWindow - add a new window to the scwm list
 *   Note that this gets called both at startup and upon recapturing
 *   Also note that we don't really have to be all that careful about
 *     decoration window placement or sizing -- SetupFrame takes care of all that stuff
 *     This code should be cleaned up to eliminate redundant (and misleading)
 *      computations from AddWindow (that are done necessarily in SetupFrame, too)
 *
 *  Returned Value:
 *	(ScwmWindow *) - pointer to the ScwmWindow structure
 *
 *  Inputs:
 *	w	- the window id of the window to add
 *
 * N.B. This code is pretty tightly coupled to DestroyScwmWindow, below
 */
ScwmWindow *
AddWindow(Window w)
{
#define FUNC_NAME "AddWindow"
  ScwmWindow *psw;		/* new scwm window structure */
  unsigned long valuemask;	/* mask for create windows */
  SCM schwin; /* To make sure it's on the stack to be marked. */

  Pixmap TexturePixmap = None, TexturePixmapSave = None;
  unsigned long valuemask_save = 0;

  XSetWindowAttributes attributes;	/* attributes for create windows */
  int i;

  int border_width = 0, resize_width = 0;
  extern ScwmWindow *colormap_win;
  int client_argc;
  char **client_argv = NULL, *str_type;
  Bool status;
  XrmValue rm_value;
  XTextProperty text_prop;
  extern Bool PPosOverride;
  int frame_x, frame_y;
  int frame_width, frame_height;

  /* allocate space for the scwm window */
  psw = NEW(ScwmWindow);

  if (!psw) {
    return NULL;
  }
  psw->w = w;
  ResetAllFlags(psw);
  psw->fBorderWidthSet = False;

  psw->icon_cursor=SCM_UNDEFINED;

  psw->cmap_windows = NULL;
  psw->number_cmap_windows = 0;
  psw->highlighted_nonant = SCWM_NONANT_NONE;

  psw->ttCreated = 
    psw->ttLastFocussed = time(NULL);

  psw->timeLastFocussed = lastTimestamp;
  psw->visibility = VisibilityUnobscured;

  if (!PPosOverride && !FXWindowAccessible(dpy,psw->w)) {
    FREE(psw);
    return (NULL);
  }

  psw->name = NoName;

  if (XGetWMName(dpy, psw->w, &text_prop) != 0) {
    psw->name = SzExtractTextPropValue(&text_prop);
  }

  /* removing NoClass change for now... */
  psw->classhint.res_name = NoResource;
  psw->classhint.res_class = NoClass;
  XGetClassHint(dpy, psw->w, &psw->classhint);
  if (psw->classhint.res_name == NULL)
    psw->classhint.res_name = NoResource;
  if (psw->classhint.res_class == NULL)
    psw->classhint.res_class = NoClass;

  FetchWmProtocols(psw);
  FetchWmColormapWindows(psw);
  if (!(XGetWindowAttributes(dpy, psw->w, &(psw->attr))))
    psw->attr.colormap = Scr.ScwmRoot.attr.colormap;

  psw->wmhints = XGetWMHints(dpy, psw->w);

  psw->fTransient =
    (XGetTransientForHint(dpy, psw->w, &psw->transientfor));

  psw->old_bw = psw->attr.border_width;

#ifdef HAVE_SHAPE
  if (ShapesSupported) {
    int xws, yws, xbs, ybs;
    unsigned wws, hws, wbs, hbs;
    int boundingShaped, clipShaped;

    XShapeSelectInput(dpy, psw->w, ShapeNotifyMask);
    XShapeQueryExtents(dpy, psw->w,
		       &boundingShaped, &xws, &yws, &wws, &hws,
		       &clipShaped, &xbs, &ybs, &wbs, &hbs);
    psw->fShaped = boundingShaped;
  }
#endif


  /* if the window is in the NoTitle list, or is a transient,
   *  dont decorate it.
   * If its a transient, and DecorateTransients was specified,
   *  decorate anyway
   */
  psw->icon_image = SCM_BOOL_F;
  psw->icon_req_image = SCM_BOOL_F;
  psw->mini_icon_image = SCM_BOOL_F;

  /* MS:FIXME:: bletcherous... we process the hint properties separately,
     since hints need to be processed early, but some procs we may
     want to pass alter the size of the window &c. Will find a better
     way to deal with this - probably a reprocesshints function of
     some kind. */

  ResetCommonFlags(psw);
  psw->fTitle = True;
  psw->fBorder = True;

  /* MS:FIXME: need to find better way to ensure colors are valid before
     window comes under GC control. */

  psw->TextColor = Scr.NotMenuColors.fg;
  psw->ReliefColor = Scr.NotMenuRelief.fg;
  psw->ShadowColor = Scr.NotMenuRelief.bg;
  psw->BackColor = Scr.NotMenuColors.bg;
  psw->HiTextColor = SCM_BOOL_F;
  psw->HiReliefColor = SCM_BOOL_F;
  psw->HiShadowColor = SCM_BOOL_F;
  psw->HiBackColor = SCM_BOOL_F;

  psw->fl = &Scr.DefaultDecor;
  DECORREF(Scr.DefaultDecor.scmdecor);
  psw->buttons = 0;

  GetMwmHints(psw);
  GetOlHints(psw);

  GetWindowSizeHints(psw);

  /* create the scheme-level window */
  psw->schwin = schwin = make_window(psw);

  /* and initialize constraint structure hanging off of psw
     (uses the scheme window so must come after the make_window assignment
     above) */
  CassowaryInitClVarsInPsw(psw);


  call1_hooks(before_new_window_hook, psw->schwin);

  SelectDecor(psw, border_width, resize_width);

  DBUG_ADDWINDOW((DBG,FUNC_NAME,"fTitle = %d, th = %d", psw->fTitle, psw->title_height));

#ifdef HAVE_LIBSM_LIBICE
  restoreWindowState(psw);
#endif

  if (psw->fStartsOnDesk) {
    DBUG_ADDWINDOW((DBG,FUNC_NAME,"fStartsOnDesk is true"));
  }

  /* GJB:FIXME:: need to provide more flexibility in how the
     icon gets selected */
  /* find a suitable icon pixmap */

  psw->xboundary_width = psw->boundary_width;
  if (psw->fMWMBorders) psw->bw = 0;
  else psw->bw = 0;

  /* Tentative size estimate */
  frame_x = 0;
  frame_y = 0;
  frame_width = psw->attr.width + 2 * psw->xboundary_width;
  frame_height = (psw->attr.height + (psw->fTitle ? (psw->title_height-1) : 1)
		  + 2 * psw->boundary_width);

  ConstrainSize(psw, 0, 0, &frame_width, &frame_height);

  /* Find out if the client requested a specific desk on the command line. */
  if (XGetCommand(dpy, psw->w, &client_argv, &client_argc)) {
    /* Used to parse command line of clients for specific desk requests. */
    /* Todo: check for multiple desks. */
    XrmDatabase db = NULL;
    XrmParseCommand(&db, table, 4, "scwm", &client_argc, client_argv);
    status = XrmGetResource(db, "scwm.desk", "Scwm.Desk", &str_type, &rm_value);
    if ((status == True) && (rm_value.size != 0)) {
      psw->StartDesk = atoi(rm_value.addr);
      psw->fStartsOnDesk = True;
    }
    XrmDestroyDatabase(db);
  }

  /*
   * Make sure the client window still exists.  We don't want to leave an
   * orphan frame window if it doesn't.  Since we now have the server
   * grabbed, the window can't disappear later without having been
   * reparented, so we'll get a DestroyNotify for it.  We won't have
   * gotten one for anything up to here, however.
   */
  XGrabServer_withSemaphore(dpy); 
  if (!FXWindowAccessible(dpy,w)) {
    invalidate_window(psw->schwin);
    FREE(psw);
    XUngrabServer_withSemaphore(dpy);
    return (NULL);
  }
  XSetWindowBorderWidth(dpy, psw->w, 0);

  psw->icon_name = NULL;
  if ( XGetWMIconName(dpy, psw->w, &text_prop) != 0) {
    psw->icon_name = SzExtractTextPropValue(&text_prop);
    /* above may still return NULL */
  }
  if (psw->icon_name == NULL)
    psw->icon_name = psw->name;

  psw->fIconified = False;
  psw->fIconUnmapped = False;

  /* add the window into the scwm list */
  psw->next = Scr.ScwmRoot.next;
  if (Scr.ScwmRoot.next != NULL)
    Scr.ScwmRoot.next->prev = psw;
  psw->prev = &Scr.ScwmRoot;
  Scr.ScwmRoot.next = psw;

  /* create windows */
  valuemask = CWBorderPixel | CWCursor | CWEventMask;
  if (Scr.d_depth < 2) {
    attributes.background_pixmap = Scr.light_gray_pixmap;
    if (psw->fSticky)
      attributes.background_pixmap = Scr.sticky_gray_pixmap;
    valuemask |= CWBackPixmap;
  } else {
    attributes.background_pixel = SAFE_COLOR(psw->BackColor);
    valuemask |= CWBackPixel;
  }

  attributes.border_pixel = SAFE_COLOR(psw->ShadowColor);

  psw->frame_cursor = get_scm_cursor_by_number(XC_top_left_arrow);
  attributes.cursor = XCURSOR(psw->frame_cursor);
  attributes.event_mask = (SubstructureRedirectMask | ButtonPressMask |
			   ButtonReleaseMask | EnterWindowMask |
			   LeaveWindowMask | ExposureMask | VisibilityChangeMask );

  if ((GET_DECOR(psw, BorderStyle.inactive->style) & ButtonFaceTypeMask)
      == TiledPixmapButton)
    TexturePixmap = IMAGE (GET_DECOR(psw, 
				    BorderStyle.inactive->u.image))->image;

  if (TexturePixmap) {
    TexturePixmapSave = attributes.background_pixmap;
    attributes.background_pixmap = TexturePixmap;
    valuemask_save = valuemask;
    valuemask = (valuemask & ~CWBackPixel) | CWBackPixmap;
  }

  DBUG_ADDWINDOW((DBG,FUNC_NAME,"Now fTitle = %d, th = %d", psw->fTitle, psw->title_height));

  /* What the heck, we'll always reparent everything from now on! */
  DBUG_ADDWINDOW((DBG,FUNC_NAME,"Creating child of root window: %d %d, %d x %d, %d",
                  frame_x,frame_y,frame_width,frame_height,psw->bw));

  psw->frame =
    XCreateWindow(dpy, Scr.Root, frame_x, frame_y,
                  frame_width, frame_height,
		  psw->bw, CopyFromParent, InputOutput,
		  CopyFromParent,
		  valuemask,
		  &attributes);
  /* turn off VisibilityNotify event for Parent window */
  attributes.event_mask &= ~VisibilityChangeMask;
  ScwmSaveContextPsw(dpy, psw->frame, psw);

  if (TexturePixmap) {
    attributes.background_pixmap = TexturePixmapSave;
    valuemask = valuemask_save;
  }

  attributes.save_under = False;

  /* create Parent window which is the direct parent
     of the client window and has exactly the same width
     and height as the client window (keeps more applications
     happy).  This Parent window is the child of the frame window, and
     holds the client window. --07/27/98 gjb */
  attributes.cursor = XCURSOR(psw->frame_cursor);
  DBUG_ADDWINDOW((DBG,FUNC_NAME,"Creating child of frame: %d %d, %d x %d, %d",
                  psw->boundary_width, psw->boundary_width + psw->title_height,
                  psw->attr.width, psw->attr.height, psw->bw));
  psw->Parent =
    XCreateWindow(dpy, psw->frame,
		  psw->boundary_width, 
		  psw->boundary_width + psw->title_height,
                  psw->attr.width, psw->attr.height, psw->bw, 
                  CopyFromParent, InputOutput, CopyFromParent, 
                  valuemask, &attributes);
  ScwmSaveContextPsw(dpy,psw->Parent,psw);

  attributes.event_mask = (ButtonPressMask | ButtonReleaseMask | ExposureMask |
			   EnterWindowMask | LeaveWindowMask);
  psw->title_x = psw->title_y = 0;
  psw->title_w = 0;
  psw->title_width = frame_width - 2 * psw->corner_width - 3 + psw->bw;
  if (psw->title_width < 1)
    psw->title_width = 1;

  psw->title_height = 1;
  if (SHOW_TITLE_P(psw)) {
    psw->title_height = GET_DECOR(psw, TitleHeight) + psw->bw;
    DBUG_ADDWINDOW((DBG,FUNC_NAME,"Set height to %d",psw->title_height));
    if (psw->title_height < 1)
      psw->title_height = 1;
  }

  if (psw->fBorder) {
    DBUG_ADDWINDOW((DBG,FUNC_NAME,"Has border"));

    if (TexturePixmap) {
      TexturePixmapSave = attributes.background_pixmap;
      attributes.background_pixmap = TexturePixmap;
      valuemask_save = valuemask;
      valuemask = (valuemask & ~CWBackPixel) | CWBackPixmap;
    }
    /* Just dump the decoration windows at 0,0 and
       let SetupFrame move them to their appropriate positions */
    for (i = 0; i < 4; i++) {
      unsigned int cursors[4]={
	XC_top_left_corner,
	XC_top_right_corner,
	XC_bottom_left_corner,
	XC_bottom_right_corner
      };
      psw->corner_cursors[i]=get_scm_cursor_by_number(cursors[i]);
      attributes.cursor = XCURSOR(psw->corner_cursors[i]);
      psw->corners[i] =
	XCreateWindow(dpy, psw->frame, 0, 0,
		      psw->corner_width, psw->corner_width,
		      0, CopyFromParent, InputOutput,
		      CopyFromParent,
		      valuemask,
		      &attributes);
      ScwmSaveContextPsw(dpy, psw->corners[i], psw);
    }
    if (TexturePixmap) {
      attributes.background_pixmap = TexturePixmapSave;
      valuemask = valuemask_save;
    }
  }

  /* We always create the title bar since we can dynamically show or hide it */
  psw->title_x = psw->boundary_width + psw->title_height + 1;
  psw->title_y = psw->boundary_width;
  psw->title_cursor=get_scm_cursor_by_number(XC_top_left_arrow);
  attributes.cursor = XCURSOR(psw->title_cursor);
  DBUG_ADDWINDOW((DBG,FUNC_NAME,"Creating title window: %d %d, %d x %d",
                  psw->title_x, psw->title_y,
                  psw->title_width, psw->title_height));
  psw->title_w =
    XCreateWindow(dpy, psw->frame, psw->title_x, psw->title_y,
                  psw->title_width, psw->title_height, 0,
                  CopyFromParent, InputOutput, CopyFromParent,
                  valuemask, &attributes);
  ScwmSaveContextPsw(dpy, psw->title_w, psw);

  psw->sys_cursor = get_scm_cursor_by_number(XC_hand2);
  attributes.cursor = XCURSOR(psw->sys_cursor);
  for (i = 4; i >= 0; i--) {
    if ((i < Scr.nr_left_buttons) && (psw->left_w[i] > 0)) {
      if (TexturePixmap
          && GET_DECOR(psw, left_buttons[i].flags) & UseBorderStyle) {
        TexturePixmapSave = attributes.background_pixmap;
        attributes.background_pixmap = TexturePixmap;
        valuemask_save = valuemask;
        valuemask = (valuemask & ~CWBackPixel) | CWBackPixmap;
      }
      DBUG_ADDWINDOW((DBG,FUNC_NAME,"Creating left button %d",i));
      psw->left_w[i] =
        XCreateWindow(dpy, psw->frame, psw->title_height * i, 0,
                      psw->title_height, psw->title_height, 0,
                      CopyFromParent, InputOutput,
                      CopyFromParent,
                      valuemask,
                      &attributes);
      ScwmSaveContextPsw(dpy, psw->left_w[i], psw);
      if (TexturePixmap
          && GET_DECOR(psw, left_buttons[i].flags) & UseBorderStyle) {
        attributes.background_pixmap = TexturePixmapSave;
        valuemask = valuemask_save;
      }
    } else
      psw->left_w[i] = None;
    
    if ((i < Scr.nr_right_buttons) && (psw->right_w[i] > 0)) {
      if (TexturePixmap
          && GET_DECOR(psw, right_buttons[i].flags) & UseBorderStyle) {
        TexturePixmapSave = attributes.background_pixmap;
        attributes.background_pixmap = TexturePixmap;
        valuemask_save = valuemask;
        valuemask = (valuemask & ~CWBackPixel) | CWBackPixmap;
      }
      DBUG_ADDWINDOW_VERBOSE((DBG,FUNC_NAME,"Creating right button %d",i));
      psw->right_w[i] =
        XCreateWindow(dpy, psw->frame,
                      psw->title_width -
                      psw->title_height * (i + 1),
                      0, psw->title_height,
                      psw->title_height,
                      0, CopyFromParent, InputOutput,
                      CopyFromParent,
                      valuemask,
                      &attributes);
      ScwmSaveContextPsw(dpy, psw->right_w[i], psw);
      if (TexturePixmap
          && GET_DECOR(psw, right_buttons[i].flags) & UseBorderStyle) {
        attributes.background_pixmap = TexturePixmapSave;
        valuemask = valuemask_save;
      }
    } else
      psw->right_w[i] = None;
  }

  if (psw->fBorder) {
    if (TexturePixmap) {
      TexturePixmapSave = attributes.background_pixmap;
      attributes.background_pixmap = TexturePixmap;
      valuemask_save = valuemask;
      valuemask = (valuemask & ~CWBackPixel) | CWBackPixmap;
    }
    for (i = 0; i < 4; i++) {
      unsigned int cursors[4]={
	XC_top_side,
	XC_right_side,
	XC_bottom_side,
	XC_left_side
      };
      psw->side_cursors[i]=get_scm_cursor_by_number(cursors[i]);
      attributes.cursor = XCURSOR(psw->side_cursors[i]);
      DBUG_ADDWINDOW_VERBOSE((DBG,FUNC_NAME,"Creating side %d",i));
      psw->sides[i] =
	XCreateWindow(dpy, psw->frame, 0, 0, psw->boundary_width,
		      psw->boundary_width, 0, CopyFromParent,
		      InputOutput, CopyFromParent,
		      valuemask,
		      &attributes);
      ScwmSaveContextPsw(dpy, psw->sides[i], psw);
    }
    if (TexturePixmap) {
      attributes.background_pixmap = TexturePixmapSave;
      valuemask = valuemask_save;
    }
  }

  /* maps the decoration and reparenting windows,
     but the frame is still not mapped until later */
  XMapSubwindows(dpy, psw->frame);
  XRaiseWindow(dpy, psw->Parent);

  /* Finally, take over the client window and let
     psw->frame be the top-level window (since it 
     is the parent of psw->Parent).  Remember,
     psw->Parent is the direct parent of the application
     window psw->w --07/27/98 gjb */
  XReparentWindow(dpy, psw->w, psw->Parent, 0, 0);
  ScwmSaveContextPsw(dpy, psw->w, psw);


  valuemask = (CWEventMask | CWDontPropagate);
  attributes.event_mask = (StructureNotifyMask | PropertyChangeMask |
			   VisibilityChangeMask | EnterWindowMask |
			   LeaveWindowMask |
			   ColormapChangeMask | FocusChangeMask);

  attributes.do_not_propagate_mask = ButtonPressMask | ButtonReleaseMask;

  XChangeWindowAttributes(dpy, psw->w, valuemask, &attributes);

  /* do not let the client window disappear if scwm dies */
  XAddToSaveSet(dpy, psw->w);

  /*
   * Reparenting generates an UnmapNotify event, followed by a MapNotify.
   * Set the map state to False to prevent a transition back to
   * WithdrawnState in HandleUnmapNotify.  Map state gets set correctly
   * again in HandleMapNotify.
   */
  psw->fMapped = False;

  /* initialize the fields in case a placewindow function
     tries to use them */
  SET_CVALUE(psw, frame_x, frame_x);
  SET_CVALUE(psw, frame_y, frame_y);
  SET_CVALUE(psw, frame_width, frame_width);
  SET_CVALUE(psw, frame_height, frame_height);

  /* PlaceWindow, below, will call move_finalize, which will
     inform cassowary of the new position, so we don't need
     to  resolve */
  CassowarySetCValuesAndSolve(psw,False /* no solve */);

  /* stay constraints need to be added before any calls to 
     move_finalize, which PlaceWindow, e.g., does */
  CassowaryNewWindow(psw);      /* add the stay constraints in */

  /* MS:FIXME:: Hmm, do we need to do any real cleanup if this fails?
     _Can_ it fail, in its new location?
     -- I think we just have to make PlaceWindow put it somewhere
     and never fail - that's its current behaviour, but it still
     returns a Bool that's always just True... --07/27/98 gjb
  */

  call1_hooks(before_place_new_window_hook, psw->schwin);

  if (!PlaceWindow(psw)) {
    scwm_msg(ERR,FUNC_NAME,"PlaceWindow failed for %s -- resources leaked!",psw->name);
    /* there is cleanup we would need to do (but what is the 
       meaning of a failed PlaceWindow?) --07/27/98 gjb */
    return NULL;
  }

  /* wait until the window is iconified and the icon window is mapped
     before creating the icon window */
  psw->icon_w = None;

  GrabButtonsForPsw(psw);
  GrabKeysForPsw(psw);

  RaiseWindow(psw);
  KeepOnTop();

  /* Mark the window as fully constructed */
  psw->fFullyConstructed = True;
  XUngrabServer_withSemaphore(dpy);

  if (psw->fClickToFocus) {
    /* need to grab all buttons for window that we are about to
       * unhighlight */
    for (i = 0; i < XSERVER_MAX_BUTTONS; i++)
      if (Scr.buttons2grab & (1 << i)) {
        GrabButtonWithModifiersMaskXcPm(i+1,0,psw->frame,
                                        ButtonPressMask,
                                        XCursorByNumber(XC_hand2),GrabModeSync);
      }
  }

  BroadcastConfig(M_ADD_WINDOW, psw);
  BroadcastName(M_WINDOW_NAME, psw->w, psw->frame,
		(unsigned long) psw, psw->name);
  BroadcastName(M_ICON_NAME, psw->w, psw->frame,
		(unsigned long) psw, psw->icon_name);
  /* GJB:FIXME: It'd be really nice to get full pathname of
     the picture into the image object for debugging of scwmrc-s;
     then this could go back in, too, though I imagine it's
     rarely used --gjb 11/28/97  */

  /* This could be made to work with the current stuff, but under our
     model now, the icon won't get set until later, at which point (I
     think) the right broadcast will happen. -MS */
  /*if (psw->szIconFile != NULL &&
      psw->szIconFile != Scr.DefaultIcon)
    BroadcastName(M_ICON_FILE, psw->w, psw->frame,
    (unsigned long) psw, psw->szIConfile); */
  BroadcastName(M_RES_CLASS, psw->w, psw->frame,
		(unsigned long) psw, psw->classhint.res_class);
  BroadcastName(M_RES_NAME, psw->w, psw->frame,
		(unsigned long) psw, psw->classhint.res_name);
  if (psw->mini_icon_image != SCM_BOOL_F) {
    BroadcastMiniIcon(M_MINI_ICON, psw);
  }

  FetchWmProtocols(psw);
  FetchWmColormapWindows(psw);
  if (!(XGetWindowAttributes(dpy, psw->w, &(psw->attr))))
    psw->attr.colormap = Scr.ScwmRoot.attr.colormap;

  InstallWindowColormaps(colormap_win);

  call1_hooks(after_new_window_hook, psw->schwin);


  if (!psw->fSticky) {
    if (restart_vp_offset_x != 0 || restart_vp_offset_y != 0) {
#ifdef SCWM_DEBUG_RESTART_MOVE_MSGS      
      scwm_msg(INFO,FUNC_NAME,"Moving %s by %d,%d to to correct for restart_vp_offset",
               psw->name,restart_vp_offset_x,restart_vp_offset_y);
#endif
      MoveTo(psw, FRAME_X(psw)+restart_vp_offset_x, 
             FRAME_Y(psw)+restart_vp_offset_y);
    }
  }

  CreateIconWindow(psw,ICON_X_VP(psw),ICON_Y_VP(psw));

  return (psw);
}
#undef FUNC_NAME 
 

/*
 * Handles destruction of a window 
 * pretty tightly coupled to AddWindow, above.
 */
void 
DestroyScwmWindow(ScwmWindow *psw)
{
  int i;
  extern Bool PPosOverride;

  /*
   * Warning, this is also called by HandleUnmapNotify; if it ever needs to
   * look at the event, HandleUnmapNotify will have to mash the UnmapNotify
   * into a DestroyNotify.
   */
  if (!psw)
    return;

  if (Scr.Hilite==psw) {
    Scr.Hilite=NULL;
  }

  call1_hooks(window_close_hook, psw->schwin);

  CassowaryCloseWindow(psw);

  XUnmapWindow(dpy, psw->frame);

  if (!PPosOverride)
    XSync(dpy, False);

  Broadcast(M_DESTROY_WINDOW, 3, psw->w, psw->frame,
	    (unsigned long) psw, 0, 0, 0, 0);

  XDestroyWindow(dpy, psw->frame);
  XDeleteContext(dpy, psw->frame, ScwmContext);

  XDestroyWindow(dpy, psw->Parent);

  XDeleteContext(dpy, psw->Parent, ScwmContext);

  XDeleteContext(dpy, psw->w, ScwmContext);

  if (psw->icon_w && psw->fPixmapOurs &&
      psw->icon_image != SCM_BOOL_F) {
    XFreePixmap(dpy, IMAGE(psw->icon_image)->image);
  }

  /* GJB:FIXME:: these should check if the windows were created,
     not if the feature is currently turned on */
  if (psw->icon_w) {
    XDestroyWindow(dpy, psw->icon_w);
    XDeleteContext(dpy, psw->icon_w, ScwmContext);
  }
  if (psw->fIconOurs && (psw->icon_pixmap_w != None))
    XDestroyWindow(dpy, psw->icon_pixmap_w);
  if (psw->icon_pixmap_w != None)
    XDeleteContext(dpy, psw->icon_pixmap_w, ScwmContext);

  if (psw->fTitle) {
    XDeleteContext(dpy, psw->title_w, ScwmContext);
    for (i = 0; i < Scr.nr_left_buttons; i++)
      if (psw->left_w[i] != None)
        XDeleteContext(dpy, psw->left_w[i], ScwmContext);
    for (i = 0; i < Scr.nr_right_buttons; i++)
      if (psw->right_w[i] != None)
	XDeleteContext(dpy, psw->right_w[i], ScwmContext);
  }
  if (psw->fBorder) {
    for (i = 0; i < 4; i++)
      XDeleteContext(dpy, psw->sides[i], ScwmContext);
    for (i = 0; i < 4; i++)
      XDeleteContext(dpy, psw->corners[i], ScwmContext);
  }
  psw->prev->next = psw->next;
  if (psw->next != NULL)
    psw->next->prev = psw->prev;
  free_window_names(psw, True, True);
  if (psw->wmhints)
    XFree(psw->wmhints);
  /* removing NoClass change for now... */
  if (psw->classhint.res_name && psw->classhint.res_name != NoResource)
    XFree(psw->classhint.res_name);
  if (psw->classhint.res_class && psw->classhint.res_class != NoClass)
    XFree(psw->classhint.res_class);
  if (psw->mwm_hints)
    XFree(psw->mwm_hints);

  if (psw->cmap_windows != (Window *) NULL)
    XFree(psw->cmap_windows);

  /* XSCM */
  invalidate_window(psw->schwin);
  XFree(psw->name);
  FREE(psw);

  if (!PPosOverride)
    XSync(dpy, False);

  return;
}


/*
 *  Procedure:
 *	FetchWMProtocols - finds out which protocols the window supports
 *
 *  Inputs:
 *	tmp - the scwm window structure to use
 */
void 
FetchWmProtocols(ScwmWindow *psw)
{
  Atom *protocols = NULL, *ap;
  int i, n;
  Atom atype;
  int aformat;
  unsigned long bytes_remain, nitems;

  if (psw == NULL)
    return;
  /* First, try the Xlib function to read the protocols.
   * This is what Twm uses. */
  if (XGetWMProtocols(dpy, psw->w, &protocols, &n)) {
    for (i = 0, ap = protocols; i < n; i++, ap++) {
      if (*ap == (Atom) XA_WM_TAKE_FOCUS)
	psw->fDoesWmTakeFocus = True;
      if (*ap == (Atom) XA_WM_DELETE_WINDOW)
	psw->fDoesWmDeleteWindow = True;
    }
    if (protocols)
      XFree((char *) protocols);
  } else {
    /* Next, read it the hard way. mosaic from Coreldraw needs to 
     * be read in this way. */
    if ((XGetWindowProperty(dpy, psw->w, XA_WM_PROTOCOLS, 0L, 10L, False,
			    XA_WM_PROTOCOLS, &atype, &aformat, &nitems,
			    &bytes_remain,
			    (unsigned char **) &protocols)) == Success) {
      for (i = 0, ap = protocols; i < (int) nitems; i++, ap++) {
	if (*ap == (Atom) XA_WM_TAKE_FOCUS)
	  psw->fDoesWmTakeFocus = True;
	if (*ap == (Atom) XA_WM_DELETE_WINDOW)
	  psw->fDoesWmDeleteWindow = True;
      }
      if (protocols)
	XFree((char *) protocols);
    }
  }
  return;
}

/*
 * GetWindowSizeHints - gets application supplied size info into psw->hints
 *           
 *  Inputs:
 *	psw - the scwm window structure to use
 */
void 
GetWindowSizeHints(ScwmWindow * psw)
{
  long supplied = 0;

  if (!XGetWMNormalHints(dpy, psw->w, &psw->hints, &supplied))
    psw->hints.flags = 0;

  /* Beat up our copy of the hints, so that all important field are
   * filled in! */
  if (psw->hints.flags & PResizeInc) {
    if (psw->hints.width_inc == 0)
      psw->hints.width_inc = 1;
    if (psw->hints.height_inc == 0)
      psw->hints.height_inc = 1;
  } else {
    psw->hints.width_inc = 1;
    psw->hints.height_inc = 1;
  }

  /*
   * ICCCM says that PMinSize is the default if no PBaseSize is given,
   * and vice-versa.
   */

  if (!(psw->hints.flags & PBaseSize)) {
    if (psw->hints.flags & PMinSize) {
      psw->hints.base_width = psw->hints.min_width;
      psw->hints.base_height = psw->hints.min_height;
    } else {
      psw->hints.base_width = 0;
      psw->hints.base_height = 0;
    }
  }
  if (!(psw->hints.flags & PMinSize)) {
    psw->hints.min_width = psw->hints.base_width;
    psw->hints.min_height = psw->hints.base_height;
  }
  if (!(psw->hints.flags & PMaxSize)) {
    psw->hints.max_width = MAX_WINDOW_WIDTH;
    psw->hints.max_height = MAX_WINDOW_HEIGHT;
  }
  if (psw->hints.max_width < psw->hints.min_width)
    psw->hints.max_width = MAX_WINDOW_WIDTH;
  if (psw->hints.max_height < psw->hints.min_height)
    psw->hints.max_height = MAX_WINDOW_HEIGHT;

  /* Zero width/height windows are bad news! */
  if (psw->hints.min_height <= 0)
    psw->hints.min_height = 1;
  if (psw->hints.min_width <= 0)
    psw->hints.min_width = 1;

  if (!(psw->hints.flags & PWinGravity)) {
    psw->hints.win_gravity = NorthWestGravity;
    psw->hints.flags |= PWinGravity;
  }
}

void init_add_window()
{
#ifndef SCM_MAGIC_SNARFER
#include "add_window.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */
