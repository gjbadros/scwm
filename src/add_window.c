/* $Id$
 * add_window.c
 */

/* This module is derived from code
 * based on Twm, but has been siginificantly modified 
 * by Rob Nation
 */

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
#include <X11/extensions/shape.h>

#include <guile/gh.h>

#define ADD_WINDOW_IMPLEMENTATION
#include "add_window.h"

#include "scwm.h"
#include "screen.h"
#include "binding.h"
#include "window.h"
#include "decorations.h"
#include "Grab.h"
#include "colors.h"
#include "borders.h"
#include "resize.h"
#include "colormaps.h"
#include "image.h"
#include "module-interface.h"
#include "icons.h"
#include "placement.h"
#include "callbacks.h"
#include "session-manager.h"
#include "xmisc.h"
#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif


SCM before_new_window_hook;
SCM before_place_new_window_hook;
SCM after_new_window_hook;

/* FIXGJB: instead of placeholder empty functions,
   pointers to functions should be used, and init_constraint_primitives should
   set the pointers to point to functions that it dynamically loads */
#ifndef USE_CASSOWARY
void CassowarySetCValuesAndSolve(ScwmWindow *psw, int fSolve)  { /* empty */ }
void CassowaryInitClVarsInPsw(ScwmWindow *psw) { /* empty */ }
void CassowaryInitClVarsInPscreen(ScreenInfo *pscreen) { /* empty */ }
void CassowaryNewWindow(ScwmWindow *psw) { /* empty */ }
void CassowaryEditPosition(ScwmWindow *psw) { /* empty */ }
void CassowaryEditSize(ScwmWindow *psw) { /* empty */ }
/* x,y are virtual positions */
void SuggestMoveWindowTo(ScwmWindow *psw, int x, int y, Bool fOpaque) {
  SetScwmWindowPosition(psw,x,y,fOpaque);
}
/* x,y are virtual positions */
void SuggestSizeWindowTo(ScwmWindow *psw, int x, int y, int w, int h, Bool fOpaque) {
  SetScwmWindowGeometry(psw,x,y,w,h, fOpaque);
}
/* from virtual.h */
void MoveViewport_internal(int newx, int newy, Bool grab);

void ChangeVirtualPosition(int vx, int vy, Bool fGrab) {
  MoveViewport_internal(vx,vy,fGrab);
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
 */
ScwmWindow *
AddWindow(Window w)
#define FUNC_NAME "AddWindow"
{
  ScwmWindow *psw;		/* new scwm window structure */
  unsigned long valuemask;	/* mask for create windows */
  SCM schwin; /* To make sure it's on the stack to be marked. */

  Pixmap TexturePixmap = None, TexturePixmapSave = None;
  unsigned long valuemask_save = 0;

  XSetWindowAttributes attributes;	/* attributes for create windows */
  int i;

  int Desk = 0, border_width = 0, resize_width = 0;
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

  psw->cmap_windows = NULL;
  psw->ttLastFocussed = time(NULL);

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


  /* if the window is in the NoTitle list, or is a transient,
   *  dont decorate it.
   * If its a transient, and DecorateTransients was specified,
   *  decorate anyway
   */
  psw->icon_image = SCM_BOOL_F;
  psw->icon_req_image = SCM_BOOL_F;
  psw->mini_icon_image = SCM_BOOL_F;

  /* FIXMS - bletcherous... we process the hint properties separately,
     since hints need to be processed early, but some procs we may
     want to pass alter the size of the window &c. Will find a better
     way to deal with this - probably a reprocesshints function of
     some kind. */

  ResetCommonFlags(psw);
  psw->fTitle = True;
  psw->fBorder = True;

  /* FIXMS: need to find better way to ensure colors are valid before
     window comes under GC control. */

  psw->TextColor = Scr.NotMenuColors.fg;
  psw->ReliefColor = Scr.NotMenuRelief.fg;
  psw->ShadowColor = Scr.NotMenuRelief.bg;
  psw->BackColor = Scr.NotMenuColors.bg;
  psw->HiTextColor = SCM_BOOL_F;
  psw->HiReliefColor = SCM_BOOL_F;
  psw->HiShadowColor = SCM_BOOL_F;
  psw->HiBackColor = SCM_BOOL_F;

  gh_defer_ints();
  /* create the scheme-level window */
  psw->schwin = schwin = make_window(psw);
  /* and initialize constraint structure hanging off of psw
     (uses the scheme window so must come after the make_window assignment
     above) */
  CassowaryInitClVarsInPsw(psw);
  gh_allow_ints();

  psw->fl = &Scr.DefaultDecor;

  call1_hooks(before_new_window_hook, psw->schwin);

  GetMwmHints(psw);
  GetOlHints(psw);

  SelectDecor(psw, border_width, resize_width);

  DBUG((DBG,FUNC_NAME,"fTitle = %d, th = %d", psw->fTitle, psw->title_height));

#ifdef HAVE_LIBSM_LIBICE
  restoreWindowState(psw);
#endif

  if (psw->fStartsOnDesk) {
    DBUG((DBG,FUNC_NAME,"fStartsOnDesk is true"));
    Desk = psw->StartDesk;
  }

  /* FIXGJB: need to provide more flexibility in how the
     icon gets selected */
  /* find a suitable icon pixmap */

  GetWindowSizeHints(psw);

  psw->xboundary_width = psw->boundary_width;
  if (psw->fMWMBorders) psw->bw = 0;
  else psw->bw = BW;

  /* Tentative size estimate */
  frame_x = 0;
  frame_y = 0;
  frame_width = psw->attr.width + 2 * psw->xboundary_width;
  frame_height = (psw->attr.height + (psw->fTitle ? psw->title_height : 0)
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
      Desk = atoi(rm_value.addr);
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

  attributes.cursor = Scr.ScwmCursors[CURSOR_DEFAULT];
  attributes.event_mask = (SubstructureRedirectMask | ButtonPressMask |
			   ButtonReleaseMask | EnterWindowMask |
			   LeaveWindowMask | ExposureMask);

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

  DBUG((DBG,FUNC_NAME,"Now fTitle = %d, th = %d", psw->fTitle, psw->title_height));

  /* What the heck, we'll always reparent everything from now on! */
  DBUG((DBG,FUNC_NAME,"Creating child of root window: %d %d, %d x %d, %d",
       frame_x,frame_y,frame_width,frame_height,psw->bw));

  psw->frame =
    XCreateWindow(dpy, Scr.Root, frame_x, frame_y,
                  frame_width, frame_height,
		  psw->bw, CopyFromParent, InputOutput,
		  CopyFromParent,
		  valuemask,
		  &attributes);
  XSaveContext(dpy, psw->frame, ScwmContext, (caddr_t) psw);

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
  attributes.cursor = Scr.ScwmCursors[CURSOR_DEFAULT];
  DBUG((DBG,FUNC_NAME,"Creating child of frame: %d %d, %d x %d, %d",
       psw->boundary_width, psw->boundary_width + psw->title_height,
       psw->attr.width, psw->attr.height, psw->bw));
  psw->Parent =
    XCreateWindow(dpy, psw->frame,
		  psw->boundary_width, 
		  psw->boundary_width + psw->title_height,
                  psw->attr.width, psw->attr.height, psw->bw, 
                  CopyFromParent, InputOutput, CopyFromParent, 
                  valuemask, &attributes);
  XSaveContext(dpy, psw->Parent, ScwmContext, (caddr_t) psw);


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
    DBUG((DBG,FUNC_NAME,"Set height to %d",psw->title_height));
    if (psw->title_height < 1)
      psw->title_height = 1;
  }

  if (psw->fBorder) {
    DBUG((DBG,FUNC_NAME,"Has border"));

    if (TexturePixmap) {
      TexturePixmapSave = attributes.background_pixmap;
      attributes.background_pixmap = TexturePixmap;
      valuemask_save = valuemask;
      valuemask = (valuemask & ~CWBackPixel) | CWBackPixmap;
    }
    /* Just dump the decoration windows at 0,0 and
       let SetupFrame move them to their appropriate positions */
    for (i = 0; i < 4; i++) {
      attributes.cursor = Scr.ScwmCursors[CURSOR_TOP_LEFT + i];
      psw->corners[i] =
	XCreateWindow(dpy, psw->frame, 0, 0,
		      psw->corner_width, psw->corner_width,
		      0, CopyFromParent, InputOutput,
		      CopyFromParent,
		      valuemask,
		      &attributes);
      XSaveContext(dpy, psw->corners[i], ScwmContext, (caddr_t) psw);
    }
    if (TexturePixmap) {
      attributes.background_pixmap = TexturePixmapSave;
      valuemask = valuemask_save;
    }
  }

  /* We always create the title bar since we can dynamically show or hide it */
  psw->title_x = psw->boundary_width + psw->title_height + 1;
  psw->title_y = psw->boundary_width;
  attributes.cursor = Scr.ScwmCursors[CURSOR_TITLE];
  DBUG((DBG,FUNC_NAME,"Creating title window: %d %d, %d x %d",
       psw->title_x, psw->title_y,
       psw->title_width, psw->title_height));
  psw->title_w =
    XCreateWindow(dpy, psw->frame, psw->title_x, psw->title_y,
                  psw->title_width, psw->title_height, 0,
                  CopyFromParent, InputOutput, CopyFromParent,
                  valuemask, &attributes);
  XSaveContext(dpy, psw->title_w, ScwmContext, (caddr_t) psw);

  attributes.cursor = Scr.ScwmCursors[CURSOR_SYS];
  for (i = 4; i >= 0; i--) {
    if ((i < Scr.nr_left_buttons) && (psw->left_w[i] > 0)) {
      if (TexturePixmap
          && GET_DECOR(psw, left_buttons[i].flags) & UseBorderStyle) {
        TexturePixmapSave = attributes.background_pixmap;
        attributes.background_pixmap = TexturePixmap;
        valuemask_save = valuemask;
        valuemask = (valuemask & ~CWBackPixel) | CWBackPixmap;
      }
      DBUG((DBG,FUNC_NAME,"Creating left button %d",i));
      psw->left_w[i] =
        XCreateWindow(dpy, psw->frame, psw->title_height * i, 0,
                      psw->title_height, psw->title_height, 0,
                      CopyFromParent, InputOutput,
                      CopyFromParent,
                      valuemask,
                      &attributes);
      XSaveContext(dpy, psw->left_w[i], ScwmContext, (caddr_t) psw);
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
      DBUG((DBG,FUNC_NAME,"Creating right button %d",i));
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
      XSaveContext(dpy, psw->right_w[i], ScwmContext, (caddr_t) psw);
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
      attributes.cursor = Scr.ScwmCursors[CURSOR_TOP + i];
      DBUG((DBG,FUNC_NAME,"Creating side %d",i));
      psw->sides[i] =
	XCreateWindow(dpy, psw->frame, 0, 0, psw->boundary_width,
		      psw->boundary_width, 0, CopyFromParent,
		      InputOutput, CopyFromParent,
		      valuemask,
		      &attributes);
      XSaveContext(dpy, psw->sides[i], ScwmContext, (caddr_t) psw);
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
  XSaveContext(dpy, psw->w, ScwmContext, (caddr_t) psw);


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

  /* FIXMS: Hmm, do we need to do any real cleanup if this fails?
     _Can_ it fail, in its new location?
     -- I think we just have to make PlaceWindow put it somewhere
     and never fail - that's its current behaviour, but it still
     returns a Bool that's always just True... --07/27/98 gjb
  */

  call1_hooks(before_place_new_window_hook, psw->schwin);

  if (!PlaceWindow(psw, Desk)) {
    scwm_msg(ERR,FUNC_NAME,"PlaceWindow failed for %s -- resources leaked!",psw->name);
    /* there is cleanup we would need to do (but what is the 
       meaning of a failed PlaceWindow?) --07/27/98 gjb */
    return NULL;
  }

  /* wait until the window is iconified and the icon window is mapped
     before creating the icon window */
  psw->icon_w = None;

  GrabButtons(psw);
  GrabKeys(psw);

  RaiseWindow(psw);
  KeepOnTop();

  XUngrabServer_withSemaphore(dpy);



  if (psw->fClickToFocus) {
    /* need to grab all buttons for window that we are about to
       * unhighlight */
    for (i = 0; i < XSERVER_MAX_BUTTONS; i++)
      if (Scr.buttons2grab & (1 << i)) {
	XGrabButton(dpy, (i + 1), 0, psw->frame, True,
		    ButtonPressMask, GrabModeSync, GrabModeAsync, None,
		    Scr.ScwmCursors[CURSOR_SYS]);
	XGrabButton(dpy, (i + 1), LockMask, psw->frame, True,
		    ButtonPressMask, GrabModeSync, GrabModeAsync, None,
		    Scr.ScwmCursors[CURSOR_SYS]);
      }
  }

  BroadcastConfig(M_ADD_WINDOW, psw);
  BroadcastName(M_WINDOW_NAME, psw->w, psw->frame,
		(unsigned long) psw, psw->name);
  BroadcastName(M_ICON_NAME, psw->w, psw->frame,
		(unsigned long) psw, psw->icon_name);
  /* MSFIX: FIXGJB: It'd be really nice to get full pathname of
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

  CreateIconWindow(psw,ICON_X_VP(psw),ICON_Y_VP(psw));

  return (psw);
}
#undef FUNC_NAME 
 
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
      for (i = 0, ap = protocols; i < nitems; i++, ap++) {
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
  SCWM_HOOK(before_new_window_hook, "before-new-window-hook");
  /** This hook is invoked when a new window structure is first starting
to be created. Only a subset of the usual window parameters should be
set here, in particular, those that control what hints will be
respected for this window, and those that control how the window will
be placed.

This hook does not typically need to be used directly by the user;
`window-style' from the "(app scwm style)" module provides a convenient
interface to setting the relevant parameters when a new window is
created. */

  SCWM_HOOK(before_place_new_window_hook, "before-place-new-window-hook");
  /** This hook is invoked just before placing a new window.
It comes after `before-new-window-hook', but before `after-new-window-hook'. 
This hook may be removed later since it is mostly redundant with the other
two hooks, despite being invoked at a slightly different time. */

  SCWM_HOOK(after_new_window_hook, "after-new-window-hook");
  /** This hook is invoked when a new window has been completely created
and placed on the screen. Any window operations may be performed at
this time. However, it is recommended that placement-related
operations, such as setting the position, desk, viewport location and
z-ordering of a window be done in the placement procedure instead.
It should be used for setting window styles, as the window geometry
needs to be fully and correctly specified before the window is placed.
The `window-style' mechanism from the "(app scwm style)" module provides a convenient
interface to setting the relevant parameters when a new window is
created. */

#ifndef SCM_MAGIC_SNARFER
#include "add_window.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
