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


/**********************************************************************
 *
 * Add a new window, put the titlbar and other stuff around
 * the window
 *
 **********************************************************************/
#include <config.h>

/* #define SCWM_DEBUG_MSGS */

#define ADD_WINDOW_IMPLEMENTATION
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "scwm.h"
#include <X11/Xatom.h>
#include "misc.h"
#include "screen.h"
#include <X11/extensions/shape.h>
#include <X11/Xresource.h>
#include <guile/gh.h>
#include "binding.h"
#include "window.h"
#include "decorations.h"
#include "Grab.h"
#include "add_window.h"
#include "colors.h"
#include "borders.h"
#include "resize.h"
#include "colormaps.h"
#include "image.h"
#include "module-interface.h"
#include "icons.h"
#include "placement.h"
#include "callbacks.h"


SCM before_new_window_hook;
SCM after_new_window_hook;

/* Used to parse command line of clients for specific desk requests. */
/* Todo: check for multiple desks. */
static XrmDatabase db;
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


/***********************************************************************
 *
 *  Procedure:
 *	GrabButtons - grab needed buttons for the window
 *
 *  Inputs:
 *	tmp_win - the scwm window structure to use
 *
 ***********************************************************************/

/* FIXGJB: rewrite to use GrabButtonWithModifiers, above */
static void 
GrabButtons(ScwmWindow * tmp_win)
{
  Binding *MouseEntry;

  MouseEntry = Scr.AllBindings;
  while (MouseEntry != (Binding *) 0) {
    if ((MouseEntry->Action != NULL) && (MouseEntry->Context & C_WINDOW)
	&& (MouseEntry->IsMouse == 1)) {
      if (MouseEntry->Button_Key > 0) {
	XGrabButton(dpy, MouseEntry->Button_Key, MouseEntry->Modifier,
		    tmp_win->w,
		    True, ButtonPressMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, None,
		    Scr.ScwmCursors[CURSOR_DEFAULT]);
	if (MouseEntry->Modifier != AnyModifier) {
	  XGrabButton(dpy, MouseEntry->Button_Key,
		      (MouseEntry->Modifier | LockMask),
		      tmp_win->w,
		      True, ButtonPressMask | ButtonReleaseMask,
		      GrabModeAsync, GrabModeAsync, None,
		      Scr.ScwmCursors[CURSOR_DEFAULT]);
	}
      } else {
	XGrabButton(dpy, 1, MouseEntry->Modifier,
		    tmp_win->w,
		    True, ButtonPressMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, None,
		    Scr.ScwmCursors[CURSOR_DEFAULT]);
	XGrabButton(dpy, 2, MouseEntry->Modifier,
		    tmp_win->w,
		    True, ButtonPressMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, None,
		    Scr.ScwmCursors[CURSOR_DEFAULT]);
	XGrabButton(dpy, 3, MouseEntry->Modifier,
		    tmp_win->w,
		    True, ButtonPressMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, None,
		    Scr.ScwmCursors[CURSOR_DEFAULT]);
	if (MouseEntry->Modifier != AnyModifier) {
	  XGrabButton(dpy, 1,
		      (MouseEntry->Modifier | LockMask),
		      tmp_win->w,
		      True, ButtonPressMask | ButtonReleaseMask,
		      GrabModeAsync, GrabModeAsync, None,
		      Scr.ScwmCursors[CURSOR_DEFAULT]);
	  XGrabButton(dpy, 2,
		      (MouseEntry->Modifier | LockMask),
		      tmp_win->w,
		      True, ButtonPressMask | ButtonReleaseMask,
		      GrabModeAsync, GrabModeAsync, None,
		      Scr.ScwmCursors[CURSOR_DEFAULT]);
	  XGrabButton(dpy, 3,
		      (MouseEntry->Modifier | LockMask),
		      tmp_win->w,
		      True, ButtonPressMask | ButtonReleaseMask,
		      GrabModeAsync, GrabModeAsync, None,
		      Scr.ScwmCursors[CURSOR_DEFAULT]);
	}
      }
    }
    MouseEntry = MouseEntry->NextBinding;
  }
  return;
}

/***********************************************************************
 *
 *  Procedure:
 *	GrabKeys - grab needed keys for the window
 *
 *  Inputs:
 *	tmp_win - the scwm window structure to use
 *
 ***********************************************************************/
void 
GrabKeys(ScwmWindow * tmp_win)
{
  Binding *tmp;

  for (tmp = Scr.AllBindings; tmp != NULL; tmp = tmp->NextBinding) {
    if ((tmp->Context & (C_WINDOW | C_TITLE | C_RALL | C_LALL | C_SIDEBAR)) &&
	(tmp->IsMouse == 0)) {
      XGrabKey(dpy, tmp->Button_Key, tmp->Modifier, tmp_win->frame, True,
	       GrabModeAsync, GrabModeAsync);
      if (tmp->Modifier != AnyModifier) {
	XGrabKey(dpy, tmp->Button_Key, tmp->Modifier | LockMask,
		 tmp_win->frame, True,
		 GrabModeAsync, GrabModeAsync);
      }
    }
  }
  return;
}


/***********************************************************************
 *
 *  Procedure:
 *	AddWindow - add a new window to the scwm list
 *
 *  Returned Value:
 *	(ScwmWindow *) - pointer to the ScwmWindow structure
 *
 *  Inputs:
 *	w	- the window id of the window to add
 *	iconm	- flag to tell if this is an icon manager window
 *
 ***********************************************************************/
ScwmWindow *
AddWindow(Window w)
{
  ScwmWindow *tmp_win;		/* new scwm window structure */
  unsigned long valuemask;	/* mask for create windows */

  SCM schwin; /* To make sure it's on the stack to be marked. */

  Pixmap TexturePixmap = None, TexturePixmapSave = None;
  unsigned long valuemask_save = 0;

  XSetWindowAttributes attributes;	/* attributes for create windows */
  int i, width, height;
  int a, b;

  char *decor = NULL;

  int Desk = 0, border_width = 0, resize_width = 0;
  extern Bool NeedToResizeToo;
  extern ScwmWindow *colormap_win;
  int client_argc;
  char **client_argv = NULL, *str_type;
  Bool status;
  XrmValue rm_value;
  XTextProperty text_prop;
  extern Bool PPosOverride;

  NeedToResizeToo = False;
  /* allocate space for the scwm window */

  tmp_win = (ScwmWindow *) calloc(1, sizeof(ScwmWindow));
  if (!tmp_win) {
    return NULL;
  }
  tmp_win->w = w;
  ResetAllFlags(tmp_win);

  tmp_win->cmap_windows = NULL;

  if (!PPosOverride)
    if (!FXWindowAccessible(dpy,tmp_win->w)) {
      free((char *) tmp_win);
      return (NULL);
    }
  if (XGetWMName(dpy, tmp_win->w, &text_prop) != 0)
    tmp_win->name = (char *) text_prop.value;
  else
    tmp_win->name = NoName;

  /* removing NoClass change for now... */
  tmp_win->classhint.res_name = NoResource;
  tmp_win->classhint.res_class = NoClass;
  XGetClassHint(dpy, tmp_win->w, &tmp_win->classhint);
  if (tmp_win->classhint.res_name == NULL)
    tmp_win->classhint.res_name = NoResource;
  if (tmp_win->classhint.res_class == NULL)
    tmp_win->classhint.res_class = NoClass;

  FetchWmProtocols(tmp_win);
  FetchWmColormapWindows(tmp_win);
  if (!(XGetWindowAttributes(dpy, tmp_win->w, &(tmp_win->attr))))
    tmp_win->attr.colormap = Scr.ScwmRoot.attr.colormap;

  tmp_win->wmhints = XGetWMHints(dpy, tmp_win->w);

  tmp_win->fTransient =
    (XGetTransientForHint(dpy, tmp_win->w, &tmp_win->transientfor));

  tmp_win->old_bw = tmp_win->attr.border_width;

  if (ShapesSupported) {
    int xws, yws, xbs, ybs;
    unsigned wws, hws, wbs, hbs;
    int boundingShaped, clipShaped;

    XShapeSelectInput(dpy, tmp_win->w, ShapeNotifyMask);
    XShapeQueryExtents(dpy, tmp_win->w,
		       &boundingShaped, &xws, &yws, &wws, &hws,
		       &clipShaped, &xbs, &ybs, &wbs, &hbs);
    tmp_win->wShaped = boundingShaped;
  }


  /* if the window is in the NoTitle list, or is a transient,
   *  dont decorate it.
   * If its a transient, and DecorateTransients was specified,
   *  decorate anyway
   */
  tmp_win->icon_image = SCM_BOOL_F;
  tmp_win->icon_req_image = SCM_BOOL_F;
  tmp_win->mini_icon_image = SCM_BOOL_F;

  /* FIXMS - bletcherous... we process the hint properties separately,
     since hints need to be processed early, but some procs we may
     want to pass alter the size of the window &c. Will find a better
     way to deal with this - probably a reprocesshints function of
     some kind. */

  ResetAllFlags(tmp_win);
  tmp_win->fTitle = True;
  tmp_win->fBorder = True;

  /* FIXMS: need to find better way to ensure colors are valid before
     window comes under GC control. */

  tmp_win->TextColor = Scr.MenuColors.fg;
  tmp_win->ReliefColor = Scr.MenuRelief.fg;
  tmp_win->ShadowColor = Scr.MenuRelief.bg;
  tmp_win->BackColor = Scr.MenuColors.bg;

  tmp_win->schwin = schwin = make_window(tmp_win);

  call1_hooks(before_new_window_hook, tmp_win->schwin);

  if (tmp_win->fStartsOnDesk) {
    DBUG(__FUNCTION__,"fStartsOnDesk is true");
    Desk = tmp_win->StartDesk;
  }

  tmp_win->fl = &Scr.DefaultDecor;

  GetMwmHints(tmp_win);
  GetOlHints(tmp_win);

  SelectDecor(tmp_win, border_width, resize_width);

  DBUG(__FUNCTION__,"fTitle = %d, th = %d", tmp_win->fTitle, tmp_win->title_height);

  /* FIXGJB: need to provide more flexibility in how the
     icon gets selected */
  /* find a suitable icon pixmap */

  GetWindowSizeHints(tmp_win);

  /* Tentative size estimate */
  tmp_win->frame_width = tmp_win->attr.width + 2 * tmp_win->boundary_width;
  tmp_win->frame_height = tmp_win->attr.height + tmp_win->title_height +
    2 * tmp_win->boundary_width;

  ConstrainSize(tmp_win, &tmp_win->frame_width, &tmp_win->frame_height);

  /* Find out if the client requested a specific desk on the command line. */
  if (XGetCommand(dpy, tmp_win->w, &client_argv, &client_argc)) {
    XrmParseCommand(&db, table, 4, "scwm", &client_argc, client_argv);
    status = XrmGetResource(db, "scwm.desk", "Scwm.Desk", &str_type, &rm_value);
    if ((status == True) && (rm_value.size != 0)) {
      Desk = atoi(rm_value.addr);
      tmp_win->fStartsOnDesk = True;
    }
    XrmDestroyDatabase(db);
    db = NULL;
  }
  if (!PlaceWindow(tmp_win, Desk))
    return NULL;

  /*
   * Make sure the client window still exists.  We don't want to leave an
   * orphan frame window if it doesn't.  Since we now have the server
   * grabbed, the window can't disappear later without having been
   * reparented, so we'll get a DestroyNotify for it.  We won't have
   * gotten one for anything up to here, however.
   */
  XGrabServer_withSemaphore(dpy); 
  if (!FXWindowAccessible(dpy,w)) {
    free((char *) tmp_win);
    XUngrabServer_withSemaphore(dpy);
    return (NULL);
  }
  XSetWindowBorderWidth(dpy, tmp_win->w, 0);
  XGetWMIconName(dpy, tmp_win->w, &text_prop);
  tmp_win->icon_name = (char *) text_prop.value;
  if (tmp_win->icon_name == (char *) NULL)
    tmp_win->icon_name = tmp_win->name;

  tmp_win->fIconified = False;
  tmp_win->fIconUnmapped = False;
  tmp_win->fMaximized = False;

  /* add the window into the scwm list */
  tmp_win->next = Scr.ScwmRoot.next;
  if (Scr.ScwmRoot.next != NULL)
    Scr.ScwmRoot.next->prev = tmp_win;
  tmp_win->prev = &Scr.ScwmRoot;
  Scr.ScwmRoot.next = tmp_win;

  /* create windows */
  tmp_win->frame_x = tmp_win->attr.x + tmp_win->old_bw - tmp_win->bw;
  tmp_win->frame_y = tmp_win->attr.y + tmp_win->old_bw - tmp_win->bw;

  tmp_win->frame_width = tmp_win->attr.width + 2 * tmp_win->boundary_width;
  tmp_win->frame_height = tmp_win->attr.height + tmp_win->title_height +
    2 * tmp_win->boundary_width;
  ConstrainSize(tmp_win, &tmp_win->frame_width, &tmp_win->frame_height);

  valuemask = CWBorderPixel | CWCursor | CWEventMask;
  if (Scr.d_depth < 2) {
    attributes.background_pixmap = Scr.light_gray_pixmap;
    if (tmp_win->fSticky)
      attributes.background_pixmap = Scr.sticky_gray_pixmap;
    valuemask |= CWBackPixmap;
  } else {
    attributes.background_pixel = SAFE_COLOR(tmp_win->BackColor);
    valuemask |= CWBackPixel;
  }

  attributes.border_pixel = SAFE_COLOR(tmp_win->ShadowColor);

  attributes.cursor = Scr.ScwmCursors[CURSOR_DEFAULT];
  attributes.event_mask = (SubstructureRedirectMask | ButtonPressMask |
			   ButtonReleaseMask | EnterWindowMask |
			   LeaveWindowMask | ExposureMask);

  if ((GetDecor(tmp_win, BorderStyle.inactive->style) & ButtonFaceTypeMask)
      == TiledPixmapButton)
    TexturePixmap = IMAGE (GetDecor(tmp_win, 
				    BorderStyle.inactive->u.image))->image;

  if (TexturePixmap) {
    TexturePixmapSave = attributes.background_pixmap;
    attributes.background_pixmap = TexturePixmap;
    valuemask_save = valuemask;
    valuemask = (valuemask & ~CWBackPixel) | CWBackPixmap;
  }

  DBUG(__FUNCTION__,"Now fTitle = %d, th = %d", tmp_win->fTitle, tmp_win->title_height);

  /* What the heck, we'll always reparent everything from now on! */
  DBUG(__FUNCTION__,"Creating child of root window: %d %d, %d x %d, %d",
       tmp_win->frame_x, tmp_win->frame_y,
       tmp_win->frame_width, tmp_win->frame_height,
       tmp_win->bw);

  tmp_win->frame =
    XCreateWindow(dpy, Scr.Root, tmp_win->frame_x, tmp_win->frame_y,
		  tmp_win->frame_width, tmp_win->frame_height,
		  tmp_win->bw, CopyFromParent, InputOutput,
		  CopyFromParent,
		  valuemask,
		  &attributes);

  if (TexturePixmap) {
    attributes.background_pixmap = TexturePixmapSave;
    valuemask = valuemask_save;
  }

  attributes.save_under = False;

  /* Thats not all, we'll double-reparent the window ! */
  attributes.cursor = Scr.ScwmCursors[CURSOR_DEFAULT];
  DBUG(__FUNCTION__,"Creating child of frame: %d %d, %d x %d, %d",
       tmp_win->boundary_width, tmp_win->boundary_width + tmp_win->title_height,
       tmp_win->frame_width - 2 * tmp_win->boundary_width,
       tmp_win->frame_height - 2 * tmp_win->boundary_width - tmp_win->title_height,
       tmp_win->bw);
  tmp_win->Parent =
    XCreateWindow(dpy, tmp_win->frame,
		  tmp_win->boundary_width, 
		  tmp_win->boundary_width + tmp_win->title_height,
		  (tmp_win->frame_width - 2 * tmp_win->boundary_width),
		  (tmp_win->frame_height - 2 * tmp_win->boundary_width -
		   tmp_win->title_height), tmp_win->bw, CopyFromParent,
		  InputOutput, CopyFromParent, valuemask, &attributes);

  attributes.event_mask = (ButtonPressMask | ButtonReleaseMask | ExposureMask |
			   EnterWindowMask | LeaveWindowMask);
  tmp_win->title_x = tmp_win->title_y = 0;
  tmp_win->title_w = 0;
  tmp_win->title_width = tmp_win->frame_width - 2 * tmp_win->corner_width
    - 3 + tmp_win->bw;
  if (tmp_win->title_width < 1)
    tmp_win->title_width = 1;
  if (tmp_win->fBorder) {
    DBUG(__FUNCTION__,"Has border");

    if (TexturePixmap) {
      TexturePixmapSave = attributes.background_pixmap;
      attributes.background_pixmap = TexturePixmap;
      valuemask_save = valuemask;
      valuemask = (valuemask & ~CWBackPixel) | CWBackPixmap;
    }
    /* Just dump the windows any old place and left SetupFrame take
     * care of the mess */
    for (i = 0; i < 4; i++) {
      attributes.cursor = Scr.ScwmCursors[CURSOR_TOP_LEFT + i];
      tmp_win->corners[i] =
	XCreateWindow(dpy, tmp_win->frame, 0, 0,
		      tmp_win->corner_width, tmp_win->corner_width,
		      0, CopyFromParent, InputOutput,
		      CopyFromParent,
		      valuemask,
		      &attributes);
    }
    if (TexturePixmap) {
      attributes.background_pixmap = TexturePixmapSave;
      valuemask = valuemask_save;
    }
  }

  /* We always create the title bar since we can dynamically show or hide it */
  tmp_win->title_x = tmp_win->boundary_width + tmp_win->title_height + 1;
  tmp_win->title_y = tmp_win->boundary_width;
  attributes.cursor = Scr.ScwmCursors[CURSOR_TITLE];
  DBUG(__FUNCTION__,"Creating title window: %d %d, %d x %d",
       tmp_win->title_x, tmp_win->title_y,
       tmp_win->title_width, tmp_win->title_height);
  tmp_win->title_w =
    XCreateWindow(dpy, tmp_win->frame, tmp_win->title_x, tmp_win->title_y,
                  tmp_win->title_width, tmp_win->title_height, 0,
                  CopyFromParent, InputOutput, CopyFromParent,
                  valuemask, &attributes);
  attributes.cursor = Scr.ScwmCursors[CURSOR_SYS];
  for (i = 4; i >= 0; i--) {
    if ((i < Scr.nr_left_buttons) && (tmp_win->left_w[i] > 0)) {
      if (TexturePixmap
          && GetDecor(tmp_win, left_buttons[i].flags) & UseBorderStyle) {
        TexturePixmapSave = attributes.background_pixmap;
        attributes.background_pixmap = TexturePixmap;
        valuemask_save = valuemask;
        valuemask = (valuemask & ~CWBackPixel) | CWBackPixmap;
      }
      DBUG(__FUNCTION__,"Creating left button %d",i);
      tmp_win->left_w[i] =
        XCreateWindow(dpy, tmp_win->frame, tmp_win->title_height * i, 0,
                      tmp_win->title_height, tmp_win->title_height, 0,
                      CopyFromParent, InputOutput,
                      CopyFromParent,
                      valuemask,
                      &attributes);
      if (TexturePixmap
          && GetDecor(tmp_win, left_buttons[i].flags) & UseBorderStyle) {
        attributes.background_pixmap = TexturePixmapSave;
        valuemask = valuemask_save;
      }
    } else
      tmp_win->left_w[i] = None;
    
    if ((i < Scr.nr_right_buttons) && (tmp_win->right_w[i] > 0)) {
      if (TexturePixmap
          && GetDecor(tmp_win, right_buttons[i].flags) & UseBorderStyle) {
        TexturePixmapSave = attributes.background_pixmap;
        attributes.background_pixmap = TexturePixmap;
        valuemask_save = valuemask;
        valuemask = (valuemask & ~CWBackPixel) | CWBackPixmap;
      }
      DBUG(__FUNCTION__,"Creating right button %d",i);
      tmp_win->right_w[i] =
        XCreateWindow(dpy, tmp_win->frame,
                      tmp_win->title_width -
                      tmp_win->title_height * (i + 1),
                      0, tmp_win->title_height,
                      tmp_win->title_height,
                      0, CopyFromParent, InputOutput,
                      CopyFromParent,
                      valuemask,
                      &attributes);
      if (TexturePixmap
          && GetDecor(tmp_win, right_buttons[i].flags) & UseBorderStyle) {
        attributes.background_pixmap = TexturePixmapSave;
        valuemask = valuemask_save;
      }
    } else
      tmp_win->right_w[i] = None;
  }

  if (tmp_win->fBorder) {
    if (TexturePixmap) {
      TexturePixmapSave = attributes.background_pixmap;
      attributes.background_pixmap = TexturePixmap;
      valuemask_save = valuemask;
      valuemask = (valuemask & ~CWBackPixel) | CWBackPixmap;
    }
    for (i = 0; i < 4; i++) {
      attributes.cursor = Scr.ScwmCursors[CURSOR_TOP + i];
      DBUG(__FUNCTION__,"Creating side %d",i);
      tmp_win->sides[i] =
	XCreateWindow(dpy, tmp_win->frame, 0, 0, tmp_win->boundary_width,
		      tmp_win->boundary_width, 0, CopyFromParent,
		      InputOutput, CopyFromParent,
		      valuemask,
		      &attributes);
    }
    if (TexturePixmap) {
      attributes.background_pixmap = TexturePixmapSave;
      valuemask = valuemask_save;
    }
  }

  XMapSubwindows(dpy, tmp_win->frame);
  XRaiseWindow(dpy, tmp_win->Parent);
  XReparentWindow(dpy, tmp_win->w, tmp_win->Parent, 0, 0);

  valuemask = (CWEventMask | CWDontPropagate);
  attributes.event_mask = (StructureNotifyMask | PropertyChangeMask |
			   VisibilityChangeMask | EnterWindowMask |
			   LeaveWindowMask |
			   ColormapChangeMask | FocusChangeMask);

  attributes.do_not_propagate_mask = ButtonPressMask | ButtonReleaseMask;

  XChangeWindowAttributes(dpy, tmp_win->w, valuemask, &attributes);
  if (XGetWMName(dpy, tmp_win->w, &text_prop) != 0)
    tmp_win->name = (char *) text_prop.value;
  else
    tmp_win->name = NoName;

  XAddToSaveSet(dpy, tmp_win->w);

  /*
   * Reparenting generates an UnmapNotify event, followed by a MapNotify.
   * Set the map state to False to prevent a transition back to
   * WithdrawnState in HandleUnmapNotify.  Map state gets set correctly
   * again in HandleMapNotify.
   */
  tmp_win->fMapped = False;
  width = tmp_win->frame_width;
  tmp_win->frame_width = 0;
  height = tmp_win->frame_height;
  tmp_win->frame_height = 0;

  /* Since we forced the width and height to be different from what is in 
     tmp_win->frame_width,frame_height, SetupFrame will pretend it has been
     resized and deal accordingly. --03/27/98 gjb */
  SetupFrame(tmp_win, tmp_win->frame_x, tmp_win->frame_y, width, height, True);


  /* wait until the window is iconified and the icon window is mapped
   * before creating the icon window 
   */
  tmp_win->icon_w = None;
  GrabButtons(tmp_win);
  GrabKeys(tmp_win);

  XSaveContext(dpy, tmp_win->w, ScwmContext, (caddr_t) tmp_win);
  XSaveContext(dpy, tmp_win->frame, ScwmContext, (caddr_t) tmp_win);
  XSaveContext(dpy, tmp_win->Parent, ScwmContext, (caddr_t) tmp_win);
  XSaveContext(dpy, tmp_win->title_w, ScwmContext, (caddr_t) tmp_win);

  /* Associate this scwm window with the decoration X windows */
  for (i = 0; i < Scr.nr_left_buttons; i++)
    XSaveContext(dpy, tmp_win->left_w[i], ScwmContext, (caddr_t) tmp_win);
  for (i = 0; i < Scr.nr_right_buttons; i++)
    if (tmp_win->right_w[i] != None)
      XSaveContext(dpy, tmp_win->right_w[i], ScwmContext,
                   (caddr_t) tmp_win);

  if (tmp_win->fBorder) {
    for (i = 0; i < 4; i++) {
      XSaveContext(dpy, tmp_win->sides[i], ScwmContext, (caddr_t) tmp_win);
      XSaveContext(dpy, tmp_win->corners[i], ScwmContext, (caddr_t) tmp_win);
    }
  }
  
  RaiseWindow(tmp_win);
  KeepOnTop();
  XUngrabServer_withSemaphore(dpy);

  /* Without calling XGetGeometryCacheIt(), windows will not restart
     in their proper positions -- we do not need any return value from
     the X server, but apparently we need to ask the server for the geometry
     of the window.... go figure! --03/29/98 gjb */
  XGetGeometryCacheIt(dpy, tmp_win->w);
  
  XTranslateCoordinates(dpy, tmp_win->frame, Scr.Root, JunkX, JunkY,
			&a, &b, &JunkChild);
  tmp_win->xdiff -= a;
  tmp_win->ydiff -= b;
  if (tmp_win->fClickToFocus) {
    /* need to grab all buttons for window that we are about to
       * unhighlight */
    for (i = 0; i < 3; i++)
      if (Scr.buttons2grab & (1 << i)) {
	XGrabButton(dpy, (i + 1), 0, tmp_win->frame, True,
		    ButtonPressMask, GrabModeSync, GrabModeAsync, None,
		    Scr.ScwmCursors[CURSOR_SYS]);
	XGrabButton(dpy, (i + 1), LockMask, tmp_win->frame, True,
		    ButtonPressMask, GrabModeSync, GrabModeAsync, None,
		    Scr.ScwmCursors[CURSOR_SYS]);
      }
  }

  BroadcastConfig(M_ADD_WINDOW, tmp_win);

  BroadcastName(M_WINDOW_NAME, tmp_win->w, tmp_win->frame,
		(unsigned long) tmp_win, tmp_win->name);
  BroadcastName(M_ICON_NAME, tmp_win->w, tmp_win->frame,
		(unsigned long) tmp_win, tmp_win->icon_name);
  /* MSFIX: FIXGJB: It'd be really nice to get full pathname of
     the picture into the image object for debugging of scwmrc-s;
     then this could go back in, too, though I imagine it's
     rarely used --gjb 11/28/97  */
  /*if (tmp_win->szIconFile != NULL &&
      tmp_win->szIconFile != Scr.DefaultIcon)
    BroadcastName(M_ICON_FILE, tmp_win->w, tmp_win->frame,
    (unsigned long) tmp_win, tmp_win->szIconFile); */
  BroadcastName(M_RES_CLASS, tmp_win->w, tmp_win->frame,
		(unsigned long) tmp_win, tmp_win->classhint.res_class);
  BroadcastName(M_RES_NAME, tmp_win->w, tmp_win->frame,
		(unsigned long) tmp_win, tmp_win->classhint.res_name);
  if (tmp_win->mini_icon_image != SCM_BOOL_F) {
    Broadcast(M_MINI_ICON, 6,
	      tmp_win->w,	/* Watch Out ! : I reduced the set of infos... */
	      IMAGE(tmp_win->mini_icon_image)->image,
	      IMAGE(tmp_win->mini_icon_image)->mask,
	      IMAGE(tmp_win->mini_icon_image)->width,
	      IMAGE(tmp_win->mini_icon_image)->height,
	      IMAGE(tmp_win->mini_icon_image)->depth, 0);
  }

  FetchWmProtocols(tmp_win);
  FetchWmColormapWindows(tmp_win);
  if (!(XGetWindowAttributes(dpy, tmp_win->w, &(tmp_win->attr))))
    tmp_win->attr.colormap = Scr.ScwmRoot.attr.colormap;

  if (NeedToResizeToo) {
    XWarpPointer(dpy, Scr.Root, Scr.Root, 0, 0, Scr.MyDisplayWidth,
		 Scr.MyDisplayHeight,
		 tmp_win->frame_x + (tmp_win->frame_width >> 1),
		 tmp_win->frame_y + (tmp_win->frame_height >> 1));
    Event.xany.type = ButtonPress;
    Event.xbutton.button = 1;
    Event.xbutton.x_root = tmp_win->frame_x + (tmp_win->frame_width >> 1);
    Event.xbutton.y_root = tmp_win->frame_y + (tmp_win->frame_height >> 1);
    Event.xbutton.x = (tmp_win->frame_width >> 1);
    Event.xbutton.y = (tmp_win->frame_height >> 1);
    Event.xbutton.subwindow = None;
    Event.xany.window = tmp_win->w;
    interactive_resize(tmp_win->schwin);
  }
  InstallWindowColormaps(colormap_win);

  call1_hooks(after_new_window_hook, tmp_win->schwin);
  CreateIconWindow(tmp_win,tmp_win->icon_x_loc,tmp_win->icon_y_loc);

  return (tmp_win);
}

void
GrabButtonWithModifiers(int button, int modifier, 
			ScwmWindow *sw)
{
  if (button > 0) {
    XGrabButton(dpy, button, modifier, sw->w,
		True, ButtonPressMask | ButtonReleaseMask,
		GrabModeAsync, GrabModeAsync, None,
		Scr.ScwmCursors[CURSOR_DEFAULT]);
    if (modifier != AnyModifier) {
      XGrabButton(dpy, button, (modifier | LockMask), sw->w,
		  True, ButtonPressMask | ButtonReleaseMask,
		  GrabModeAsync, GrabModeAsync, None,
		  Scr.ScwmCursors[CURSOR_DEFAULT]);
    }
  } else {
    GrabButtonWithModifiers(1,modifier,sw);
    GrabButtonWithModifiers(2,modifier,sw);
    GrabButtonWithModifiers(3,modifier,sw);
  }
}
  

void
UngrabButtonWithModifiers(int button, int modifier, 
			  ScwmWindow *sw)
{
  if (button > 0) {
    XUngrabButton(dpy, button, modifier, sw->w);
    if (modifier != AnyModifier) {
      XUngrabButton(dpy, button, (modifier | LockMask), sw->w);
    }
  } else {
    UngrabButtonWithModifiers(1,modifier,sw);
    UngrabButtonWithModifiers(2,modifier,sw);
    UngrabButtonWithModifiers(3,modifier,sw);
  }
}
  
/***********************************************************************
 *
 *  Procedure:
 *	FetchWMProtocols - finds out which protocols the window supports
 *
 *  Inputs:
 *	tmp - the scwm window structure to use
 *
 ***********************************************************************/
void 
FetchWmProtocols(ScwmWindow * tmp)
{
  Atom *protocols = NULL, *ap;
  int i, n;
  Atom atype;
  int aformat;
  unsigned long bytes_remain, nitems;

  if (tmp == NULL)
    return;
  /* First, try the Xlib function to read the protocols.
   * This is what Twm uses. */
  if (XGetWMProtocols(dpy, tmp->w, &protocols, &n)) {
    for (i = 0, ap = protocols; i < n; i++, ap++) {
      if (*ap == (Atom) _XA_WM_TAKE_FOCUS)
	tmp->fDoesWmTakeFocus = True;
      if (*ap == (Atom) _XA_WM_DELETE_WINDOW)
	tmp->fDoesWmDeleteWindow = True;
    }
    if (protocols)
      XFree((char *) protocols);
  } else {
    /* Next, read it the hard way. mosaic from Coreldraw needs to 
     * be read in this way. */
    if ((XGetWindowProperty(dpy, tmp->w, _XA_WM_PROTOCOLS, 0L, 10L, False,
			    _XA_WM_PROTOCOLS, &atype, &aformat, &nitems,
			    &bytes_remain,
			    (unsigned char **) &protocols)) == Success) {
      for (i = 0, ap = protocols; i < nitems; i++, ap++) {
	if (*ap == (Atom) _XA_WM_TAKE_FOCUS)
	  tmp->fDoesWmTakeFocus = True;
	if (*ap == (Atom) _XA_WM_DELETE_WINDOW)
	  tmp->fDoesWmDeleteWindow = True;
      }
      if (protocols)
	XFree((char *) protocols);
    }
  }
  return;
}

/***********************************************************************
 *
 *  Procedure:
 *	GetWindowSizeHints - gets application supplied size info
 *
 *  Inputs:
 *	tmp - the scwm window structure to use
 *
 ***********************************************************************/
void 
GetWindowSizeHints(ScwmWindow * tmp)
{
  long supplied = 0;

  if (!XGetWMNormalHints(dpy, tmp->w, &tmp->hints, &supplied))
    tmp->hints.flags = 0;

  /* Beat up our copy of the hints, so that all important field are
   * filled in! */
  if (tmp->hints.flags & PResizeInc) {
    if (tmp->hints.width_inc == 0)
      tmp->hints.width_inc = 1;
    if (tmp->hints.height_inc == 0)
      tmp->hints.height_inc = 1;
  } else {
    tmp->hints.width_inc = 1;
    tmp->hints.height_inc = 1;
  }

  /*
   * ICCCM says that PMinSize is the default if no PBaseSize is given,
   * and vice-versa.
   */

  if (!(tmp->hints.flags & PBaseSize)) {
    if (tmp->hints.flags & PMinSize) {
      tmp->hints.base_width = tmp->hints.min_width;
      tmp->hints.base_height = tmp->hints.min_height;
    } else {
      tmp->hints.base_width = 0;
      tmp->hints.base_height = 0;
    }
  }
  if (!(tmp->hints.flags & PMinSize)) {
    tmp->hints.min_width = tmp->hints.base_width;
    tmp->hints.min_height = tmp->hints.base_height;
  }
  if (!(tmp->hints.flags & PMaxSize)) {
    tmp->hints.max_width = MAX_WINDOW_WIDTH;
    tmp->hints.max_height = MAX_WINDOW_HEIGHT;
  }
  if (tmp->hints.max_width < tmp->hints.min_width)
    tmp->hints.max_width = MAX_WINDOW_WIDTH;
  if (tmp->hints.max_height < tmp->hints.min_height)
    tmp->hints.max_height = MAX_WINDOW_HEIGHT;

  /* Zero width/height windows are bad news! */
  if (tmp->hints.min_height <= 0)
    tmp->hints.min_height = 1;
  if (tmp->hints.min_width <= 0)
    tmp->hints.min_width = 1;

  if (!(tmp->hints.flags & PWinGravity)) {
    tmp->hints.win_gravity = NorthWestGravity;
    tmp->hints.flags |= PWinGravity;
  }
}

void init_add_window()
{
  DEFINE_HOOK(before_new_window_hook, "before-new-window-hook");
  DEFINE_HOOK(after_new_window_hook, "after-new-window-hook");
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
