/* $Id$
 * icons.c 
 */

/****************************************************************************
 * This module is derived from mostly all new code written
 * by Rob Nation 
 * A little of it is borrowed from ctwm.
 * Copyright 1993 Robert Nation. No restrictions are placed on this code,
 * as long as the copyright notice is preserved
 ****************************************************************************/
/***********************************************************************
 *
 * scwm icon code
 *
 ***********************************************************************/

#include <config.h>

#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>

#ifdef NeXT
#include <fcntl.h>
#endif

#include <X11/Intrinsic.h>
#include <X11/xpm.h>
#include "scwm.h"
#include "misc.h"
#include "screen.h"
#include "icons.h"
#include "borders.h"
#include "module-interface.h"
#include "binding.h"
#include "font.h"
#include "color.h"
#include "focus.h"

#include <X11/extensions/shape.h>

/***********************************************************************
 *
 *  Procedure:
 *	GrabIconButtons - grab needed buttons for the icon window
 *
 *  Inputs:
 *	tmp_win - the scwm window structure to use
 *
 ***********************************************************************/
static void 
GrabIconButtons(ScwmWindow * tmp_win, Window w)
{
  Binding *MouseEntry;

  MouseEntry = Scr.AllBindings;
  while (MouseEntry != (Binding *) 0) {
    if ((MouseEntry->Action != NULL) && (MouseEntry->Context & C_ICON) &&
	(MouseEntry->IsMouse == 1)) {
      if (MouseEntry->Button_Key > 0)
	XGrabButton(dpy, MouseEntry->Button_Key, MouseEntry->Modifier, w,
		    True, ButtonPressMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, None,
		    Scr.ScwmCursors[CURSOR_DEFAULT]);
      else {
	XGrabButton(dpy, 1, MouseEntry->Modifier, w,
		    True, ButtonPressMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, None,
		    Scr.ScwmCursors[CURSOR_DEFAULT]);
	XGrabButton(dpy, 2, MouseEntry->Modifier, w,
		    True, ButtonPressMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, None,
		    Scr.ScwmCursors[CURSOR_DEFAULT]);
	XGrabButton(dpy, 3, MouseEntry->Modifier, w,
		    True, ButtonPressMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, None,
		    Scr.ScwmCursors[CURSOR_DEFAULT]);
      }
    }
    MouseEntry = MouseEntry->NextBinding;
  }
  return;
}



/***********************************************************************
 *
 *  Procedure:
 *	GrabIconKeys - grab needed keys for the icon window
 *
 *  Inputs:
 *	tmp_win - the scwm window structure to use
 *
 ***********************************************************************/
static void 
GrabIconKeys(ScwmWindow * tmp_win, Window w)
{
  Binding *tmp;

  for (tmp = Scr.AllBindings; tmp != NULL; tmp = tmp->NextBinding) {
    if ((tmp->Context & C_ICON) && (tmp->IsMouse == 0))
      XGrabKey(dpy, tmp->Button_Key, tmp->Modifier, w, True,
	       GrabModeAsync, GrabModeAsync);
  }
  return;
}

/****************************************************************************
 *
 * Looks for an application supplied icon window
 *
 ****************************************************************************/
static void 
GetIconWindow(ScwmWindow * sw)
{
  Pixmap picture;
  Pixmap mask = None;
  int width;
  int height;
  int border_width;
  int depth;

  /* We are guaranteed that wmhints is non-null when calling this routine */
  if (XGetGeometry(dpy, sw->wmhints->icon_window, &JunkRoot,
		   &JunkX, &JunkY, 
		   &width, &height,
		   &border_width, &depth) == 0) {
    scwm_msg(ERR, "GetIconWindow", "Help! Bad Icon Window!");
    return;
  }
  picture = sw->wmhints->icon_pixmap;
  width += border_width * 2;
  height += border_width * 2;

  /*
   * Now make the new window the icon window for this window,
   * and set it up to work as such (select for key presses
   * and button presses/releases, set up the contexts for it,
   * and define the cursor for it).
   */
  sw->icon_pixmap_w = sw->wmhints->icon_window;

  if (ShapesSupported) {
    if (sw->wmhints->flags & IconMaskHint) {
      sw->fShapedIcon = True;
      mask = sw->wmhints->icon_mask;
    }
  }
  /* Make sure that the window is a child of the root window ! */
  /* Olwais screws this up, maybe others do too! */
  XReparentWindow(dpy, sw->icon_pixmap_w, Scr.Root, 0, 0);
  sw->fIconOurs = False;

  /* and finally add this picture to the ScwmWindow */
  sw->icon_image = make_image_from_pixmap("FromApp",picture,mask,width,height,depth);
  IMAGE(sw->icon_image)->foreign=1;
}


/****************************************************************************
 *
 * Looks for an application supplied bitmap or pixmap
 *
 ****************************************************************************/
static void 
GetIconBitmap(ScwmWindow *sw)
{
  Pixmap picture;
  Pixmap mask = None;
  int width;
  int height;
  int depth;

  /* We are guaranteed that wmhints is non-null when calling this
   * routine */
  if (XGetGeometry(dpy, sw->wmhints->icon_pixmap, &JunkRoot, &JunkX, &JunkY,
		   &width, &height,
		   &JunkBW, &depth) == 0) {
    scwm_msg(ERR, __FUNCTION__, "Help! Bad Icon bitmap!");
    return;
  }
  picture = sw->wmhints->icon_pixmap;
  if (ShapesSupported) {
    if (sw->wmhints->flags & IconMaskHint) {
      sw->fShapedIcon = True;
      mask = sw->wmhints->icon_mask;
    }
  }

  /* and finally add this picture to the ScwmWindow */
  sw->icon_image = make_image_from_pixmap("FromAppBitmap",
					  picture,mask,
					  width,height,depth);
  IMAGE(sw->icon_image)->foreign=1;
}

/************************************************************************
 ************************************************************************
 * Public functions below here
 ************************************************************************
 ************************************************************************/


/****************************************************************************
 *
 * Creates an icon window as needed
 *
 ****************************************************************************/
void 
CreateIconWindow(ScwmWindow * sw, int def_x, int def_y)
{
  int final_x, final_y;
  unsigned long valuemask;	/* mask for create windows */
  XSetWindowAttributes attributes;	/* attributes for create windows */

  sw->fIconOurs = True;
  sw->fPixmapOurs = False;
  sw->fShapedIcon = False;
  sw->icon_pixmap_w = None;

  if (sw->fSuppressIcon)
     return;


  /* If the icon is forced, use the requested icon no matter what. */

  if (sw->fForceIcon) {
    sw->icon_image=sw->icon_req_image;
  } else if ((sw->wmhints) && (sw->wmhints->flags & IconWindowHint)) {
    /* Next, See if the app supplies its own icon window */
    GetIconWindow(sw);
  } else if ((sw->wmhints) && (sw->wmhints->flags & IconPixmapHint)) {
    /* Next, try to get icon bitmap from the application */
    GetIconBitmap(sw);
  } else {
    /* If all else fails, use the requested icon anyway. */
    sw->icon_image=sw->icon_req_image;
  }

  if (ShapesSupported && sw->icon_image != SCM_BOOL_F && 
      IMAGE(sw->icon_image)->mask!=None) {
    sw->fShapedIcon = True;
  }

  /* FIXGJB: we need a way of setting an icon here if we've not got
     one already; e.g., a user should be able to specify a default
     icon in case none can be found in any of the previous places.
     Just using a default as it is now lets that icon take priority
     over any icon window or bitmap window that the application might
     provide.  Perhaps :icon and `:forced-icon' or something like
     that, where the #:icon behaviour allows the application to
     override, and the forced-icon says we always want a specific icon
  */

  
  /* FIXMS: You should be able to set separately whether you want icon
     titles or icon images or both. */

  /* figure out the icon window size */
  if (!sw->fNoIconTitle || sw->icon_p_height == 0) {
    sw->icon_t_width = XTextWidth(XFONT(Scr.icon_font),
				       sw->icon_name,
				       strlen(sw->icon_name));
    sw->icon_w_height = ICON_HEIGHT;
  } else {
    sw->icon_t_width = 0;
    sw->icon_w_height = 0;
  }

  if (sw->icon_image != SCM_BOOL_F) {
    sw->icon_p_height = IMAGE(sw->icon_image)->height + 
      (sw->fIconOurs ? 4 : 0);
    sw->icon_p_width = IMAGE(sw->icon_image)->width + 
      (sw->fIconOurs ? 4 : 0);  
  } else {
    sw->icon_p_height = 0;
    sw->icon_p_width = sw->icon_t_width + 6;
  }

  sw->icon_w_width = sw->icon_p_width;


  /* Not having an icon picture should not throw an error,
     it is a valid state! All it means is that we don't want an icon
     picture at all, just the icon title. - MS 11/19/97 */

  final_x = def_x;
  final_y = def_y;
  if (final_x < 0)
    final_x = 0;
  if (final_y < 0)
    final_y = 0;

  if (final_x + sw->icon_w_width >= Scr.MyDisplayWidth)
    final_x = Scr.MyDisplayWidth - sw->icon_w_width - 1;
  if (final_y + sw->icon_w_height >= Scr.MyDisplayHeight)
    final_y = Scr.MyDisplayHeight - sw->icon_w_height - 1;

  sw->icon_x_loc = final_x;
  sw->icon_xl_loc = final_x;
  sw->icon_y_loc = final_y;

  /* clip to fit on screen */
  attributes.background_pixel = XCOLOR(Scr.MenuColors.bg);
  valuemask = CWBorderPixel | CWCursor | CWEventMask | CWBackPixel;
  attributes.border_pixel = XCOLOR(Scr.MenuColors.fg);
  attributes.cursor = Scr.ScwmCursors[CURSOR_DEFAULT];
  attributes.event_mask = (ButtonPressMask | ButtonReleaseMask |
			   VisibilityChangeMask |
			   ExposureMask | KeyPressMask | EnterWindowMask |
			   FocusChangeMask);
  if (!sw->fNoIconTitle || (sw->icon_p_height == 0))
    sw->icon_w =
      XCreateWindow(dpy, Scr.Root, final_x, final_y + sw->icon_p_height,
		    sw->icon_w_width, sw->icon_w_height, 0,
		    CopyFromParent,
		    CopyFromParent, CopyFromParent, valuemask, &attributes);
  
  /* sw->icon_p_width should always be > 0 here - MS 2-19-98 */
  if ((sw->fIconOurs) /* && sw->icon_p_width > 0 */ 
      && sw->icon_p_height > 0) {
    sw->icon_pixmap_w =
      XCreateWindow(dpy, Scr.Root, final_x, final_y, sw->icon_p_width,
		    sw->icon_p_height, 0, CopyFromParent,
		    CopyFromParent, CopyFromParent, valuemask, &attributes);
  } else {
    attributes.event_mask = (ButtonPressMask | ButtonReleaseMask |
			     VisibilityChangeMask |
			     KeyPressMask | EnterWindowMask |
			     FocusChangeMask | LeaveWindowMask);
    
    valuemask = CWEventMask;
    XChangeWindowAttributes(dpy, sw->icon_pixmap_w,
			    valuemask, &attributes);
  }


  if (ShapesSupported && sw->fShapedIcon &&
    sw->icon_image != SCM_BOOL_F) {
    XShapeCombineMask(dpy, sw->icon_pixmap_w, ShapeBounding, 2, 2,
		      IMAGE(sw->icon_image)->mask, ShapeSet);
  }

  if (sw->icon_w != None) {
    XSaveContext(dpy, sw->icon_w, ScwmContext, (caddr_t) sw);
    XDefineCursor(dpy, sw->icon_w, Scr.ScwmCursors[CURSOR_DEFAULT]);
    GrabIconButtons(sw, sw->icon_w);
    GrabIconKeys(sw, sw->icon_w);
  }
  if (sw->icon_pixmap_w != None) {
    XSaveContext(dpy, sw->icon_pixmap_w, ScwmContext, (caddr_t) sw);
    XDefineCursor(dpy, sw->icon_pixmap_w, Scr.ScwmCursors[CURSOR_DEFAULT]);
    GrabIconButtons(sw, sw->icon_pixmap_w);
    GrabIconKeys(sw, sw->icon_pixmap_w);
  }

  return;
}

/****************************************************************************
 *
 * Draws the icon window
 *
 ****************************************************************************/
void 
DrawIconWindow(ScwmWindow * sw)
{
  GC Shadow, Relief;
  Pixel TextColor, BackColor;
  int x;

  if (sw->fSuppressIcon)
    return;

  if (sw->icon_w != None)
    flush_expose(sw->icon_w);
  if (sw->icon_pixmap_w != None)
    flush_expose(sw->icon_pixmap_w);

  if (Scr.Hilite == sw) {
    /* FIXMS: This can't poossibly be right. */
    if (Scr.d_depth < 2) {
      Relief =
	Shadow = Scr.DefaultDecor.HiShadowGC;
      TextColor = XCOLOR(Scr.DefaultDecor.HiColors.fg);
      BackColor = XCOLOR(Scr.DefaultDecor.HiColors.bg);
    } else {
      Relief = GetDecor(sw, HiReliefGC);
      Shadow = GetDecor(sw, HiShadowGC);
      TextColor = XCOLOR(GetDecor(sw, HiColors.fg));
      BackColor = XCOLOR(GetDecor(sw, HiColors.bg));
    }
    /* resize the icon name window */
    if (sw->icon_w != None) {
      sw->icon_w_width = sw->icon_t_width + 6;
      if (sw->icon_w_width < sw->icon_p_width)
	sw->icon_w_width = sw->icon_p_width;
      sw->icon_xl_loc = sw->icon_x_loc -
	(sw->icon_w_width - sw->icon_p_width) / 2;
    }
  } else {
    if (Scr.d_depth < 2) {
      Relief = Scr.MenuGC;
      Shadow = Scr.MenuGC;
    } else {
      Globalgcv.foreground = XCOLOR(sw->ReliefColor);
      Globalgcm = GCForeground;
      XChangeGC(dpy, Scr.ScratchGC1, Globalgcm, &Globalgcv);
      Relief = Scr.ScratchGC1;

      Globalgcv.foreground = XCOLOR(sw->ShadowColor);
      XChangeGC(dpy, Scr.ScratchGC2, Globalgcm, &Globalgcv);
      Shadow = Scr.ScratchGC2;
    }
    /* resize the icon name window */
    if (sw->icon_w != None) {
      sw->icon_w_width = sw->icon_p_width;
      sw->icon_xl_loc = sw->icon_x_loc;
    }
    TextColor = XCOLOR(sw->TextColor);
    BackColor = XCOLOR(sw->BackColor);

  }
  if (sw->fIconOurs && (sw->icon_pixmap_w != None)) {
    XSetWindowBackground(dpy, sw->icon_pixmap_w,
			 BackColor);
  }
  if (sw->icon_w != None)
    XSetWindowBackground(dpy, sw->icon_w, BackColor);

  /* write the icon label */
  NewFontAndColor(XFONT(Scr.icon_font)->fid, TextColor, BackColor);

  if (sw->icon_pixmap_w != None)
    XMoveWindow(dpy, sw->icon_pixmap_w, sw->icon_x_loc,
		sw->icon_y_loc);

  if (sw->icon_w != None) {
    sw->icon_w_height = ICON_HEIGHT;
    XMoveResizeWindow(dpy, sw->icon_w, sw->icon_xl_loc,
		      sw->icon_y_loc + sw->icon_p_height,
		      sw->icon_w_width, ICON_HEIGHT);

    XClearWindow(dpy, sw->icon_w);
  }


  if (sw->fIconOurs) {
    if ((sw->icon_image != SCM_BOOL_F) &&
	!sw->fShapedIcon) {
      RelieveWindow(sw, sw->icon_pixmap_w, 0, 0,
		    sw->icon_p_width, sw->icon_p_height,
		    Relief, Shadow, FULL_HILITE);
    }

  /* need to locate the icon pixmap */
    if (sw->icon_image != SCM_BOOL_F) {
      if (IMAGE(sw->icon_image)->depth == Scr.d_depth) {
	XCopyArea(dpy, IMAGE(sw->icon_image)->image, 
		  sw->icon_pixmap_w, Scr.ScratchGC3,
		  0, 0, sw->icon_p_width-4,
		  sw->icon_p_height-4, 2, 2);
      } else {
	XCopyPlane(dpy, IMAGE(sw->icon_image)->image, 
		   sw->icon_pixmap_w, Scr.ScratchGC3, 0,
		   0, sw->icon_p_width-4, sw->icon_p_height-4,
		   2, 2, 1);
      }
    }
  }

  if (sw->icon_w != None) {
    /* text position */
    x = (sw->icon_w_width - sw->icon_t_width) / 2;
    if (x < 3)
      x = 3;

    XDrawString(dpy, sw->icon_w, Scr.ScratchGC3, x,
		sw->icon_w_height - FONTHEIGHT(Scr.icon_font) +
		FONTY(Scr.icon_font) - 3,
		sw->icon_name, strlen(sw->icon_name));
    RelieveWindow(sw, sw->icon_w, 0, 0, sw->icon_w_width,
		  ICON_HEIGHT, Relief, Shadow, FULL_HILITE);
  }
}

/***********************************************************************
 *
 *  Procedure:
 *	RedoIconName - procedure to re-position the icon window and name
 *
 ************************************************************************/
void 
RedoIconName(ScwmWindow *psw)
{

  if (psw->fSuppressIcon)
    return;

  if (psw->icon_w == 0)
    return;

  psw->icon_t_width = XTextWidth(XFONT(Scr.icon_font), psw->icon_name,
				 strlen(psw->icon_name));
  /* clear the icon window, and trigger a re-draw via an expose event */
  if (psw->fIconified) {
    XClearArea(dpy, psw->icon_w, 0, 0, 0, 0, True);
  }

  return;
}


/***********************************************************************
 *
 *  Procedure:
 *	AutoPlace - Find a home for an icon
 *
 ************************************************************************/
void 
AutoPlace(ScwmWindow * t)
{
  int test_x = 0, test_y = 0, tw, th, tx, ty, temp_h, temp_w;
  int base_x, base_y;
  int width = 0;
  int height = 0;
  ScwmWindow *test_window;
  Bool loc_ok;
  int real_x = 10, real_y = 10;
  int new_x, new_y;


  /* New! Put icon in same page as the center of the window */
  /* Not a good idea for fStickyIcon */
  if (t->fStickyIcon || t->fSticky) {
    base_x = 0;
    base_y = 0;
    /*Also, if its a stickyWindow, put it on the current page! */
    new_x = t->frame_x % Scr.MyDisplayWidth;
    new_y = t->frame_y % Scr.MyDisplayHeight;
    if (new_x < 0)
      new_x += Scr.MyDisplayWidth;
    if (new_y < 0)
      new_y += Scr.MyDisplayHeight;
    SetupFrame(t, new_x, new_y,
	       t->frame_width, t->frame_height, False);
    t->Desk = Scr.CurrentDesk;
  } else {
    base_x = ((t->frame_x + Scr.Vx + (t->frame_width >> 1)) / Scr.MyDisplayWidth) *
      Scr.MyDisplayWidth - Scr.Vx;
    base_y = ((t->frame_y + Scr.Vy + (t->frame_height >> 1)) / Scr.MyDisplayHeight) *
      Scr.MyDisplayHeight - Scr.Vy;
  }
  if (t->fIconMoved) {
    /* just make sure the icon is on this screen */
    t->icon_x_loc = t->icon_x_loc % Scr.MyDisplayWidth + base_x;
    t->icon_y_loc = t->icon_y_loc % Scr.MyDisplayHeight + base_y;
    if (t->icon_x_loc < 0)
      t->icon_x_loc += Scr.MyDisplayWidth;
    if (t->icon_y_loc < 0)
      t->icon_y_loc += Scr.MyDisplayHeight;
  } else if (t->wmhints && t->wmhints->flags & IconPositionHint) {
    t->icon_x_loc = t->wmhints->icon_x;
    t->icon_y_loc = t->wmhints->icon_y;
  } else if (t->IconBox[0] >= 0) {
    width = t->icon_p_width;
    height = t->icon_p_height + t->icon_w_height;
    loc_ok = False;

    /* check all boxes in order */
    /* In each IconBox, start at the upper left, travel right, then
     * down */
    test_y = t->IconBox[1] + base_y;

    temp_h = height;
    temp_w = width;

    /* OK second try at this.
     * If the window is taller than the icon box, ignore the icon height
     * when figuring where to put it. Same goes for the width */
    /* This should permit reasonably graceful handling of big icons. */
    if (width >= (t->IconBox[2] - t->IconBox[0]))
      temp_w = 0;
    if (height >= (t->IconBox[3] - t->IconBox[1]))
      temp_h = 0;

    while (((test_y + temp_h) < (t->IconBox[3] + base_y)) && (!loc_ok)) {
      test_x = t->IconBox[0] + base_x;
      while (((test_x + temp_w) < (t->IconBox[2] + base_x)) &&
	     (!loc_ok)) {
	real_x = test_x;
	real_y = test_y;

	if (test_x + width > (Scr.MyDisplayWidth - 2 + base_x))
	  real_x = Scr.MyDisplayWidth - width - 2 + base_x;
	if (test_y + height > (Scr.MyDisplayHeight - 2 + base_y))
	  real_y = Scr.MyDisplayHeight - height - 2 + base_y;
	if (test_x < base_x)
	  real_x = base_x;
	if (test_y < base_y)
	  real_y = base_y;
	loc_ok = True;
	test_window = Scr.ScwmRoot.next;
	while ((test_window != (ScwmWindow *) 0) && (loc_ok == True)) {
	  if (test_window->Desk == t->Desk) {
	    if (test_window->fIconified &&
		(test_window->icon_w || test_window->icon_pixmap_w) &&
		(test_window != t)) {
	      tw = test_window->icon_p_width;
	      th = test_window->icon_p_height + test_window->icon_w_height;
	      tx = test_window->icon_x_loc;
	      ty = test_window->icon_y_loc;

	      if ((tx < (real_x + width + 3)) && ((tx + tw + 3) > real_x) &&
		  (ty < (real_y + height + 3)) && ((ty + th + 3) > real_y)) {
		loc_ok = False;
	      }
	    }
	  }
	  test_window = test_window->next;
	}
	test_x += 3;
      }
      test_y += 3;
    }
    if (loc_ok == False)
      return;
    t->icon_x_loc = real_x;
    t->icon_y_loc = real_y;

    if (t->icon_pixmap_w)
      XMoveWindow(dpy, t->icon_pixmap_w, t->icon_x_loc, t->icon_y_loc);

    t->icon_w_width = t->icon_p_width;
    t->icon_xl_loc = t->icon_x_loc;

    if (t->icon_w != None)
      XMoveResizeWindow(dpy, t->icon_w, t->icon_xl_loc,
			t->icon_y_loc + t->icon_p_height,
			t->icon_w_width, ICON_HEIGHT);
    Broadcast(M_ICON_LOCATION, 7, t->w, t->frame,
	      (unsigned long) t,
	      t->icon_x_loc, t->icon_y_loc,
	      t->icon_w_width, t->icon_w_height + t->icon_p_height);
  }
}

/***********************************************************************
 *
 *  Procedure:
 *	DeIconify a window
 *
 ***********************************************************************/
void 
DeIconify(ScwmWindow * tmp_win)
{
  ScwmWindow *t, *tmp;

  if (!tmp_win)
    return;

  RaiseWindow(tmp_win);
  /* now de-iconify transients */
  for (t = Scr.ScwmRoot.next; t != NULL; t = t->next) {
    if ((t == tmp_win) ||
	(t->fTransient && (t->transientfor == tmp_win->w))) {
      t->fMapped = True;
      if (Scr.Hilite == t)
	SetBorder(t, False, True, True, None);

      XMapWindow(dpy, t->w);
      if (t->Desk == Scr.CurrentDesk) {
	XMapWindow(dpy, t->frame);
	t->fMapPending = True;
      }
      XMapWindow(dpy, t->Parent);
      SetMapStateProp(t, NormalState);
      t->fIconified = False;
      t->fIconUnmapped = False;
      /* Need to make sure the border is colored correctly,
       * in case it was stuck or unstuck while iconified. */
      tmp = Scr.Hilite;
      Scr.Hilite = t;
      SetBorder(t, False, True, True, None);
      Scr.Hilite = tmp;
      XRaiseWindow(dpy, t->w);
      if (t->icon_w)
	XUnmapWindow(dpy, t->icon_w);
      if (t->icon_pixmap_w)
	XUnmapWindow(dpy, t->icon_pixmap_w);
      Broadcast(M_DEICONIFY, 3, t->w, t->frame, (unsigned long) t, 0, 0, 0, 0);
    }
  }

  if (tmp_win->fClickToFocus)
    FocusOn(tmp_win, 1);

  KeepOnTop();

  return;
}


/****************************************************************************
 *
 * Iconifies the selected window
 *
 ****************************************************************************/
void 
Iconify(ScwmWindow * tmp_win, int def_x, int def_y)
{
  ScwmWindow *t;
  XWindowAttributes winattrs;
  unsigned long eventMask;

  if (!tmp_win)
    return;
  XGetWindowAttributes(dpy, tmp_win->w, &winattrs);
  eventMask = winattrs.your_event_mask;

  if ((tmp_win == Scr.Hilite) &&
      tmp_win->fClickToFocus && tmp_win->next) {
    SetFocus(tmp_win->next->w, tmp_win->next, 1);
  }

  /* iconify transients first */
  for (t = Scr.ScwmRoot.next; t != NULL; t = t->next) {
    if ((t == tmp_win) ||
	(t->fTransient && (t->transientfor == tmp_win->w))) {
      /*
       * Prevent the receipt of an UnmapNotify, since that would
       * cause a transition to the Withdrawn state.
       */
      t->fMapped = False;
      XSelectInput(dpy, t->w, eventMask & ~StructureNotifyMask);
      XUnmapWindow(dpy, t->w);
      XSelectInput(dpy, t->w, eventMask);
      XUnmapWindow(dpy, t->frame);
      t->DeIconifyDesk = t->Desk;
      if (t->icon_w)
	XUnmapWindow(dpy, t->icon_w);
      if (t->icon_pixmap_w)
	XUnmapWindow(dpy, t->icon_pixmap_w);

      SetMapStateProp(t, IconicState);
      SetBorder(t, False, False, False, None);
      if (t != tmp_win) {
	t->fIconified = True;
	t->fIconUnmapped = True;

	Broadcast(M_ICONIFY, 7, t->w, t->frame,
		  (unsigned long) t,
		  -10000, -10000,
		  t->icon_w_width,
		  t->icon_w_height + t->icon_p_height);
	BroadcastConfig(M_CONFIGURE_WINDOW, t);
      }
    }
  }
  if (tmp_win->icon_w == None)
    if (tmp_win->fIconMoved)
      CreateIconWindow(tmp_win, tmp_win->icon_x_loc, tmp_win->icon_y_loc);
    else
      CreateIconWindow(tmp_win, def_x, def_y);

  /* if no pixmap we want icon width to change to text width every iconify */
  if ((tmp_win->icon_w != None) && (tmp_win->icon_pixmap_w == None)) {
    tmp_win->icon_t_width =
      XTextWidth(XFONT(Scr.icon_font), tmp_win->icon_name,
		 strlen(tmp_win->icon_name));
    tmp_win->icon_w_width = tmp_win->icon_t_width + 6;
  }
  AutoPlace(tmp_win);
  tmp_win->fIconified = True;
  tmp_win->fIconUnmapped = False;
  Broadcast(M_ICONIFY, 7, tmp_win->w, tmp_win->frame,
	    (unsigned long) tmp_win,
	    tmp_win->icon_x_loc, tmp_win->icon_y_loc,
	    tmp_win->icon_w_width,
	    tmp_win->icon_w_height + tmp_win->icon_p_height);
  BroadcastConfig(M_CONFIGURE_WINDOW, tmp_win);

  LowerWindow(tmp_win);
  if (tmp_win->Desk == Scr.CurrentDesk) {
    if (tmp_win->icon_w != None)
      XMapWindow(dpy, tmp_win->icon_w);

    if (tmp_win->icon_pixmap_w != None)
      XMapWindow(dpy, tmp_win->icon_pixmap_w);
    KeepOnTop();
  }
  if (tmp_win->fClickToFocus || tmp_win->fSloppyFocus) {
    if (tmp_win == Scr.Focus) {
      if (Scr.PreviousFocus == Scr.Focus)
	Scr.PreviousFocus = NULL;
      if (tmp_win->fClickToFocus && (tmp_win->next))
	SetFocus(tmp_win->next->w, tmp_win->next, 1);
      else {
	SetFocus(Scr.NoFocusWin, NULL, 1);
      }
    }
  }
  return;
}


void redraw_icon_titles()
{
  ScwmWindow *tmp;

  tmp = Scr.ScwmRoot.next;
  while (tmp != NULL) {
    RedoIconName(tmp);
    if (tmp->fIconified) {
      DrawIconWindow(tmp);
    }
    tmp = tmp->next;
  }
}

/****************************************************************************
 *
 * This is used to tell applications which windows on the screen are
 * top level appication windows, and which windows are the icon windows
 * that go with them.
 *
 ****************************************************************************/
void 
SetMapStateProp(ScwmWindow * tmp_win, int state)
{
  unsigned long data[2];	/* "suggested" by ICCCM version 1 */

  data[0] = (unsigned long) state;
  data[1] = (unsigned long) tmp_win->icon_w;
/*  data[2] = (unsigned long) tmp_win->icon_pixmap_w; */

  XChangeProperty(dpy, tmp_win->w, _XA_WM_STATE, _XA_WM_STATE, 32,
		  PropModeReplace, (unsigned char *) data, 2);
  return;
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
