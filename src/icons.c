/* $Id$
 * icons.c 
 * Copyright (C) 1998-1999 Maciej Stachowiak and Greg J. Badros
 */

/****************************************************************************
 * This module is derived from mostly all code written by Rob Nation 
 * A little of it is borrowed from ctwm.
 * Copyright 1993 Robert Nation. No restrictions are placed on this code,
 * as long as the copyright notice is preserved
 ****************************************************************************/

/*
 * scwm icon code
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>

#ifdef NeXT
#include <fcntl.h>
#endif

#include <X11/Intrinsic.h>
#ifdef HAVE_SHAPE
#include <X11/extensions/shape.h>
#endif

#include "icons.h"

#include "scwm.h"
#include "screen.h"
#include "borders.h"
#include "module-interface.h"
#include "binding.h"
#include "font.h"
#include "color.h"
#include "focus.h"
#include "xmisc.h"
#include "callbacks.h"
#include "cursor.h"

SCWM_HOOK(iconify_hook, "iconify-hook", 2,
"This hook is invoked when a window is iconified.\n\
It is called with two arguments: WINDOW, WAS-ICONIFIED?.\n\
WINDOW is the window iconfied, and WAS-ICONIFIED? is\n\
a boolean telling whether the window was iconified previously.");

SCWM_HOOK(deiconify_hook, "deiconify-hook", 2,
"This hook is invoked when a window is deiconified.\n\
It is called with two arguments: WINDOW, WAS-ICONIFIED?.\n\
WINDOW is the window iconfied, and WAS-ICONIFIED? is\n\
a boolean telling whether the window was iconified previously.");


/*
 * GrabIconButtons - grab needed buttons for the icon window
 *
 * psw - the scwm window structure to use 
 * GJB:FIXME:: psw should be used -- the buttons needed to be
 * grabbed might vary based on what kind of window it is.
 */
static void 
GrabIconButtons(ScwmWindow *ARG_UNUSED(psw), Window w)
{
  Binding *pbnd;

  for (pbnd = Scr.AllBindings; pbnd; pbnd = pbnd->NextBinding) {
    if ((pbnd->Context & C_ICON) && 
        pbnd->IsMouse ) {
      GrabButtonWithModifiersMaskXcPm(pbnd->Button_Key,pbnd->Modifier,
                                      w,ButtonPressMask|ButtonReleaseMask,
                                      XCURSOR_ICON,GrabModeAsync);
    }
  }
  return;
}



/*
 * GrabIconKeys - grab needed keys for the icon window
 *
 * psw - the scwm window structure to use
 *
 * GJB:FIXME:: psw should be used -- the buttons needed to be
 * grabbed might vary based on what kind of window it is.
 */
static void 
GrabIconKeys(ScwmWindow *ARG_UNUSED(psw), Window w)
{
  Binding *pbnd;

  for (pbnd = Scr.AllBindings; pbnd; pbnd = pbnd->NextBinding) {
    if ((pbnd->Context & C_ICON) && 
        !pbnd->IsMouse) {
      GrabKeyWithModifiersWin(pbnd->Button_Key,pbnd->Modifier,w);
    }
  }
  return;
}

/*
 * Looks for an application supplied icon window
 */
static void 
GetIconWindow(ScwmWindow * psw)
{
  Pixmap picture;
  Pixmap mask = None;
  unsigned int width;
  unsigned int height;
  unsigned int border_width;
  unsigned int depth;

  /* We are guaranteed that wmhints is non-null when calling this routine */
  if (XGetGeometry(dpy, psw->wmhints->icon_window, &JunkRoot,
		   &JunkX, &JunkY, 
		   &width, &height,
		   &border_width, &depth) == 0) {
    scwm_msg(ERR, "GetIconWindow", "Help! Bad Icon Window!");
    return;
  }
  picture = psw->wmhints->icon_pixmap;
  width += border_width * 2;
  height += border_width * 2;

  /*
   * Now make the new window the icon window for this window,
   * and set it up to work as such (select for key presses
   * and button presses/releases, set up the contexts for it,
   * and define the cursor for it).
   */
  psw->icon_pixmap_w = psw->wmhints->icon_window;

  if (ShapesSupported) {
    if (psw->wmhints->flags & IconMaskHint) {
      psw->fShapedIcon = True;
      mask = psw->wmhints->icon_mask;
    }
  }
  /* Make sure that the window is a child of the root window ! */
  /* Olwais screws this up, maybe others do too! */
  XReparentWindow(dpy, psw->icon_pixmap_w, Scr.Root, 0, 0);
  psw->fIconOurs = False;

  /* and finally add this picture to the ScwmWindow */
  psw->icon_image = make_image_from_pixmap("FromApp",picture,mask,width,height,depth);
  IMAGE(psw->icon_image)->foreign=1;
}


/*
 * Looks for an application supplied bitmap or pixmap
 */
static void 
GetIconBitmap(ScwmWindow *psw)
{
#define FUNC_NAME "GetIconBitmap"
  Pixmap picture;
  Pixmap mask = None;
  unsigned int width;
  unsigned int height;
  unsigned int depth;

  /* We are guaranteed that wmhints is non-null when calling this
   * routine */
  if (XGetGeometry(dpy, psw->wmhints->icon_pixmap, &JunkRoot, &JunkX, &JunkY,
		   &width, &height,
		   &JunkBW, &depth) == 0) {
    scwm_msg(ERR, FUNC_NAME, "Help! Bad Icon bitmap!");
    return;
  }
  picture = psw->wmhints->icon_pixmap;
  if (ShapesSupported) {
    if (psw->wmhints->flags & IconMaskHint) {
      psw->fShapedIcon = True;
      mask = psw->wmhints->icon_mask;
    }
  }

  /* and finally add this picture to the ScwmWindow */
  psw->icon_image = make_image_from_pixmap("FromAppBitmap",
					  picture,mask,
					  width,height,depth);
  IMAGE(psw->icon_image)->foreign=1;
}
#undef FUNC_NAME

/*
 * Public functions below here
 *************************************************************************/


/*
 * Creates an icon window as needed
 * def_x, def_y are viewport positions (I think --09/17/98 gjb)
 */
void 
CreateIconWindow(ScwmWindow * psw, int def_x, int def_y)
{
  int final_x, final_y;
  unsigned long valuemask;	/* mask for create windows */
  XSetWindowAttributes attributes;	/* attributes for create windows */

  psw->fIconOurs = True;
  psw->fPixmapOurs = False;
  psw->fShapedIcon = False;
  psw->icon_pixmap_w = None;

  if (psw->fSuppressIcon)
     return;


  /* If the icon is forced, use the requested icon no matter what. */

  if (psw->fForceIcon) {
    psw->icon_image=psw->icon_req_image;
  } else if ((psw->wmhints) && (psw->wmhints->flags & IconWindowHint)) {
    /* Next, See if the app supplies its own icon window */
    GetIconWindow(psw);
  } else if ((psw->wmhints) && (psw->wmhints->flags & IconPixmapHint)) {
    /* Next, try to get icon bitmap from the application */
    GetIconBitmap(psw);
  } else {
    /* If all else fails, use the requested icon anyway. */
    psw->icon_image=psw->icon_req_image;
  }

  if (ShapesSupported && psw->icon_image != SCM_BOOL_F && 
      IMAGE(psw->icon_image)->mask!=None) {
    psw->fShapedIcon = True;
  }

  
  /* MS:FIXME:: You should be able to set separately whether you want icon
     titles or icon images or both. */
  /* CRW:FIXME:MS: Has the above already been implemented?  If so,
     the comment should be deleted... */

  /* figure out the icon window size */
  if (!psw->fNoIconTitle || psw->icon_p_height == 0) {
    psw->icon_t_width = ComputeXTextWidth(XFONT(Scr.icon_font),
                                          psw->icon_name,-1);
    psw->icon_w_height = ICON_HEIGHT;
  } else {
    psw->icon_t_width = 0;
    psw->icon_w_height = 0;
  }

  if (psw->icon_image != SCM_BOOL_F) {
    psw->icon_p_height = IMAGE(psw->icon_image)->height + 
      (psw->fIconOurs ? 4 : 0);
    psw->icon_p_width = IMAGE(psw->icon_image)->width + 
      (psw->fIconOurs ? 4 : 0);  
  } else {
    psw->icon_p_height = 0;
    psw->icon_p_width = psw->icon_t_width + 6;
  }

  psw->icon_w_width = psw->icon_p_width;


  /* Not having an icon picture should not throw an error,
     it is a valid state! All it means is that we don't want an icon
     picture at all, just the icon title. - MS 11/19/97 */

  final_x = def_x;
  final_y = def_y;
  if (final_x < ICON_VP_OFFSET_X(psw))
    final_x = ICON_VP_OFFSET_X(psw);
  if (final_y < ICON_VP_OFFSET_Y(psw))
    final_y = ICON_VP_OFFSET_Y(psw);

  if (final_x + psw->icon_w_width >= ICON_VP_OFFSET_X(psw) + Scr.DisplayWidth)
    final_x = ICON_VP_OFFSET_X(psw) + Scr.DisplayWidth - psw->icon_w_width - 1;
  if (final_y + psw->icon_w_height >= ICON_VP_OFFSET_Y(psw) + Scr.DisplayHeight)
    final_y = ICON_VP_OFFSET_Y(psw) + Scr.DisplayHeight - psw->icon_w_height - 1;

  psw->icon_x_loc = final_x;
  psw->icon_xl_loc = final_x;
  psw->icon_y_loc = final_y;

  /* clip to fit on screen */
  attributes.background_pixel = XCOLOR(Scr.NotMenuColors.bg);
  valuemask = CWBorderPixel | CWCursor | CWEventMask | CWBackPixel;
  attributes.border_pixel = XCOLOR(Scr.NotMenuColors.fg);
  psw->icon_cursor=get_scm_cursor_by_number(XCURSOR_ICON);
  attributes.cursor = XCURSOR(psw->icon_cursor);
  attributes.event_mask = (ButtonPressMask | ButtonReleaseMask |
			   VisibilityChangeMask |
			   ExposureMask | KeyPressMask | EnterWindowMask |
			   FocusChangeMask);

  if (!psw->fNoIconTitle || (psw->icon_p_height == 0))
    psw->icon_w =
      XCreateWindow(dpy, Scr.Root, final_x, final_y + psw->icon_p_height,
		    psw->icon_w_width, psw->icon_w_height, 0,
		    CopyFromParent,
		    CopyFromParent, CopyFromParent, valuemask, &attributes);

  /* psw->icon_p_width should always be > 0 here - MS 2-19-98 */
  if ((psw->fIconOurs) /* && psw->icon_p_width > 0 */ 
      && psw->icon_p_height > 0) {
    psw->icon_pixmap_w =
      XCreateWindow(dpy, Scr.Root, final_x, final_y, psw->icon_p_width,
		    psw->icon_p_height, 0, CopyFromParent,
		    CopyFromParent, CopyFromParent, valuemask, &attributes);
  } else if (None != psw->icon_pixmap_w) {
    attributes.event_mask = (ButtonPressMask | ButtonReleaseMask |
			     VisibilityChangeMask |
			     KeyPressMask | EnterWindowMask |
			     FocusChangeMask | LeaveWindowMask);
    
    valuemask = CWEventMask;
    XChangeWindowAttributes(dpy, psw->icon_pixmap_w,
			    valuemask, &attributes);
  }


  if (ShapesSupported && psw->fShapedIcon &&
    psw->icon_image != SCM_BOOL_F) {
    XShapeCombineMask(dpy, psw->icon_pixmap_w, ShapeBounding, 2, 2,
		      IMAGE(psw->icon_image)->mask, ShapeSet);
  }

  if (psw->icon_w != None) {
    ScwmSaveContextPsw(dpy,psw->icon_w,psw);
    XDefineCursor(dpy, psw->icon_w, XCURSOR(psw->icon_cursor));
    GrabIconButtons(psw, psw->icon_w);
    GrabIconKeys(psw, psw->icon_w);
  }
  if (psw->icon_pixmap_w != None) {
    ScwmSaveContextPsw(dpy,psw->icon_pixmap_w,psw);
    XDefineCursor(dpy, psw->icon_pixmap_w, XCURSOR(psw->icon_cursor));
    GrabIconButtons(psw, psw->icon_pixmap_w);
    GrabIconKeys(psw, psw->icon_pixmap_w);
  }

  return;
}

/*
 * Draws the icon window
 */
void 
DrawIconWindow(ScwmWindow * psw)
{
  GC Shadow, Relief;
  Pixel TextColor, BackColor;
  int x;

  if (psw->fSuppressIcon)
    return;

  if (psw->icon_w != None)
    flush_expose(psw->icon_w);
  if (psw->icon_pixmap_w != None)
    flush_expose(psw->icon_pixmap_w);

  if (Scr.Hilite == psw) {
    /* MS:FIXME:: This can't poossibly be right. */
    if (Scr.d_depth < 2) {
      Relief =
	Shadow = Scr.DefaultDecor.HiShadowGC;
      TextColor = XCOLOR(Scr.DefaultDecor.HiColors.fg);
      BackColor = XCOLOR(Scr.DefaultDecor.HiColors.bg);
    } else {
      Relief = GET_DECOR(psw, HiReliefGC);
      Shadow = GET_DECOR(psw, HiShadowGC);
      TextColor = XCOLOR(GET_DECOR(psw, HiColors.fg));
      BackColor = XCOLOR(GET_DECOR(psw, HiColors.bg));
    }
    /* resize the icon name window */
    if (psw->icon_w != None) {
      psw->icon_w_width = psw->icon_t_width + 6;
      if (psw->icon_w_width < psw->icon_p_width)
	psw->icon_w_width = psw->icon_p_width;
      psw->icon_xl_loc = psw->icon_x_loc -
	(psw->icon_w_width - psw->icon_p_width) / 2;
    }
  } else {
    if (Scr.d_depth < 2) {
      /* FIXJTL: Is this what was intended before? Does it make any
         sense? */
      SetGCFg(Relief = Scr.ScratchGC1,XCOLOR(Scr.NotMenuColors.fg));
      SetGCBg(Relief,XCOLOR(Scr.NotMenuColors.bg));
      Shadow = Relief;
    } else {
      /* now set the fg colors for the two GCs we'll use to draw with */
      SetGCFg(Relief = Scr.ScratchGC1,XCOLOR(psw->ReliefColor));
      SetGCFg(Shadow = Scr.ScratchGC2,XCOLOR(psw->ShadowColor));
    }
    /* resize the icon name window */
    if (psw->icon_w != None) {
      psw->icon_w_width = psw->icon_p_width;
      psw->icon_xl_loc = psw->icon_x_loc;
    }
    TextColor = XCOLOR(psw->TextColor);
    BackColor = XCOLOR(psw->BackColor);

  }
  if (psw->fIconOurs && (psw->icon_pixmap_w != None)) {
    XSetWindowBackground(dpy, psw->icon_pixmap_w,
			 BackColor);
  }
  if (psw->icon_w != None)
    XSetWindowBackground(dpy, psw->icon_w, BackColor);

  /* write the icon label */

  NewFontAndColor(Scr.ScratchGC3,XFONTID(Scr.icon_font), TextColor, BackColor);


  if (psw->icon_w != None) {
    psw->icon_w_height = ICON_HEIGHT;
    XClearWindow(dpy, psw->icon_w);
  }

  MovePswIconToCurrentPosition(psw);

  if (psw->fIconOurs) {
    if ((psw->icon_image != SCM_BOOL_F) &&
	!psw->fShapedIcon) {
      RelieveWindow(psw, psw->icon_pixmap_w, 0, 0,
		    psw->icon_p_width, psw->icon_p_height,
		    Relief, Shadow, FULL_HILITE);
    }

  /* need to locate the icon pixmap */
    if (psw->icon_image != SCM_BOOL_F) {
      if (IMAGE(psw->icon_image)->depth == Scr.d_depth) {
	XCopyArea(dpy, IMAGE(psw->icon_image)->image, 
		  psw->icon_pixmap_w, Scr.ScratchGC3,
		  0, 0, psw->icon_p_width-4,
		  psw->icon_p_height-4, 2, 2);
      } else {
	XCopyPlane(dpy, IMAGE(psw->icon_image)->image, 
		   psw->icon_pixmap_w, Scr.ScratchGC3, 0,
		   0, psw->icon_p_width-4, psw->icon_p_height-4,
		   2, 2, 1);
      }
    }
  }

  if (psw->icon_w != None) {
    /* text position */
    x = (psw->icon_w_width - psw->icon_t_width) / 2;
    if (x < 3)
      x = 3;

#ifdef I18N
    XmbDrawString(dpy, psw->icon_w, XFONT(Scr.icon_font),
		  Scr.ScratchGC3, x,
		  psw->icon_w_height - FONTHEIGHT(Scr.icon_font) +
		  FONTY(Scr.icon_font) - 3,
		  psw->icon_name, strlen(psw->icon_name));
#else
    XDrawString(dpy, psw->icon_w, Scr.ScratchGC3, x,
		psw->icon_w_height - FONTHEIGHT(Scr.icon_font) +
		FONTY(Scr.icon_font) - 3,
		psw->icon_name, strlen(psw->icon_name));
#endif
    RelieveWindow(psw, psw->icon_w, 0, 0, psw->icon_w_width,
		  ICON_HEIGHT, Relief, Shadow, FULL_HILITE);
  }
}

/*
 * RedoIconName - procedure to re-position the icon window and name
 */
void 
RedoIconName(ScwmWindow *psw)
{
  if (psw->fSuppressIcon)
    return;

  if (psw->icon_w == None)
    return;

  psw->icon_t_width = ComputeXTextWidth(XFONT(Scr.icon_font), psw->icon_name,
                                        strlen(psw->icon_name));
  /* clear the icon window, and trigger a re-draw via an expose event */
  if (psw->fIconified) {
    XClearArea(dpy, psw->icon_w, 0, 0, 0, 0, True);
  }

  return;
}


/*
 * AutoPlace - Find a home for an icon
 */
void 
AutoPlace(ScwmWindow *psw)
{
  int test_x = 0, test_y = 0, tw, th, tx, ty, temp_h, temp_w;
  int base_x, base_y;
  int width = 0;
  int height = 0;
  ScwmWindow *test_window;
  Bool loc_ok;
  int real_x = 10, real_y = 10;

  base_x = ((FRAME_X_VP(psw) + ICON_VP_OFFSET_X(psw) + (FRAME_WIDTH(psw)/2))
	    / Scr.DisplayWidth) * Scr.DisplayWidth - ICON_VP_OFFSET_X(psw);
  base_y = ((FRAME_Y_VP(psw) + ICON_VP_OFFSET_Y(psw) + (FRAME_HEIGHT(psw)/2))
	    / Scr.DisplayHeight) * Scr.DisplayHeight - ICON_VP_OFFSET_Y(psw);

  if (psw->fIconMoved) {
    if (psw->fStickyIcon) {
      /* GJB:FIXME:: may not want this -- icons should be able to iconify
         to anywhere */
      /* just make sure the icon is on this screen */
      psw->icon_x_loc = ICON_X_VIRT(psw) % Scr.DisplayWidth + base_x;
      psw->icon_y_loc = ICON_Y_VIRT(psw) % Scr.DisplayHeight + base_y;
    }
  } else if (psw->wmhints && psw->wmhints->flags & IconPositionHint) {
    psw->icon_x_loc = psw->wmhints->icon_x;
    psw->icon_y_loc = psw->wmhints->icon_y;
  } else if (psw->IconBox[0] >= 0) {
    width = psw->icon_p_width;
    height = psw->icon_p_height + psw->icon_w_height;
    loc_ok = False;

    /* check all boxes in order */
    /* In each IconBox, start at the upper left, travel right, then
     * down */
    test_y = psw->IconBox[1] + base_y;

    temp_h = height;
    temp_w = width;

    /* OK second try at this.
     * If the window is taller than the icon box, ignore the icon height
     * when figuring where to put it. Same goes for the width */
    /* This should permit reasonably graceful handling of big icons. */
    if (width >= (psw->IconBox[2] - psw->IconBox[0]))
      temp_w = 0;
    if (height >= (psw->IconBox[3] - psw->IconBox[1]))
      temp_h = 0;

    while (((test_y + temp_h) < (psw->IconBox[3] + base_y)) && (!loc_ok)) {
      test_x = psw->IconBox[0] + base_x;
      while (((test_x + temp_w) < (psw->IconBox[2] + base_x)) &&
	     (!loc_ok)) {
	real_x = test_x;
	real_y = test_y;

	if (test_x + width > (Scr.DisplayWidth - 2 + base_x))
	  real_x = Scr.DisplayWidth - width - 2 + base_x;
	if (test_y + height > (Scr.DisplayHeight - 2 + base_y))
	  real_y = Scr.DisplayHeight - height - 2 + base_y;
	if (test_x < base_x)
	  real_x = base_x;
	if (test_y < base_y)
	  real_y = base_y;
	loc_ok = True;
	test_window = Scr.ScwmRoot.next;
	while ((test_window != NULL) && (loc_ok == True)) {
	  if (test_window->Desk == psw->Desk) {
	    if (test_window->fIconified &&
		(test_window->icon_w || test_window->icon_pixmap_w) &&
		(test_window != psw)) {
	      tw = test_window->icon_p_width;
	      th = test_window->icon_p_height + test_window->icon_w_height;
	      tx = ICON_X_VP(test_window);
	      ty = ICON_Y_VP(test_window);

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
    psw->icon_x_loc = real_x + ICON_VP_OFFSET_X(psw);
    psw->icon_y_loc = real_y + ICON_VP_OFFSET_Y(psw);

    psw->icon_w_width = psw->icon_p_width;
    psw->icon_xl_loc = psw->icon_x_loc;

    MovePswIconToCurrentPosition(psw);
  }
}

/*
 * DeIconify a window
 */
void 
DeIconify(ScwmWindow *psw)
{
  ScwmWindow *t = NULL;
  ScwmWindow *pswTmpHilite = NULL;

  if (!psw)
    return;

  /* GJB:FIXME:: maybe comment this line out? */
  MovePswToCurrentPosition(psw);
  RaiseWindow(psw);

  /* now de-iconify the window and its transients */
  for (t = Scr.ScwmRoot.next; t != NULL; t = t->next) {
    if ((t == psw) ||
	(t->fTransient && (t->transientfor == psw->w))) {
      SCM was_iconified_p = SCM_BOOL_FromBool(t->fIconified);
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
      pswTmpHilite = Scr.Hilite;
      Scr.Hilite = t;
      SetBorder(t, False, True, True, None);
      Scr.Hilite = pswTmpHilite;
      XRaiseWindow(dpy, t->w);
      if (t->icon_w)
	XUnmapWindow(dpy, t->icon_w);
      if (t->icon_pixmap_w)
	XUnmapWindow(dpy, t->icon_pixmap_w);
      Broadcast(M_DEICONIFY, 3, t->w, t->frame, (unsigned long) t, 0, 0, 0, 0);
      call2_hooks(deiconify_hook, SCM_FROM_PSW(t), was_iconified_p);
    }
  }

  if (psw->fClickToFocus)
    FocusOn(psw);

  KeepOnTop();

  return;
}


/*
 * Iconifies the selected window
 */
void 
Iconify(ScwmWindow *psw, int def_x, int def_y)
{
  ScwmWindow *t;
  XWindowAttributes winattrs;
  unsigned long eventMask;

  if (!psw)
    return;
  XGetWindowAttributes(dpy, psw->w, &winattrs);
  eventMask = winattrs.your_event_mask;

  if ((psw == Scr.Hilite) &&
      psw->fClickToFocus && psw->next) {
    SetFocus(psw->next->w, psw->next, True);
  }

  /* iconify transients first */
  for (t = Scr.ScwmRoot.next; t != NULL; t = t->next) {
    if ((t == psw) ||
	(t->fTransient && (t->transientfor == psw->w))) {
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
      if (t != psw) {
        SCM was_iconified_p = SCM_BOOL_FromBool(psw->fIconified);
	t->fIconified = True;
	t->fIconUnmapped = True;

        BroadcastIconInfo(M_ICONIFY,t);
	BroadcastConfig(M_CONFIGURE_WINDOW, t);
        call2_hooks(iconify_hook, SCM_FROM_PSW(psw), was_iconified_p);
      }
    }
  }
  if (psw->icon_w == None) {
    if (psw->fIconMoved)
      CreateIconWindow(psw, ICON_X_VP(psw), ICON_Y_VP(psw));
    else
      CreateIconWindow(psw, def_x, def_y);
  }

  /* if no pixmap we want icon width to change to text width every iconify */
  if ((psw->icon_w != None) && (psw->icon_pixmap_w == None)) {
    psw->icon_t_width =
      ComputeXTextWidth(XFONT(Scr.icon_font), psw->icon_name,
                        strlen(psw->icon_name));
    psw->icon_w_width = psw->icon_t_width + 6;
  }
  { /* scope */
    SCM was_iconified_p = SCM_BOOL_FromBool(psw->fIconified);
    AutoPlace(psw);
    psw->fIconified = True;
    psw->fIconUnmapped = False;
    BroadcastIconInfo(M_ICONIFY, psw);
    BroadcastConfig(M_CONFIGURE_WINDOW, psw);
    call2_hooks(iconify_hook, SCM_FROM_PSW(psw), was_iconified_p);
  }

  LowerWindow(psw);

  if (psw->Desk == Scr.CurrentDesk) {
    if (psw->icon_w != None)
      XMapWindow(dpy, psw->icon_w);

    if (psw->icon_pixmap_w != None)
      XMapWindow(dpy, psw->icon_pixmap_w);
    KeepOnTop();
  }

  if (psw->fClickToFocus || psw->fSloppyFocus) {
    if (psw == Scr.Focus) {
      if (Scr.PreviousFocus == Scr.Focus)
	Scr.PreviousFocus = NULL;
      if (psw->fClickToFocus && (psw->next))
	SetFocus(psw->next->w, psw->next, True);
      else {
	SetFocus(Scr.NoFocusWin, NULL, True);
      }
    }
  }
  return;
}


void redraw_icon_titles()
{
  ScwmWindow *psw = Scr.ScwmRoot.next;

  while (psw != NULL) {
    RedoIconName(psw);
    if (psw->fIconified) {
      DrawIconWindow(psw);
    }
    psw = psw->next;
  }
}

/*
 * This is used to tell applications which windows on the screen are
 * top level appication windows, and which windows are the icon windows
 * that go with them.
 */
void 
SetMapStateProp(ScwmWindow *psw, int state)
{
  unsigned long data[2];	/* "suggested" by ICCCM version 1 */

  data[0] = (unsigned long) state;
  data[1] = (unsigned long) psw->icon_w;
/*  data[2] = (unsigned long) psw->icon_pixmap_w; */

  XChangeProperty(dpy, psw->w, XA_WM_STATE, XA_WM_STATE, 32,
		  PropModeReplace, (unsigned char *) data, 2);
  return;
}

void
init_icons()
{
#ifndef SCM_MAGIC_SNARFER
#include "icons.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

