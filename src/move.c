


/*
 * This module is all original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/

/***********************************************************************
 *
 * code for moving windows
 *
 ***********************************************************************/

#include <config.h>

#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <X11/keysym.h>
#include "scwm.h"
#include "menus.h"
#include "misc.h"
#include "move.h"
#include "screen.h"
#include "Grab.h"
#include "icons.h"


extern XEvent Event;
extern int menuFromFrameOrWindowOrTitlebar;
Bool NeedToResizeToo;

float rgpctMovementDefault[32] = {
    -.01, 0, .01, .03,.08,.18,.3,.45,.60,.75,.85,.90,.94,.97,.99,1.0 
    /* must end in 1.0 */
  };

int cmsDelayDefault = 10; /* milliseconds */

/* Perform the movement of the window. ppctMovement *must* have a 1.0 entry
   somewhere in ins list of floats, and movement will stop when it hits a 1.0 entry */
void 
AnimatedMoveWindow(Window w,int startX,int startY,int endX, int endY,
		   Bool fWarpPointerToo, int cmsDelay, float *ppctMovement )
{
  int pointerX, pointerY;
  int currentX, currentY;
  int lastX, lastY;
  int deltaX, deltaY;

  /* set our defaults */
  if (ppctMovement == NULL) ppctMovement = rgpctMovementDefault;
  if (cmsDelay < 0)         cmsDelay     = cmsDelayDefault;

  if (startX < 0 || startY < 0) 
    {
    XGetGeometry(dpy, w, &JunkRoot, &currentX, &currentY, 
		 &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth);
    if (startX < 0) startX = currentX;
    if (startY < 0) startY = currentY;
    }

  deltaX = endX - startX;
  deltaY = endY - startY;
  lastX = startX;
  lastY = startY;

  do {
    currentX = startX + deltaX * (*ppctMovement);
    currentY = startY + deltaY * (*ppctMovement);
    XMoveWindow(dpy,w,currentX,currentY);
    if (fWarpPointerToo) {
      XQueryPointer(dpy, Scr.Root, &JunkRoot, &JunkChild,
		    &JunkX,&JunkY,&pointerX,&pointerY,&JunkMask);
      pointerX += currentX - lastX;
      pointerY += currentY - lastY;
      XWarpPointer(dpy,None,Scr.Root,0,0,0,0,
		   pointerX,pointerY);
    }
    XFlush(dpy);
    sleep_ms(cmsDelay);
#ifdef FIXGJB_ALLOW_ABORTING_ANIMATED_MOVES
    /* this didn't work for me -- maybe no longer necessary since
       we warn the user when they use > .5 seconds as a between-frame delay
       time */
    if (XCheckMaskEvent(dpy, 
			ButtonPressMask|ButtonReleaseMask|
			KeyPressMask,
			&Event)) {
      /* finish the move immediately */
      XMoveWindow(dpy,w,endX,endY);
      XFlush(dpy);
      return;
    }
#endif
    lastX = currentX;
    lastY = currentY;
    }
  while (*ppctMovement != 1.0 && ppctMovement++);
 
}


/* AnimatedShadeWindow handles animating of window shades
   note that the first argument to this is a ScwmWindow *, since
   the frame needs to be manipulated; the last two args are like
   AnimatedMoveWindow, above --11/09/97 gjb */
void 
AnimatedShadeWindow(ScwmWindow *sw, Bool fRollUp, 
		    int cmsDelay, float *ppctMovement)
{
  Window w = sw->w;
  Window wFrame = sw->frame;
  int width = sw->frame_width;
  int shaded_height = sw->title_height + sw->boundary_width;
  /* FIXGJB: using orig_ht doesn't seem right -- does it interact
     correctly w/ maximization? */
/*  Bool shaded = sw->buttons & WSHADE; FIXGJB: unused for now
    int normal_height = shaded? sw->orig_ht : sw->frame_height; */
  int normal_height = sw->orig_ht;
  int client_height = normal_height - shaded_height - sw->boundary_width;
  int deltaY = normal_height - shaded_height;
  /* set our defaults */
  if (ppctMovement == NULL) ppctMovement = rgpctMovementDefault;
  if (cmsDelay < 0)         cmsDelay     = cmsDelayDefault;
  
  if (fRollUp) {
    XLowerWindow(dpy,w);
    do {
      /* while (height > end_height)  */
      XMoveWindow(dpy, w, 0, -deltaY * (*ppctMovement));
      XResizeWindow(dpy, wFrame, width, 
		    shaded_height + client_height * (1 - *ppctMovement));
      XFlush(dpy);
      sleep_ms(cmsDelay);
    } while (*ppctMovement != 1.0 && ppctMovement++);
  } else {  /* roll down the window shade */
    do {
      XResizeWindow(dpy, wFrame, width, 
		    shaded_height + client_height * (*ppctMovement));
      XMoveWindow(dpy, w, 0, -deltaY * (1-*ppctMovement));
      XFlush(dpy);
      sleep_ms(cmsDelay);
    } while (*ppctMovement != 1.0 && ppctMovement++);
  }
  XMoveWindow(dpy,w,0,0);
}

/****************************************************************************
 *
 * Move the rubberband around, return with the new window location
 *
 ****************************************************************************/
void 
moveLoop(ScwmWindow * tmp_win, int XOffset, int YOffset, int Width,
	 int Height, int *FinalX, int *FinalY, Bool opaque_move,
	 Bool fAddWindow)
{
  Bool finished = False;
  Bool done;
  int xl, yt, delta_x, delta_y, paged;

  XQueryPointer(dpy, Scr.Root, &JunkRoot, &JunkChild, &xl, &yt,
		&JunkX, &JunkY, &JunkMask);
  xl += XOffset;
  yt += YOffset;

  if (((!opaque_move) && (!(Scr.flags & MWMMenus))) || (fAddWindow))
    MoveOutline(Scr.Root, xl, yt, Width, Height);

  DisplayPosition(tmp_win, xl + Scr.Vx, yt + Scr.Vy, True);

  while (!finished) {
    /* block until there is an interesting event */
    XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask | KeyPressMask |
	       PointerMotionMask | ButtonMotionMask | ExposureMask, &Event);
    StashEventTime(&Event);

    /* discard any extra motion events before a logical release */
    if (Event.type == MotionNotify) {
      while (XCheckMaskEvent(dpy, PointerMotionMask | ButtonMotionMask |
			     ButtonPressMask | ButtonRelease, &Event)) {
	StashEventTime(&Event);
	if (Event.type == ButtonRelease)
	  break;
      }
    }
    done = FALSE;
    /* Handle a limited number of key press events to allow mouseless
     * operation */
    if (Event.type == KeyPress)
      Keyboard_shortcuts(&Event, ButtonRelease);
    switch (Event.type) {
    case KeyPress:
      /* simple code to bag out of move - CKH */
      if (XLookupKeysym(&(Event.xkey), 0) == XK_Escape) {
	if (!opaque_move)
	  MoveOutline(Scr.Root, 0, 0, 0, 0);
	*FinalX = tmp_win->frame_x;
	*FinalY = tmp_win->frame_y;
	finished = TRUE;
      }
      done = TRUE;
      break;
    case ButtonPress:
      XAllowEvents(dpy, ReplayPointer, CurrentTime);
      if (((Event.xbutton.button == 2) && (!(Scr.flags & MWMMenus))) ||
	  ((Event.xbutton.button == 1) && (Scr.flags & MWMMenus) &&
	   (Event.xbutton.state & ShiftMask))) {
	NeedToResizeToo = True;
	/* Fallthrough to button-release */
      } else {
	done = 1;
	break;
      }
    case ButtonRelease:
      if (!opaque_move)
	MoveOutline(Scr.Root, 0, 0, 0, 0);
      xl = Event.xmotion.x_root + XOffset;
      yt = Event.xmotion.y_root + YOffset;

      /* Resist moving windows over the edge of the screen! */
      if (((xl + Width) >= Scr.MyDisplayWidth) &&
	  ((xl + Width) < Scr.MyDisplayWidth + Scr.MoveResistance))
	xl = Scr.MyDisplayWidth - Width - tmp_win->bw;
      if ((xl <= 0) && (xl > -Scr.MoveResistance))
	xl = 0;
      if (((yt + Height) >= Scr.MyDisplayHeight) &&
	  ((yt + Height) < Scr.MyDisplayHeight + Scr.MoveResistance))
	yt = Scr.MyDisplayHeight - Height - tmp_win->bw;
      if ((yt <= 0) && (yt > -Scr.MoveResistance))
	yt = 0;

      *FinalX = xl;
      *FinalY = yt;

      done = TRUE;
      finished = TRUE;
      break;

    case MotionNotify:
      xl = Event.xmotion.x_root;
      yt = Event.xmotion.y_root;
/*        HandlePaging(Scr.MyDisplayWidth,Scr.MyDisplayHeight,&xl,&yt,
   &delta_x,&delta_y,False);  mab */
      /* redraw the rubberband */
      xl += XOffset;
      yt += YOffset;

      /* Resist moving windows over the edge of the screen! */
      if (((xl + Width) >= Scr.MyDisplayWidth) &&
	  ((xl + Width) < Scr.MyDisplayWidth + Scr.MoveResistance))
	xl = Scr.MyDisplayWidth - Width - tmp_win->bw;
      if ((xl <= 0) && (xl > -Scr.MoveResistance))
	xl = 0;
      if (((yt + Height) >= Scr.MyDisplayHeight) &&
	  ((yt + Height) < Scr.MyDisplayHeight + Scr.MoveResistance))
	yt = Scr.MyDisplayHeight - Height - tmp_win->bw;
      if ((yt <= 0) && (yt > -Scr.MoveResistance))
	yt = 0;

      /* check Paging request once and only once after outline redrawn */
      /* redraw after paging if needed - mab */
      paged = 0;
      while (paged <= 1) {
	if (!opaque_move)
	  MoveOutline(Scr.Root, xl, yt, Width, Height);
	else {
	  if (tmp_win->flags & ICONIFIED) {
	    tmp_win->icon_x_loc = xl;
	    tmp_win->icon_xl_loc = xl -
	      (tmp_win->icon_w_width - ICON_P_WIDTH(tmp_win)) / 2;
	    tmp_win->icon_y_loc = yt;
	    if (tmp_win->icon_pixmap_w != None)
	      XMoveWindow(dpy, tmp_win->icon_pixmap_w,
			  tmp_win->icon_x_loc, yt);
	    else if (tmp_win->icon_w != None)
	      XMoveWindow(dpy, tmp_win->icon_w, tmp_win->icon_xl_loc,
			  yt + ICON_P_HEIGHT(tmp_win));

	  } else
	    XMoveWindow(dpy, tmp_win->frame, xl, yt);
	}
	DisplayPosition(tmp_win, xl + Scr.Vx, yt + Scr.Vy, False);

/* prevent window from lagging behind mouse when paging - mab */
	if (paged == 0) {
	  xl = Event.xmotion.x_root;
	  yt = Event.xmotion.y_root;
	  HandlePaging(Scr.MyDisplayWidth, Scr.MyDisplayHeight, &xl, &yt,
		       &delta_x, &delta_y, False);
	  xl += XOffset;
	  yt += YOffset;
	  if ((delta_x == 0) && (delta_y == 0))
	    break;		/* break from while paged */
	}
	paged++;
      }				/* end while paged */

      done = TRUE;
      break;

    default:
      break;
    }
    if (!done) {
      if (!opaque_move)
	MoveOutline(Scr.Root, 0, 0, 0, 0);
      DispatchEvent();
      if (!opaque_move)
	MoveOutline(Scr.Root, xl, yt, Width, Height);
    }
  }
}

/***********************************************************************
 *
 *  Procedure:
 *      DisplayPosition - display the position in the dimensions window
 *
 *  Inputs:
 *      tmp_win - the current scwm window
 *      x, y    - position of the window
 *
 ************************************************************************/

void 
DisplayPosition(ScwmWindow * tmp_win, int x, int y, int Init)
{
  char str[100];
  int offset;

  (void) sprintf(str, " %+-4d %+-4d ", x, y);
  if (Init) {
    XClearWindow(dpy, Scr.SizeWindow);
    if (Scr.d_depth >= 2)
      RelieveWindow(tmp_win, Scr.SizeWindow, 0, 0,
		    Scr.SizeStringWidth + SIZE_HINDENT * 2,
		    Scr.StdFont.height + SIZE_VINDENT * 2,
		    Scr.MenuReliefGC, Scr.MenuShadowGC, FULL_HILITE);
  } else {
    XClearArea(dpy, Scr.SizeWindow, SIZE_HINDENT, SIZE_VINDENT, Scr.SizeStringWidth,
	       Scr.StdFont.height, False);
  }

  offset = (Scr.SizeStringWidth + SIZE_HINDENT * 2
	    - XTextWidth(Scr.StdFont.font, str, strlen(str))) / 2;
  XDrawString(dpy, Scr.SizeWindow, Scr.MenuGC,
	      offset,
	      Scr.StdFont.font->ascent + SIZE_VINDENT,
	      str, strlen(str));
}


/****************************************************************************
 *
 * For menus, move, and resize operations, we can effect keyboard 
 * shortcuts by warping the pointer.
 *
 ****************************************************************************/
void 
Keyboard_shortcuts(XEvent * Event, int ReturnEvent)
{
  int x, y, x_root, y_root;
  int move_size, x_move, y_move;
  KeySym keysym;

  /* Pick the size of the cursor movement */
  move_size = Scr.EntryHeight;
  if (Event->xkey.state & ControlMask)
    move_size = 1;
  if (Event->xkey.state & ShiftMask)
    move_size = 100;

  keysym = XLookupKeysym(&Event->xkey, 0);

  x_move = 0;
  y_move = 0;
  switch (keysym) {
  case XK_Up:
  case XK_k:
  case XK_p:
    y_move = -move_size;
    break;
  case XK_Down:
  case XK_n:
  case XK_j:
    y_move = move_size;
    break;
  case XK_Left:
  case XK_b:
  case XK_h:
    x_move = -move_size;
    break;
  case XK_Right:
  case XK_f:
  case XK_l:
    x_move = move_size;
    break;
  case XK_Return:
  case XK_space:
    /* beat up the event */
    Event->type = ReturnEvent;
    break;
  case XK_Escape:
    /* simple code to bag out of move - CKH */
    /* return keypress event instead */
    Event->type = KeyPress;
    Event->xkey.keycode = XKeysymToKeycode(Event->xkey.display, keysym);
    break;
  default:
    break;
  }
  XQueryPointer(dpy, Scr.Root, &JunkRoot, &Event->xany.window,
		&x_root, &y_root, &x, &y, &JunkMask);

  if ((x_move != 0) || (y_move != 0)) {
    /* beat up the event */
    XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, x_root + x_move,
		 y_root + y_move);

    /* beat up the event */
    Event->type = MotionNotify;
    Event->xkey.x += x_move;
    Event->xkey.y += y_move;
    Event->xkey.x_root += x_move;
    Event->xkey.y_root += y_move;
  }
}


void 
InteractiveMove(Window * win, ScwmWindow * tmp_win, 
		int *FinalX, int *FinalY, XEvent * eventp)
{
  extern int Stashed_X, Stashed_Y;
  int origDragX, origDragY, DragX, DragY, DragWidth, DragHeight;
  int XOffset, YOffset;
  Window w;

  Bool opaque_move = False;

  w = *win;

  InstallRootColormap();
  if (menuFromFrameOrWindowOrTitlebar) {
    /* warp the pointer to the cursor position from before menu appeared */
    XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, Stashed_X, Stashed_Y);
    XFlush(dpy);
  }
  DragX = eventp->xbutton.x_root;
  DragY = eventp->xbutton.y_root;

  /* If this is left commented out, then the move starts from the button press 
   * location instead of the current location, which seems to be an
   * improvement */
  /* XQueryPointer(dpy, Scr.Root, &JunkRoot, &JunkChild,
     &DragX, &DragY,    &JunkX, &JunkY, &JunkMask);
   */
  if (!GrabEm(MOVE)) {
    XBell(dpy, Scr.screen);
    return;
  }
  XGetGeometry(dpy, w, &JunkRoot, &origDragX, &origDragY,
	       (unsigned int *) &DragWidth, (unsigned int *) &DragHeight,
	       &JunkBW, &JunkDepth);

  if (DragWidth * DragHeight <
      (Scr.OpaqueSize * Scr.MyDisplayWidth * Scr.MyDisplayHeight) / 100)
    opaque_move = True;
  else
    XGrabServer_withSemaphore(dpy);

  if ((!opaque_move) && (tmp_win->flags & ICONIFIED))
    XUnmapWindow(dpy, w);

  DragWidth += JunkBW;
  DragHeight += JunkBW;
  XOffset = origDragX - DragX;
  YOffset = origDragY - DragY;
  XMapRaised(dpy, Scr.SizeWindow);
  moveLoop(tmp_win, XOffset, YOffset, DragWidth, DragHeight, FinalX, FinalY,
	   opaque_move, False);

  XUnmapWindow(dpy, Scr.SizeWindow);
  UninstallRootColormap();

  if (!opaque_move)
    XUngrabServer_withSemaphore(dpy);
  UngrabEm();

}
