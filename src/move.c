/* $Id$
 * move.c
 * (C) 1998 Maciej Stachowiak and Greg J. Badros
 */

/*
 * This module is derived from all original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 */

/***********************************************************************
 *
 * code for moving windows
 *
 ***********************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <assert.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <X11/keysym.h>
#include "scwm.h"
#include "move.h"
#include "events.h"
#include "screen.h"
#include "Grab.h"
#include "icons.h"
#include "resize.h"
#include "borders.h"
#include "colormaps.h"
#include "font.h"
#include "focus.h"
#include "syscompat.h"
#include "virtual.h"
#include "xmisc.h"
#include "callbacks.h"

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
    currentX = (int) (startX + deltaX * (*ppctMovement));
    currentY = (int) (startY + deltaY * (*ppctMovement));
    XMoveWindow(dpy,w,currentX,currentY);
    if (fWarpPointerToo) {
      FXGetPointerWindowOffsets(Scr.Root,&pointerX,&pointerY);
      pointerX += currentX - lastX;
      pointerY += currentY - lastY;
      XWarpPointer(dpy,None,Scr.Root,0,0,0,0,
		   pointerX,pointerY);
    }
    XFlush(dpy);
    /* handle expose events as we're animating the window move */
    while (XCheckMaskEvent(dpy,  ExposureMask, &Event))
      {
      DispatchEvent();
      }

    usleep(cmsDelay);
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
/* Note that this does not allow animations to overshoot target-- it
   stops at first pctMovement >= 1.0 --11/25/97 gjb */
void 
AnimatedShadeWindow(ScwmWindow *psw, Bool fRollUp, 
		    int cmsDelay, float *ppctMovement)
{
  Window w = psw->w;
  Window wFrame = psw->frame;
  int width = FRAME_WIDTH(psw);
  int shaded_height = psw->title_height + 2 * (psw->boundary_width + psw->bw);
  int normal_height = psw->orig_height;
  int client_height = normal_height - shaded_height;
  /* set our defaults */
  if (ppctMovement == NULL) ppctMovement = rgpctMovementDefault;
  if (cmsDelay < 0)         cmsDelay     = cmsDelayDefault;
  
  if (fRollUp) {
    XLowerWindow(dpy,w);
    do {
      XMoveWindow(dpy, w, 0, (int) (-client_height * (*ppctMovement)));
      XResizeWindow(dpy, wFrame, width, 
		    (int) (shaded_height + client_height * (1 - *ppctMovement)));
      XFlush(dpy);
      /* handle expose events as we're rolling up the window shade */
      while (XCheckMaskEvent(dpy,  ExposureMask, &Event))
	DispatchEvent();
      usleep(cmsDelay);
    } while (*ppctMovement < 1.0 && ppctMovement++);
    XMoveWindow(dpy,w,0,-client_height);
    XResizeWindow(dpy,wFrame,width,shaded_height);
  } else {  /* roll down the window shade */
    do {
      XResizeWindow(dpy, wFrame, width, 
		    (int) (shaded_height + client_height * (*ppctMovement)));
      XMoveWindow(dpy, w, 0, (int) (-client_height * (1 - *ppctMovement)));
      XFlush(dpy);
      while (XCheckMaskEvent(dpy,  ExposureMask, &Event))
	DispatchEvent();
      usleep(cmsDelay);
    } while (*ppctMovement < 1.0 && ppctMovement++);
    XResizeWindow(dpy,wFrame,width,shaded_height+client_height);
    XMoveWindow(dpy,w,0,0);
  }
  XFlush(dpy);
}

void
MapMessageWindow()
{
  int w, h;
  if (!FXGetWindowSize(Scr.MsgWindow,&w,&h))
    assert(False);

  /* center it onscreen */
  XMoveWindow(dpy, Scr.MsgWindow, 
              Scr.DisplayWidth/2 - w/2,
              Scr.DisplayHeight/2 - h/2);
  XMapRaised(dpy, Scr.MsgWindow);
}

void
UnmapMessageWindow()
{
  XUnmapWindow(dpy, Scr.MsgWindow);
}



static void
DisplayPosition(ScwmWindow *psw, int x, int y, Bool fRelief)
{
  char sz[30];
  sprintf(sz, " %+-4d %+-4d ", x, y);
  DisplayMessage(sz,fRelief);
}


/*
  Move the window around, return with the new window location in
  Final[XY]
  
  XOffset, YOffset are amounts (in pixels) that the pointer has moved
  since the original button press event (so the window should be moved
  by that amount, initially)

  Width and Height refer to the size of the window (for drawing the
  rubberband)

  FinalX, FinalY are used to return the ending position of the move

  opaque_move is true iff the window itself should be moved, instead
  of the rubberband

 */
void 
moveLoop(ScwmWindow * psw, int XOffset, int YOffset, int Width,
	 int Height, int *FinalX, int *FinalY, Bool opaque_move)
{
  Bool finished = False;
  Bool done;
  int xl, yt, delta_x, delta_y, paged;

  /* show the size/position window */
  MapMessageWindow();

  FXGetPointerWindowOffsets(Scr.Root, &xl, &yt);
  xl += XOffset;
  yt += YOffset;

  if (((!opaque_move) && (!(Scr.fMWMMenus))))
    RedrawOutlineAtNewPosition(Scr.Root, xl, yt, Width, Height);

  DisplayPosition(psw, xl + Scr.Vx, yt + Scr.Vy, True);

  CassowaryEditPosition(psw);

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
    done = False;
    /* Handle a limited number of key press events to allow mouseless
     * operation */
    if (Event.type == KeyPress)
      Keyboard_shortcuts(&Event, ButtonRelease);
    switch (Event.type) {
    case KeyPress:
      /* simple code to bag out of move - CKH */
      if (XLookupKeysym(&(Event.xkey), 0) == XK_Escape) {
	if (!opaque_move)
          RemoveRubberbandOutline(Scr.Root);
	finished = True;
      }
      done = True;
      break;
    case ButtonPress:
      XAllowEvents(dpy, ReplayPointer, CurrentTime);
      if (((Event.xbutton.button == 2) && !Scr.fMWMMenus) ||
	  ((Event.xbutton.button == 1) && Scr.fMWMMenus &&
	   (Event.xbutton.state & ShiftMask))) {
	NeedToResizeToo = True;
	/* Fallthrough to button-release */
      } else {
	done = 1;
	break;
      }
    case ButtonRelease:
      if (!opaque_move)
	RemoveRubberbandOutline(Scr.Root);
      xl = Event.xmotion.x_root + XOffset;
      yt = Event.xmotion.y_root + YOffset;

      /* Resist moving windows over the edge of the screen! */
      if (((xl + Width) >= Scr.DisplayWidth) &&
	  ((xl + Width) < Scr.DisplayWidth + Scr.MoveResistance))
	xl = Scr.DisplayWidth - Width - psw->bw;
      if ((xl <= 0) && (xl > -Scr.MoveResistance))
	xl = 0;
      if (((yt + Height) >= Scr.DisplayHeight) &&
	  ((yt + Height) < Scr.DisplayHeight + Scr.MoveResistance))
	yt = Scr.DisplayHeight - Height - psw->bw;
      if ((yt <= 0) && (yt > -Scr.MoveResistance))
	yt = 0;

      done = True;
      finished = True;
      break;

    case MotionNotify:
      xl = Event.xmotion.x_root;
      yt = Event.xmotion.y_root;
/*        HandlePaging(Scr.DisplayWidth,Scr.DisplayHeight,&xl,&yt,
   &delta_x,&delta_y,False);  mab */
      /* redraw the rubberband */
      xl += XOffset;
      yt += YOffset;

      /* Resist moving windows over the edge of the screen! */
      if (((xl + Width) >= Scr.DisplayWidth) &&
	  ((xl + Width) < Scr.DisplayWidth + Scr.MoveResistance))
	xl = Scr.DisplayWidth - Width - psw->bw;
      if ((xl <= 0) && (xl > -Scr.MoveResistance))
	xl = 0;
      if (((yt + Height) >= Scr.DisplayHeight) &&
	  ((yt + Height) < Scr.DisplayHeight + Scr.MoveResistance))
	yt = Scr.DisplayHeight - Height - psw->bw;
      if ((yt <= 0) && (yt > -Scr.MoveResistance))
	yt = 0;

      /* check Paging request once and only once after outline redrawn */
      /* redraw after paging if needed - mab */
      paged = 0;
      while (paged <= 1) {
	if (!opaque_move)
	  RedrawOutlineAtNewPosition(Scr.Root, xl, yt, Width, Height);
	else {
	  if (psw->fIconified) {
	    psw->icon_x_loc = xl;
	    psw->icon_xl_loc = xl -
	      (psw->icon_w_width - psw->icon_p_width) / 2;
	    psw->icon_y_loc = yt;
	    if (psw->icon_pixmap_w != None)
	      XMoveWindow(dpy, psw->icon_pixmap_w,
			  psw->icon_x_loc, yt);
	    else if (psw->icon_w != None)
	      XMoveWindow(dpy, psw->icon_w, psw->icon_xl_loc,
			  yt + psw->icon_p_height);

	  } else {
            /* the solver's resolve does the move window */
            /* if not using Cassowary, this just does an XMoveWindow */
            SuggestMoveWindowTo(psw,xl,yt);
          }
	}
	DisplayPosition(psw, xl + Scr.Vx, yt + Scr.Vy, True);

/* prevent window from lagging behind mouse when paging - mab */
	if (paged == 0) {
	  xl = Event.xmotion.x_root;
	  yt = Event.xmotion.y_root;
	  HandlePaging(Scr.DisplayWidth, Scr.DisplayHeight, &xl, &yt,
		       &delta_x, &delta_y, False);
	  xl += XOffset;
	  yt += YOffset;
	  if ((delta_x == 0) && (delta_y == 0))
	    break;		/* break from while paged */
	}
	paged++;
      }				/* end while paged */

      done = True;
      break;

    default:
      break;
    }
    if (!done) {
      if (!opaque_move)
        RemoveRubberbandOutline(Scr.Root);
      DispatchEvent();
      if (!opaque_move)
	RedrawOutlineAtNewPosition(Scr.Root, xl, yt, Width, Height);
    }
  }
  SuggestMoveWindowTo(psw,xl,yt);
  CassowaryEndEdit(psw);
  *FinalX = xl;
  *FinalY = yt;
  UnmapMessageWindow();
}



const double message_hilight_factor = 1.2;
const double message_shadow_factor = 0.5;

/* Return the width of the string sz in pixels
   Takes the font as an XFontStruct or an XFontSet depending
   on i18n support; the arg passed is XFONT(scmFont) */
int
ComputeXTextWidth(
#ifdef I18N
XFontSet
#else
XFontStruct
#endif
                  *pxfs, const char *sz)
{
#ifdef I18N
  XRectangle dummy,log_ret;
  XmbTextExtents(XFONT(Scr.msg_window_font), sz, strlen(sz), &dummy, &log_ret);
  return log_ret.width;
#else
  return XTextWidth(pxfs, sz, strlen(sz));
#endif
}

/* FIXGJB: It'd be nice to make this facility available at the scheme level */

/*
 * DisplayMessage - Display a string in the dimensions window
 *      psw - the current scwm window
 *      x, y    - position of the window
 */
void 
DisplayMessage(const char *sz, Bool fRelief)
{
  int textwidth = ComputeXTextWidth(XFONT(Scr.msg_window_font),sz);
  int winwidth = textwidth + SIZE_HINDENT*2;
  int textheight = FONTHEIGHT(Scr.msg_window_font);
  int winheight = textheight + SIZE_VINDENT*2;
  GC gcMsg = Scr.ScratchGC2;
  GC gcHilite = Scr.ScratchGC2;
  GC gcShadow = Scr.ScratchGC3;
  SCM scmFgRelief, scmBgRelief;
  scmBgRelief = adjust_brightness(Scr.msg_window_bg, message_shadow_factor);
  scmFgRelief = adjust_brightness(Scr.msg_window_bg, message_hilight_factor);

  SetGCFg(gcHilite,XCOLOR(scmFgRelief));
  SetGCFg(gcShadow,XCOLOR(scmBgRelief));
  
  XMoveResizeWindow(dpy, Scr.MsgWindow, 
                    (Scr.DisplayWidth-winwidth)/2,(Scr.DisplayHeight-winheight)/2,
                    winwidth, winheight);

  if (fRelief) {
    XClearWindow(dpy, Scr.MsgWindow);
    if (Scr.d_depth >= 2) {
      RelieveRectangle(Scr.MsgWindow, 0, 0, winwidth, winheight,
                       gcHilite, gcShadow);
    }
  } else {
    XClearArea(dpy, Scr.MsgWindow, 0, 0, 
               textwidth, textheight, False);
  }

  NewFontAndColor(gcMsg,XFONT(Scr.msg_window_font)->fid,
                  XCOLOR(Scr.msg_window_fg), XCOLOR(Scr.msg_window_bg)); 

#ifdef I18N
  XmbDrawString(dpy, Scr.MsgWindow, XFONT(Scr.msg_window_font), /* ) */
#else
  XDrawString(dpy, Scr.MsgWindow, 
#endif
              gcMsg, SIZE_HINDENT, FONTY(Scr.msg_window_font) + SIZE_VINDENT,
	      sz, strlen(sz));
}


/*
 * For menus, move, and resize operations, we can effect keyboard 
 * shortcuts by warping the pointer.
 */
void 
Keyboard_shortcuts(XEvent * Event, int ReturnEvent)
{
  int x, y, x_root, y_root;
  int move_size, x_move, y_move;
  KeySym keysym;

  /* Pick the size of the cursor movement */
  move_size = 10;
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


/* InteractiveMove
    w is psw->frame, the X window we must move
    psw is the ScwmWindow to move
    FinalX, FinalY are used to return the final position
    eventp is the event that initiated the move -- in particular
       it contains the coordinates of the button press */
void 
InteractiveMove(Window w, ScwmWindow * psw, 
		int *FinalX, int *FinalY, XEvent *eventp)
{
  int origDragX, origDragY, DragX, DragY;
  unsigned int DragWidth, DragHeight;
  int border_width;
  int XOffset, YOffset;
  Bool opaque_move = False;
  assert(w == psw->frame || w == psw->icon_w || w == psw->icon_pixmap_w);

  InstallRootColormap();

  /* the move starts from the button press location, not from
     the current location */
  DragX = eventp->xbutton.x_root;
  DragY = eventp->xbutton.y_root;

  if (!GrabEm(CURSOR_MOVE)) {
    call0_hooks(invalid_interaction_hook);
    return;
  }
  XGetGeometry(dpy, w, &JunkRoot, &origDragX, &origDragY,
	       &DragWidth, &DragHeight, &border_width, &JunkDepth);

  /* FIXGJB: this should be a single proc predicate callback
     with one argument: the window to be moved interactively,
     then remove the Scr.OpaqueSize var and setters/getters
  */
  if (DragWidth * DragHeight <
      (Scr.OpaqueSize * Scr.DisplayWidth * Scr.DisplayHeight) / 100)
    opaque_move = True;
  else
    XGrabServer_withSemaphore(dpy);

  if (!opaque_move && psw->fIconified)
    XUnmapWindow(dpy, w);

  /* enlarge the draw rubberband to include the border width */
  DragWidth += border_width;
  DragHeight += border_width;

  XOffset = origDragX - DragX;
  YOffset = origDragY - DragY;

  moveLoop(psw, XOffset, YOffset, DragWidth, DragHeight, FinalX, FinalY, opaque_move);

  UninstallRootColormap();

  if (!opaque_move)
    XUngrabServer_withSemaphore(dpy);

  UngrabEm();
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
