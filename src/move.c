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
#include "util.h"

extern XEvent Event;
extern int menuFromFrameOrWindowOrTitlebar;

float rgpctMovementDefault[32] = {
    -.01, 0, .01, .03,.08,.18,.3,.45,.60,.75,.85,.90,.94,.97,.99,1.0 
    /* must end in 1.0 */
  };

int cmsDelayDefault = 10; /* milliseconds */

/* Perform the movement of the window. ppctMovement *must* have a 1.0 entry
   somewhere in ins list of floats, and movement will stop when it hits a 1.0 entry
   The positions given are viewport positions (not virtual */
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

  if (startX < 0 || startY < 0) {
    FXGetWindowTopLeft(w,&currentX,&currentY);
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
      WXGetPointerWindowOffsets(Scr.Root,&pointerX,&pointerY);
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

    ms_sleep(cmsDelay);
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
  int shaded_height = psw->title_height + psw->boundary_width;
  /* FIXGJB above was psw->title_height + 2 * (psw->boundary_width + psw->bw); */
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
      ms_sleep(cmsDelay);
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
      ms_sleep(cmsDelay);
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


#if 0 /* FIXGJB: remove old version */
static void
SnapCoordsToEdges(int *px, int *py, int width, int height, int bw, int resistance)
{
  int xr = *px + width + 2*bw;
  int yb = *py + height + 2*bw;
  /* Resist moving windows over the edge of the screen! */
  if ((xr >= Scr.DisplayWidth) && (xr < Scr.DisplayWidth + resistance))
    *px = Scr.DisplayWidth - width - bw*2;
  if ((*px < 0) && (*px > -resistance))
    *px = 0;
  if ((yb >= Scr.DisplayHeight) && (yb < Scr.DisplayHeight + resistance))
    *py = Scr.DisplayHeight - height - bw*2;
  if ((*py < 0) && (*py > -resistance))
    *py = 0;
}
#else
/* New version from Todd Larson */
static void
SnapCoordsToEdges(int *px, int *py, int width, int height, int bw, int resistance)
{
  int pixel_past_last, last_pixel, last_pixel_on_screen;
 
  /* Resist moving windows over the edge of the screen! */
  pixel_past_last = *px + bw + width + bw;
  last_pixel = pixel_past_last - 1;
  last_pixel_on_screen = Scr.DisplayWidth - 1;
 
  /* *px + width + 2*bw > Scr.DisplayWidth */
  if (last_pixel > last_pixel_on_screen &&
      last_pixel < last_pixel_on_screen + resistance) {
    /* last_pixel = last_pixel_on_screen */
    /* pixel_past_last - 1 = Scr.DisplayWidth - 1 */
    /* *px + bw + width + bw = Scr.DisplayWidth; */
    *px = Scr.DisplayWidth - width - bw*2;
  }
 
  if ((*px < 0) && (*px > -resistance))
    *px = 0;
 
  pixel_past_last = *py + bw + height + bw;
  last_pixel = pixel_past_last - 1;
  last_pixel_on_screen = Scr.DisplayHeight - 1;
 
  if (last_pixel > last_pixel_on_screen &&
      last_pixel < last_pixel_on_screen + resistance) {
    *py = Scr.DisplayHeight - height - bw*2;
  }
 
  if ((*py < 0) && (*py > -resistance))
    *py = 0;
}
#endif

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
moveLoop(ScwmWindow * psw, int XOffset, int YOffset, int OutlineWidth,
	 int OutlineHeight, int *FinalX, int *FinalY, Bool opaque_move)
{
  Bool finished = False;
  Bool done;
  int xl, yt, paged;

  /* show the size/position window */
  MapMessageWindow();

  WXGetPointerWindowOffsets(Scr.Root, &xl, &yt);
  xl += XOffset;
  yt += YOffset;


  if (!opaque_move) {
    RedrawOutlineAtNewPosition(Scr.Root, xl, yt, OutlineWidth, OutlineHeight);
  }

  DisplayPosition(psw, 
                  xl + WIN_VP_OFFSET_X(psw), yt + WIN_VP_OFFSET_Y(psw), 
                  True);

  if (!psw->fIconified) {
    CassowaryEditPosition(psw);
  }

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
    /* Handle key press events to allow mouseless operation */
    if (Event.type == KeyPress)
      Keyboard_shortcuts(&Event, ButtonRelease, psw, False);
    switch (Event.type) {
    case KeyPress:
      /* simple code to bag out of move - CKH */
      if (XLookupKeysym(&(Event.xkey), 0) == XK_Escape) {
	finished = True;
      }
      SnapCoordsToEdges(&xl, &yt, psw->frame_width, psw->frame_height,
			psw->bw, Scr.MoveResistance);
      done = True;
      break;
    case ButtonPress:
      XAllowEvents(dpy, ReplayPointer, CurrentTime);
      if (Event.xbutton.button == 2 ||
          (Event.xbutton.button == 1 && (Event.xbutton.state & ShiftMask))) {
	/* FIXGJB: this hack removed: NeedToResizeToo = True; */
	/* Fallthrough to button-release */
      } else {
	done = True;
	break;
      }
    case ButtonRelease:
      xl = Event.xmotion.x_root + XOffset;
      yt = Event.xmotion.y_root + YOffset;


      done = True;
      finished = True;
      break;

    case MotionNotify:
      xl = Event.xmotion.x_root;
      yt = Event.xmotion.y_root;
      xl += XOffset;
      yt += YOffset;

      /* Resist moving windows over the edge of the screen! */
      SnapCoordsToEdges(&xl, &yt, psw->frame_width, psw->frame_height,
			psw->bw, Scr.MoveResistance);

      /* check Paging request once and only once after outline redrawn */
      /* redraw after paging if needed - mab */
      paged = 0;
      while (paged <= 1) {
        if (psw->fIconified) {
          psw->icon_x_loc = xl + ICON_VP_OFFSET_X(psw);
          psw->icon_xl_loc = xl -
            (psw->icon_w_width - psw->icon_p_width) / 2 +
            ICON_VP_OFFSET_X(psw);
          psw->icon_y_loc = yt + ICON_VP_OFFSET_Y(psw);
          if (opaque_move) {
            MovePswIconToCurrentPosition(psw);
          } else {
            RedrawOutlineAtNewPosition(Scr.Root, 
                                       ICON_X_VP(psw), ICON_Y_VP(psw),
                                       OutlineWidth, OutlineHeight);
          }
        } else {
          /* the solver's resolve does the move window */
          /* if not using Cassowary, this just does an XMoveWindow */
          SuggestMoveWindowTo(psw,
                              WIN_VP_OFFSET_X(psw)+xl,
                              WIN_VP_OFFSET_Y(psw)+yt,opaque_move);
        }
	DisplayPosition(psw,
                        xl + WIN_VP_OFFSET_X(psw),
                        yt + WIN_VP_OFFSET_Y(psw), True);

        /* prevent window from lagging behind mouse when paging - mab */
	if (paged == 0) {
	  int xcenter = Event.xmotion.x_root;
	  int ycenter = Event.xmotion.y_root;
          int delta_x = 0;
          int delta_y = 0;
	  HandlePaging(Scr.DisplayWidth, Scr.DisplayHeight, &xcenter, &ycenter,
		       &delta_x, &delta_y, False);
	  if ((delta_x != 0) || (delta_y == 0)) {
            xl += delta_x;
            yt += delta_y;
          }
	}
	paged++;
      }

      done = True;
      break;

    default:
      break;
    }
    if (!done) {
      DispatchEvent();
    }
  }
  if (!opaque_move)
    RemoveRubberbandOutline(Scr.Root);

  SnapCoordsToEdges(&xl, &yt, psw->frame_width, psw->frame_height,
		    psw->bw, Scr.MoveResistance);
  if (!psw->fIconified) {
    SuggestMoveWindowTo(psw,WIN_VP_OFFSET_X(psw)+xl,WIN_VP_OFFSET_Y(psw)+yt,True);
    CassowaryEndEdit(psw);
  } else {
    /* we're moving an icon */
    if (!opaque_move) {
      /* we finally need to move the real windows for the icon */
      MovePswIconToCurrentPosition(psw);
    }
  }
    
  *FinalX = WIN_VP_OFFSET_X(psw) + xl;
  *FinalY = WIN_VP_OFFSET_Y(psw) + yt;
  UnmapMessageWindow();
}



const double message_hilight_factor = 1.2;
const double message_shadow_factor = 0.5;

/* FIXGJB: It'd be nice to make this facility available at the scheme level */

/*
 * DisplayMessage - Display a string in the dimensions window
 *      psw - the current scwm window
 *      x, y    - position of the window
 */
void 
DisplayMessage(const char *sz, Bool fRelief)
{
  int textwidth = ComputeXTextWidth(XFONT(Scr.msg_window_font),sz, -1);
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

  NewFontAndColor(gcMsg,XFONTID(Scr.msg_window_font),
                  XCOLOR(Scr.msg_window_fg), XCOLOR(Scr.msg_window_bg)); 

#ifdef I18N
  XmbDrawString(dpy, Scr.MsgWindow, XFONT(Scr.msg_window_font),
#else
  XDrawString(dpy, Scr.MsgWindow, 
#endif
              gcMsg, SIZE_HINDENT, FONTY(Scr.msg_window_font) + SIZE_VINDENT,
	      sz, strlen(sz));
#define MATCH_EXTRA_LP_ABOVE )  /* crazy hack, but hey, it works --07/31/98 gjb */
#undef MATCH_EXTRA_LP_ABOVE
}


/*
 * For menus, move, and resize operations, we can effect keyboard 
 * shortcuts by warping the pointer.
 */
void 
Keyboard_shortcuts(XEvent *Event, int ReturnEvent, 
                   const ScwmWindow *psw, Bool fResize)
{
  int x, y, x_root, y_root;
  int xmove_size, ymove_size, x_move, y_move;
  KeySym keysym;
  Bool fOnXBoundary = False;
  Bool fOnYBoundary = False;

  Event->xany.window =
    WXGetPointerOffsets(psw?psw->frame:Scr.Root, &x_root, &y_root, &x, &y);

  /* Pick the size of the cursor movement */
  if (fResize && psw) {
    xmove_size = psw->hints.width_inc;
    ymove_size = psw->hints.height_inc;
    if (Event->xkey.state & ShiftMask) {
      xmove_size *= 5;
      ymove_size *= 5;
    }
  } else {
    xmove_size = 10;
    if (Event->xkey.state & ControlMask)
      xmove_size = 1;
    if (Event->xkey.state & ShiftMask)
      xmove_size = 100;
    ymove_size = xmove_size;
  }

  /* we're on the boundary if we're within 2 pixels */
  if (psw) {
    if (x >= (psw->frame_width - 2) || (x <= 2))
      fOnXBoundary = True;
    
    if (y >= (psw->frame_height - 2) || (y <= 2))
      fOnYBoundary = True;
  }
    
  keysym = XLookupKeysym(&Event->xkey, 0);

  x_move = 0;
  y_move = 0;
  switch (keysym) {
  case XK_Up:
  case XK_k:
  case XK_p:
    if (fResize && !fOnYBoundary) y_move = -y - ymove_size;
    else y_move = -ymove_size;
    break;
  case XK_Down:
  case XK_n:
  case XK_j:
    if (fResize && !fOnYBoundary) y_move = psw->frame_height - y + ymove_size;
    else y_move = ymove_size;
    break;
  case XK_Left:
  case XK_b:
  case XK_h:
    if (fResize && !fOnXBoundary) x_move = -x - xmove_size;
    else x_move = -xmove_size;
    break;
  case XK_Right:
  case XK_f:
  case XK_l:
    if (fResize && !fOnXBoundary) x_move = psw->frame_width - x + xmove_size;
    else x_move = xmove_size;
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
    psw is the ScwmWindow to move
    FinalX, FinalY are used to return the final position */
Bool 
InteractiveMove(ScwmWindow *psw, Bool fOpaque,
		int *FinalX, int *FinalY)
{
  int origDragX, origDragY, DragX, DragY;
  unsigned int DragWidth, DragHeight;
  int border_width;
  int XOffset, YOffset;
  Window w = psw->frame;
  /* FIXGJB: pass these in instead */
  extern Bool have_orig_position;
  extern int orig_x, orig_y;

  /* Allow using the rubberband for initial placement even with
     cassowary in use */
  if (FXIsWindowMapped(dpy,psw->w))
    CassowaryModifyOpaqueFlag(&fOpaque);

  /* find out from where we should start the move */
  if (have_orig_position) {
    DragX = orig_x;
    DragY = orig_y;
  } else {
    /* just use current position */
    WXGetPointerWindowOffsets(Scr.Root, &DragX, &DragY);
  }

  InstallRootColormap();

  if (!GrabEm(CURSOR_MOVE)) {
    call0_hooks(invalid_interaction_hook);
    return False;
  }

  if (psw->fIconified) {
    if (psw->icon_pixmap_w != None)
      w = psw->icon_pixmap_w;
    else
      w = psw->icon_w;
  }

  XGetGeometry(dpy, w, &JunkRoot, &origDragX, &origDragY,
	       &DragWidth, &DragHeight, &border_width, &JunkDepth);

  if (!fOpaque) {
    XGrabServer_withSemaphore(dpy);
    /* enlarge the draw rubberband to include the border width */
    DragWidth += border_width;
    DragHeight += border_width;
    if (SHADED_P(psw)) {
      DragHeight = psw->title_height + psw->boundary_width;
    }
  }


  XOffset = origDragX - DragX;
  YOffset = origDragY - DragY;

  moveLoop(psw, XOffset, YOffset, DragWidth, DragHeight, FinalX, FinalY, fOpaque);
  if (psw->fIconified) {
    psw->fIconMoved = True;
  }

  UninstallRootColormap();

  if (!fOpaque)
    XUngrabServer_withSemaphore(dpy);

  UngrabEm();
  return True;
}


SCWM_PROC(interactive_move, "interactive-move", 0, 2, 0,
          (SCM win, SCM opaque_p))
     /** Move WIN interactively.
This allows the user to drag a rubber band frame or the window itself
around the screen. WIN defaults to the window context in the
usual way if not specified.  If OPAQUE? is #t, the move will be done
"opaquely", moving the actual X window, if #f a rubberband will be
used instead to save on server computation (note that the rubberband
requires a server "grab" which means that nothing else changes on
screen while the non-opaque move takes place. */
#define FUNC_NAME s_interactive_move
{
  ScwmWindow *psw;
  int x, y;                     /* not used now */
  Bool fOpaque;

  SCM_REDEFER_INTS;
  VALIDATE_PRESS_ONLY(win, FUNC_NAME);
  COPY_BOOL_OR_ERROR_DEFAULT_FALSE(fOpaque,opaque_p,2,FUNC_NAME);
  psw = PSWFROMSCMWIN(win);
  InteractiveMove(psw, fOpaque, &x, &y);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


void 
init_move()
{
#ifndef SCM_MAGIC_SNARFER
#include "move.x"
#endif
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
