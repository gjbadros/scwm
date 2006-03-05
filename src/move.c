/* $Id$
 * move.c
 * Copyright (C) 1998, 1999, 2000  Greg J. Badros and Maciej Stachowiak 
 *
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
#include "scwmconfig.h"
#endif

#include <stdio.h>
#include <assert.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <X11/keysym.h>

#include "move.h"

#include "scwm.h"
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
#include "cursor.h"

extern XEvent Event;
extern int menuFromFrameOrWindowOrTitlebar;

SCWM_HOOK(interactive_move_start_hook,"interactive-move-start-hook", 1,
"This hook is invoked at the start of an interactive move.\n\
It is called with one argument, WINDOW.");

SCWM_HOOK(interactive_move_new_position_hook,"interactive-move-new-position-hook", 3,
"This hook is invoked during an interactive move.\n\
It is called with three arguments, WINDOW, NEW-VP-X, and NEW-VP-Y,\n\
whenever the window is moved to a new location. The position refers\n\
to the position of the frame window (not the client window) in\n\
viewport coordinates.");

SCWM_HOOK(interactive_move_finish_hook,"interactive-move-finish-hook", 1,
"This hook is invoked at the end of an interactive move.\n\
It is called with one argument, WINDOW.");

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
  int xl, yt, paged, real_x, real_y, saved_x, saved_y; 
  
  /* show the size/position window */

  WXGetPointerWindowOffsets(Scr.Root, &xl, &yt);
  xl += XOffset;
  yt += YOffset;


  if (!opaque_move) {
    RedrawOutlineAtNewPosition(xl, yt, OutlineWidth, OutlineHeight);
  }

  if (psw->fIconified) {
    saved_x = ICON_X_VP(psw);
    saved_y = ICON_Y_VP(psw);
  } else {
    saved_x = FRAME_X_VP(psw);
    saved_y = FRAME_Y_VP(psw);
    CassowaryEditPosition(psw);
  }

  /* same hook is called identically before the iterations; see above */
  scwm_run_hook(interactive_move_new_position_hook, 
                scm_list_n(SCM_FROM_PSW(psw),
			   scm_from_int(saved_x), scm_from_int(saved_y),
			   SCM_UNDEFINED));

  while (!finished) {
    while (XCheckMaskEvent(dpy, 
                           ButtonPressMask | ButtonReleaseMask | KeyPressMask |
                           PointerMotionMask | ButtonMotionMask | ExposureMask | VisibilityChangeMask, 
                           &Event) == False) {
#ifndef NOT_MORE_RESPONSIVE
      NoEventsScwmUpdate(False);
#else
      ms_sleep(10);
#endif
    }
    /* fallen through, so we got an event we're interested in */
    StashEventTime(&Event);

    /* discard any extra motion events before a logical release */
    if (Event.type == MotionNotify) {
      while (XCheckMaskEvent(dpy, PointerMotionMask | ButtonMotionMask |
                             ButtonPressMask | ButtonReleaseMask, &Event)) {
	StashEventTime(&Event);
	if (Event.type == ButtonRelease || Event.type == ButtonPress) {
#ifdef SCWM_DEBUG_MOVE_LOOP
          fprintf(stderr,"leaving early due to button event\n");
#endif
	  break;
        }
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
	if (psw->fIconified) {
          psw->icon_x_loc = saved_x + ICON_VP_OFFSET_X(psw);
          psw->icon_xl_loc = saved_x -
            (psw->icon_w_width - psw->icon_p_width) / 2 +
            ICON_VP_OFFSET_X(psw);
          psw->icon_y_loc = saved_y + ICON_VP_OFFSET_Y(psw);
	} else {
	  xl = saved_x;
	  yt = saved_y;
	}
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
	/* GJB:FIXME:: this hack removed: NeedToResizeToo = True; */
	/* Fallthrough to button-release */
      } else {
	done = True;
	break;
      }
    case ButtonRelease:
      xl = Event.xbutton.x_root + XOffset;
      yt = Event.xbutton.y_root + YOffset;

#ifdef SCWM_DEBUG_MOVE_LOOP
      fprintf(stderr,"button released\n");
#endif
      done = True;
      finished = True;
      break;

    case MotionNotify:
      xl = Event.xmotion.x_root + XOffset;
      yt = Event.xmotion.y_root + YOffset;

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
            RedrawOutlineAtNewPosition(ICON_X_VP(psw), ICON_Y_VP(psw),
                                       OutlineWidth, OutlineHeight);
          }
	  real_x = ICON_X_VP(psw);
	  real_y = ICON_Y_VP(psw);
        } else {
          /* the solver's resolve does the move window */
          /* if not using Cassowary, this just does an XMoveWindow */
          SuggestMoveWindowTo(psw,
                              WIN_VP_OFFSET_X(psw)+xl,
                              WIN_VP_OFFSET_Y(psw)+yt,opaque_move);
	  /* recheck - may not have moved at all */
	  real_x = FRAME_X_VP(psw);
	  real_y = FRAME_Y_VP(psw);
        }

        /* same hook is called above, before the iterations begin */
#ifdef SCWM_DEBUG_MOVE_LOOP
        fprintf(stderr,"new-position hook %d,%d, finalX/Y %d,%d\n",
                real_x,real_y,
                WIN_VP_OFFSET_X(psw)+xl,WIN_VP_OFFSET_Y(psw)+yt);
#endif
	scwm_run_hook(interactive_move_new_position_hook, 
                      scm_list_n(SCM_FROM_PSW(psw),
				 scm_from_int(real_x), scm_from_int(real_y),
				 SCM_UNDEFINED));

	/*DisplayPosition(psw, real_x, real_y, True);*/

        /* prevent window from lagging behind mouse when paging - mab */
	if (paged == 0) {
	  int xcenter = Event.xmotion.x_root;
	  int ycenter = Event.xmotion.y_root;
          int delta_x = 0;
          int delta_y = 0;

          GenerateEdgeEvents();
	  HandlePaging(Scr.DisplayWidth, Scr.DisplayHeight, &xcenter, &ycenter,
		       &delta_x, &delta_y, False);
	  if ((delta_x != 0) || (delta_y == 0)) {
            xl += delta_x;
            yt += delta_y;
          }
	}
	paged++;
      } /* while (paged <= 1) */

      done = True;
      break;

    default:
      break;
    } /* switch (Event.type) */
    if (!done) {
      DispatchEvent();
    }
  } /* while (!finished) */

  if (!opaque_move)
    RemoveRubberbandOutline();

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
    
#ifdef SCWM_DEBUG_MOVE_LOOP
  fprintf(stderr,"return finalX/Y %d,%d, frameXY %d,%d\n",
          WIN_VP_OFFSET_X(psw)+xl, WIN_VP_OFFSET_Y(psw) + yt,
          FRAME_X_VP(psw),FRAME_Y_VP(psw));
#endif
  *FinalX = WIN_VP_OFFSET_X(psw) + xl;
  *FinalY = WIN_VP_OFFSET_Y(psw) + yt;
}


/*
 * For menus, move, and resize operations, we can effect keyboard 
 * shortcuts by warping the pointer.
 * This also gets used by DeferExecution for selecting windows
 * interactively (it probably should not be used for that. GJB:FIXME::)
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
  case XK_KP_8:
  case XK_k:
  case XK_p:
    if (fResize && !fOnYBoundary) y_move = -y - ymove_size;
    else y_move = -ymove_size;
    break;
  case XK_Down:
  case XK_KP_2:
  case XK_n:
  case XK_j:
    if (fResize && !fOnYBoundary) y_move = psw->frame_height - y + ymove_size;
    else y_move = ymove_size;
    break;
  case XK_Left:
  case XK_KP_4:
  case XK_b:
  case XK_h:
    if (fResize && !fOnXBoundary) x_move = -x - xmove_size;
    else x_move = -xmove_size;
    break;
  case XK_Right:
  case XK_KP_6:
  case XK_f:
  case XK_l:
    if (fResize && !fOnXBoundary) x_move = psw->frame_width - x + xmove_size;
    else x_move = xmove_size;
    break;
  case XK_KP_1:
    x_move = -xmove_size;
    y_move = ymove_size;
    break;
  case XK_KP_3:
    x_move = xmove_size;
    y_move = ymove_size;
    break;
  case XK_KP_7:
    x_move = -xmove_size;
    y_move = -ymove_size;
    break;
  case XK_KP_9:
    x_move = xmove_size;
    y_move = -ymove_size;
    break;
     
  case XK_Return:
  case XK_KP_Enter:
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

/* return either the frame or the icon window,
   based on whether psw->fIconified */
Window
WFrameOrIcon(ScwmWindow *psw)
{
  Window w = psw->frame;
  if (psw->fIconified) {
    if (psw->icon_pixmap_w != None)
      w = psw->icon_pixmap_w;
    else
      w = psw->icon_w;
  }

  return w;
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
  unsigned int border_width;
  int XOffset, YOffset;
  Window w = psw->frame;
  /* GJB:FIXME:: pass these in instead of them being globals! */
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

  /* reset the global so it only is set if explicitly
     set before calling -- still a hack! GJB:FIXME:: --09/24/98 gjb */
  have_orig_position = False;
  InstallRootColormap();

  if (!GrabEm(XCURSOR_MOVE)) {
    /* GJB:FIXME:: xmag caused this to run
       when click-to place (no auto/smart placement)
       and it should not, IMO --09/22/98 gjb
       scwm_run_hook0(invalid_interaction_hook);
    */
    return False;
  }

  w = WFrameOrIcon(psw);
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

  scwm_run_hook1(interactive_move_start_hook, SCM_FROM_PSW(psw));
  moveLoop(psw, XOffset, YOffset, DragWidth, DragHeight, FinalX, FinalY, fOpaque);
  /* same hook is called above, before the iterations begin, and during the iterations */
  scwm_run_hook(interactive_move_new_position_hook, 
                scm_list_n(SCM_FROM_PSW(psw),
			   scm_from_int(FRAME_X_VP(psw)), scm_from_int(FRAME_Y_VP(psw)),
			   SCM_UNDEFINED));
  scwm_run_hook1(interactive_move_finish_hook, SCM_FROM_PSW(psw));

  if (psw->fIconified) {
    psw->fIconMoved = True;
  }

  UninstallRootColormap();

  if (!fOpaque)
    XUngrabServer_withSemaphore(dpy);

  UngrabEm();
  return True;
}


SCM_DEFINE(rubber_band_move, "rubber-band-move", 0, 1, 0,
           (SCM win),
"Move WIN interactively, using a rubber band frame.\n\
Returns a list '(X Y) which is the new viewport position of WIN.\n\
This allows the user to drag a rubber band frame around the\n\
screen. WIN defaults to the window context in the usual way if not\n\
specified.")
#define FUNC_NAME s_rubber_band_move
{
  int x, y;                     /* not used now */

  VALIDATE_PRESS_ONLY(win);
  InteractiveMove(PSWFROMSCMWIN(win), False, &x, &y);

  return scm_list_n(scm_from_int(x),scm_from_int(y),SCM_UNDEFINED);
}
#undef FUNC_NAME


SCM_DEFINE(opaque_move, "opaque-move", 0, 1, 0,
           (SCM win),
"Move WIN interactively, opaquely.\n\
Returns a list '(X Y) which is the new viewport position of WIN.\n\
This allows the user to drag the window itself around the screen. WIN\n\
defaults to the window context in the usual way if not specified.")
#define FUNC_NAME s_opaque_move
{
  int x, y;

  VALIDATE_PRESS_ONLY(win);
  InteractiveMove(PSWFROMSCMWIN(win), True, &x, &y);

  return scm_list_n(scm_from_int(x),scm_from_int(y),SCM_UNDEFINED);
}
#undef FUNC_NAME


void 
init_move()
{
#include "move.x"
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

