/* $Id$ 
 * virtual.c
 *
 * Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
 *
 * Code is derived from Robert Nation's fvwm2 (twm, ctwm derivative)
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <assert.h>

#include "virtual.h"

#include "scwm.h"
#include "util.h"
#include "window.h"
#include "icons.h"
#include "screen.h"
#include "Grab.h"
#include "resize.h"
#include "borders.h"
#include "focus.h"
#include "module-interface.h"
#include "xmisc.h"
#include "syscompat.h"
#include "callbacks.h"
#include "cursor.h"
#include "events.h"

SCWM_SYMBOL(sym_north,"north");
SCWM_SYMBOL(sym_east,"east");
SCWM_SYMBOL(sym_south,"south");
SCWM_SYMBOL(sym_west,"west");

#define SCROLL_REGION (gh_scm2int(*pscm_scroll_region))

SCWM_HOOK(change_desk_hook,"change-desk-hook", 2,
"This hook is invoked whenever the current desktop is changed.\n\
It is called with two argument, both integers.  The first is the\n\
new desktop number, the second is the old desktop number. It is\n\
called before the desk is changed.  See also `after-change-desk-hook'.");

SCWM_HOOK(after_change_desk_hook,"after-change-desk-hook", 2,
"This hook is invoked just after the current desktop is changed.\n\
It is called with two argument, both integers.  The first is the\n\
new desktop number, the second is the old desktop number.  It\n\
is called after the desk is changed. See also `change-desk-hook'.");

SCWM_HOOK(viewport_position_change_hook,"viewport-position-change-hook", 4,
"This hook is invoked whenever the viewport position is changed.\n\
It is called with four arguments, all integers.  The first two are \n\
the x and y coordinates of the new viewport position in pixels and\n\
the second two are the change in x and y from the previous position.");

SCWM_HOOK(after_viewport_position_change_hook,"after-viewport-position-change-hook", 4,
"This hook is invoked just after the viewport position is changed.\n\
It is called with four arguments, all integers.  The first two are \n\
the x and y coordinates of the new viewport position in pixels and\n\
the second two are the change in x and y from the previous position.");

SCWM_HOOK(edge_enter_hook,"edge-enter-hook", 1,
"This hook is invoked whenever the mouse pointer enters a screen edge.\n\
Procedures in the hook are called with one argument, one of the\n\
symbols 'north, 'south, 'east or 'west indicating which edge was\n\
entered.");

SCWM_HOOK(edge_leave_hook,"edge-leave-hook", 1,
"This hook is invoked whenever the mouse pointer leaves a screen edge.\n\
Procedures in the hook are called with one argument, one of the\n\
symbols 'north, 'south, 'east or 'west indicating which edge was\n\
entered.");

SCWM_HOOK(edge_scroll_hook,"edge-scroll-hook", 0,
"This hook is invoked whenever an edge scroll takes place.\n\
Procedures in the hook are called with no arguments.");


static Edge in_edge = EDGE_NONE;

static SCM *pscm_scroll_region;

int CpixScrollRegion()
{
  return gh_scm2int(*pscm_scroll_region);
}


static void 
notify_edge_enter(Edge e)
{
  SCM edge_sym = SCM_BOOL_F;

  switch (e) {
  case EDGE_TOP:
    edge_sym = sym_north;
    break;
  case EDGE_LEFT:
    edge_sym = sym_west;
    break;
  case EDGE_RIGHT:
    edge_sym = sym_east;
    break;
  case EDGE_BOTTOM:
    edge_sym = sym_south;
    break;
  case EDGE_NONE:
  default:
    /* this should not happen */
    assert(False);
    break;
  }

  scwm_run_hook1(edge_enter_hook, edge_sym);
  in_edge = e;
}

static void 
notify_edge_leave(Edge e)
{
  SCM edge_sym = SCM_BOOL_F;

  switch (e) {
  case EDGE_TOP:
    edge_sym = sym_north;
    break;
  case EDGE_LEFT:
    edge_sym = sym_west;
    break;
  case EDGE_RIGHT:
    edge_sym = sym_east;
    break;
  case EDGE_BOTTOM:
    edge_sym = sym_south;
    break;
  case EDGE_NONE:
  default:
    /* this should not happen */
    assert(False);
    break;
  }

  scwm_run_hook1(edge_leave_hook, edge_sym);
  in_edge = EDGE_NONE;
}


void 
GenerateEdgeEvents()
{
  Edge new_in_edge;
  int xl, yt;
  const int cpix_scroll_region = CpixScrollRegion();

  WXGetPointerWindowOffsets(Scr.Root, &xl, &yt);

  if (yt <= cpix_scroll_region) {
    new_in_edge = EDGE_TOP;
  } else if (yt > Scr.DisplayHeight - cpix_scroll_region) {
    new_in_edge = EDGE_BOTTOM;
  } else if (xl <= cpix_scroll_region) {
    new_in_edge = EDGE_LEFT;
  } else if (xl > Scr.DisplayWidth - cpix_scroll_region) {
    new_in_edge = EDGE_RIGHT;
  } else {
    new_in_edge = EDGE_NONE;
  }

  if (new_in_edge != in_edge) {
    if (in_edge != EDGE_NONE) {
      notify_edge_leave(in_edge);
    }

    if (new_in_edge != EDGE_NONE) {
      notify_edge_enter(new_in_edge);
    }
  }
}


Bool
FNeedsPaging(int HorWarpSize, int VertWarpSize, int xl, int yt)
{
  int x, y;
  const int cpix_scroll_region = CpixScrollRegion();

  if ((Scr.ScrollResistance < 0) ||
      ((HorWarpSize == 0) && (VertWarpSize == 0))) {
    return False;
  }

  /* need to move the viewport */
  if ((Scr.VxMax == 0 ||
       (xl >= cpix_scroll_region && 
        xl < Scr.DisplayWidth - cpix_scroll_region)) &&
      (Scr.VyMax == 0 ||
       (yt >= cpix_scroll_region && 
        yt < Scr.DisplayHeight - cpix_scroll_region))) {
    return False;
  }

  WXGetPointerWindowOffsets(Scr.Root, &x, &y);
  
  /* check actual pointer location since PanFrames can get buried under
     a window being moved or resized - mab */
  if ((x >= cpix_scroll_region) && 
      (x < Scr.DisplayWidth - cpix_scroll_region) &&
      (y >= cpix_scroll_region) && 
      (y < Scr.DisplayHeight - cpix_scroll_region)) {
    GenerateEdgeEvents(Event.xcrossing.x_root, Event.xcrossing.y_root);
    return False;
  }

  return True;
}



/***************************************************************************
 * 
 * Check to see if the pointer is on the edge of the screen, and scroll/page
 * if needed 
 ***************************************************************************/
void 
HandlePaging(int HorWarpSize, int VertWarpSize, int *xl, int *yt,
	     int *delta_x, int *delta_y, Bool Grab)
{
  int x, y, total;
  const int cpix_scroll_region = CpixScrollRegion();

  *delta_x = 0;
  *delta_y = 0;

  total = 0;

  if (!FNeedsPaging(HorWarpSize, VertWarpSize, *xl, *yt)) {
    return;
  }

  while (total < Scr.ScrollResistance) {
    ms_sleep(10);
    total += 10;

    WXGetPointerWindowOffsets(Scr.Root, &x, &y);

    if (XCheckWindowEvent(dpy, Scr.PanFrameTop.win,
			  LeaveWindowMask, &Event)) {
      StashEventTime(&Event);
      GenerateEdgeEvents(Event.xcrossing.x_root, Event.xcrossing.y_root);
      return;
    }
    if (XCheckWindowEvent(dpy, Scr.PanFrameBottom.win,
			  LeaveWindowMask, &Event)) {
      StashEventTime(&Event);
      GenerateEdgeEvents(Event.xcrossing.x_root, Event.xcrossing.y_root);
      return;
    }
    if (XCheckWindowEvent(dpy, Scr.PanFrameLeft.win,
			  LeaveWindowMask, &Event)) {
      StashEventTime(&Event);
      GenerateEdgeEvents(Event.xcrossing.x_root, Event.xcrossing.y_root);
      return;
    }
    if (XCheckWindowEvent(dpy, Scr.PanFrameRight.win,
			  LeaveWindowMask, &Event)) {
      StashEventTime(&Event);
      GenerateEdgeEvents(Event.xcrossing.x_root, Event.xcrossing.y_root);
      return;
    }
    /* check actual pointer location since PanFrames can get buried under
       a window being moved or resized - mab */
    if ((x >= cpix_scroll_region) &&
        (x < Scr.DisplayWidth - cpix_scroll_region) &&
	(y >= cpix_scroll_region) && 
        (y < Scr.DisplayHeight - cpix_scroll_region)) {
      GenerateEdgeEvents(Event.xcrossing.x_root, Event.xcrossing.y_root);
      return;
    }
  }

  WXGetPointerWindowOffsets(Scr.Root, &x, &y);

  RemoveRubberbandOutline();

  /* Move the viewport */
  /* and/or move the cursor back to the approximate correct location */
  /* that is, the same place on the virtual desktop that it */
  /* started at */
  if (x < cpix_scroll_region)
    *delta_x = -HorWarpSize;
  else if (x >= Scr.DisplayWidth - cpix_scroll_region)
    *delta_x = HorWarpSize;
  else
    *delta_x = 0;
  if (Scr.VxMax == 0)
    *delta_x = 0;
  if (y < cpix_scroll_region)
    *delta_y = -VertWarpSize;
  else if (y >= Scr.DisplayHeight - cpix_scroll_region)
    *delta_y = VertWarpSize;
  else
    *delta_y = 0;
  if (Scr.VyMax == 0)
    *delta_y = 0;

  /* Ouch! lots of bounds checking */
  if (Scr.Vx + *delta_x < 0) {
    if (!Scr.fEdgeWrapX) {
      *delta_x = -Scr.Vx;
      *xl = x - *delta_x;
    } else {
      *delta_x += Scr.VxMax + Scr.DisplayWidth;
      *xl = x + *delta_x % Scr.DisplayWidth + HorWarpSize;
    }
  } else if (Scr.Vx + *delta_x > Scr.VxMax) {
    if (!Scr.fEdgeWrapX) {
      *delta_x = Scr.VxMax - Scr.Vx;
      *xl = x - *delta_x;
    } else {
      *delta_x -= Scr.VxMax + Scr.DisplayWidth;
      *xl = x + *delta_x % Scr.DisplayWidth - HorWarpSize;
    }
  } else {
    *xl = x - *delta_x;
  }

  if (Scr.Vy + *delta_y < 0) {
    if (!Scr.fEdgeWrapY) {
      *delta_y = -Scr.Vy;
      *yt = y - *delta_y;
    } else {
      *delta_y += Scr.VyMax + Scr.DisplayHeight;
      *yt = y + *delta_y % Scr.DisplayHeight + VertWarpSize;
    }
  } else if (Scr.Vy + *delta_y > Scr.VyMax) {
    if (!Scr.fEdgeWrapY) {
      *delta_y = Scr.VyMax - Scr.Vy;
      *yt = y - *delta_y;
    } else {
      *delta_y -= Scr.VyMax + Scr.DisplayHeight;
      *yt = y + *delta_y % Scr.DisplayHeight - VertWarpSize;
    }
  } else {
    *yt = y - *delta_y;
  }

  if (*xl <= cpix_scroll_region)
    *xl = cpix_scroll_region + 1;
  if (*yt <= cpix_scroll_region)
    *yt = cpix_scroll_region + 1;
  if (*xl >= Scr.DisplayWidth - cpix_scroll_region)
    *xl = Scr.DisplayWidth - cpix_scroll_region - 1;
  if (*yt >= Scr.DisplayHeight - cpix_scroll_region)
    *yt = Scr.DisplayHeight - cpix_scroll_region - 1;

  if ((*delta_x != 0) || (*delta_y != 0)) {
    if (Grab)
      XGrabServer_withSemaphore(dpy);
    XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, *xl, *yt);
    MoveViewport(Scr.Vx + *delta_x, Scr.Vy + *delta_y);
    WXGetPointerWindowOffsets(Scr.Root, xl, yt);
    if (Grab)
      XUngrabServer_withSemaphore(dpy);
    scwm_run_hook0(edge_scroll_hook);
  }
}



/* the root window is surrounded by four window slices, which are InputOnly.
 * So you can see 'through' them, but they eat the input. An EnterEvent in
 * one of these windows causes a Paging. The windows have the according cursor
 * pointing in the pan direction or are hidden if there is no more panning
 * in that direction. This is mostly intended to get a panning even atop
 * of Motif applictions, which does not work yet. It seems Motif windows
 * eat all mouse events.
 *
 * Hermann Dunkel, HEDU, dunkel@cul-ipn.uni-kiel.de 1/94
 */

/*
 * checkPanFrames hides PanFrames if they are on the very border of the
 * VIRTUAL screen and EdgeWrap for that direction is off. 
 */
void 
checkPanFrames()
{
  if (!(Scr.fWindowsCaptured))
    return;
  
  if (Scr.fEdgeWrapX || Scr.EdgeScrollX != 0) {
    XMapRaised(dpy, Scr.PanFrameLeft.win);
    XMapRaised(dpy, Scr.PanFrameRight.win);
    Scr.PanFrameLeft.isMapped = 
      Scr.PanFrameRight.isMapped = True;
  } else {
    XUnmapWindow(dpy, Scr.PanFrameLeft.win);
    XUnmapWindow(dpy, Scr.PanFrameRight.win);
    Scr.PanFrameLeft.isMapped = 
      Scr.PanFrameRight.isMapped = False;
  }

  if (Scr.fEdgeWrapY || Scr.EdgeScrollY != 0) {
    XMapRaised(dpy, Scr.PanFrameTop.win);
    XMapRaised(dpy, Scr.PanFrameBottom.win);
    Scr.PanFrameTop.isMapped =
      Scr.PanFrameBottom.isMapped = True;
  } else {
    XUnmapWindow(dpy, Scr.PanFrameTop.win);
    XUnmapWindow(dpy, Scr.PanFrameBottom.win);
    Scr.PanFrameTop.isMapped =
      Scr.PanFrameBottom.isMapped = False;
  }
}

/*
 * Gotta make sure these things are on top of everything else, or they
 * don't work!
 */
void 
raisePanFrames()
{
  if (Scr.PanFrameTop.isMapped)
    XRaiseWindow(dpy, Scr.PanFrameTop.win);
  if (Scr.PanFrameLeft.isMapped)
    XRaiseWindow(dpy, Scr.PanFrameLeft.win);
  if (Scr.PanFrameRight.isMapped)
    XRaiseWindow(dpy, Scr.PanFrameRight.win);
  if (Scr.PanFrameBottom.isMapped)
    XRaiseWindow(dpy, Scr.PanFrameBottom.win);
}

/****************************************************************************
 *
 * Creates the windows for edge-scrolling 
 *
 ****************************************************************************/
void 
initPanFrames()
{
  XSetWindowAttributes attributes;	/* attributes for create */
  unsigned long valuemask;

  attributes.event_mask = (EnterWindowMask | LeaveWindowMask |
			   VisibilityChangeMask);
  valuemask = (CWEventMask | CWCursor);

  attributes.cursor = XCursorByNumber(XC_top_side);
  Scr.PanFrameTop.win =
    XCreateWindow(dpy, Scr.Root,
		  0, 0,
		  Scr.DisplayWidth, PAN_FRAME_THICKNESS,
		  0,		/* no border */
		  CopyFromParent, InputOnly,
		  CopyFromParent,
		  valuemask, &attributes);
  attributes.cursor = XCursorByNumber(XC_left_side);
  Scr.PanFrameLeft.win =
    XCreateWindow(dpy, Scr.Root,
		  0, PAN_FRAME_THICKNESS,
		  PAN_FRAME_THICKNESS,
		  Scr.DisplayHeight - 2 * PAN_FRAME_THICKNESS,
		  0,		/* no border */
		  CopyFromParent, InputOnly, CopyFromParent,
		  valuemask, &attributes);
  attributes.cursor = XCursorByNumber(XC_right_side);
  Scr.PanFrameRight.win =
    XCreateWindow(dpy, Scr.Root,
	      Scr.DisplayWidth - PAN_FRAME_THICKNESS, PAN_FRAME_THICKNESS,
		  PAN_FRAME_THICKNESS,
		  Scr.DisplayHeight - 2 * PAN_FRAME_THICKNESS,
		  0,		/* no border */
		  CopyFromParent, InputOnly, CopyFromParent,
		  valuemask, &attributes);
  attributes.cursor = XCursorByNumber(XC_bottom_side);
  Scr.PanFrameBottom.win =
    XCreateWindow(dpy, Scr.Root,
		  0, Scr.DisplayHeight - PAN_FRAME_THICKNESS,
		  Scr.DisplayWidth, PAN_FRAME_THICKNESS,
		  0,		/* no border */
		  CopyFromParent, InputOnly, CopyFromParent,
		  valuemask, &attributes);
  Scr.PanFrameTop.isMapped = Scr.PanFrameLeft.isMapped =
    Scr.PanFrameRight.isMapped = Scr.PanFrameBottom.isMapped = False;
}

int fInMoveViewport_internal = False;

/*
 *  Moves the viewport within the virtual desktop
 */
void 
MoveViewport_internal(int newx, int newy)
{
  ScwmWindow *psw;
  int diffx, diffy;

  fInMoveViewport_internal = True;

  if (newx < 0)
    newx = 0;
  else if (newx > Scr.VxMax)
    newx = Scr.VxMax;
  if (newy < 0)
    newy = 0;
  else if (newy > Scr.VyMax)
    newy = Scr.VyMax;

  /* no change? then do nothing */
  if (newx == Scr.Vx && newy == Scr.Vy)
    return;

  diffx = newx - Scr.Vx;
  diffy = newy - Scr.Vy;
  Scr.Vx = newx;
  Scr.Vy = newy;

  /* These properties are used upon restart to move windows back
     to their appropriate place since the current viewport is kept
     visible when Scwm is shut down.  (Making all the windows above and to
     the left the current viewport have negative positions). */
  XChangeProperty(dpy, Scr.Root, XA_SCWM_VIEWPORT_OFFSET_X,
		  XA_CARDINAL, 32, PropModeReplace, (unsigned char *) &Scr.Vx, 1);
  XChangeProperty(dpy, Scr.Root, XA_SCWM_VIEWPORT_OFFSET_Y,
		  XA_CARDINAL, 32, PropModeReplace, (unsigned char *) &Scr.Vy, 1);

  scwm_run_hook(viewport_position_change_hook, 
                gh_list(gh_int2scm(Scr.Vx), gh_int2scm(Scr.Vy),
                        gh_int2scm(diffx), gh_int2scm(diffy),
                        SCM_UNDEFINED)); 

  Broadcast(M_NEW_PAGE, 5, Scr.Vx, Scr.Vy, Scr.CurrentDesk, Scr.VxMax, Scr.VyMax, 0, 0);

  for (psw = Scr.ScwmRoot.next; psw != NULL; psw = psw->next) {
    /* do not bother with moving windows not on the current desk */
    if (psw->Desk == Scr.CurrentDesk) {
      if (psw->fIconified)
        MovePswIconToCurrentPosition(psw);
      else
        MovePswToCurrentPosition(psw);
    } else {
      BroadcastConfig(M_CONFIGURE_WINDOW, psw);
      if (psw->fIconified)
        BroadcastIconInfo(M_ICON_LOCATION, psw);
    }
  }

  XSync(dpy,False);
  while (XCheckMaskEvent(dpy, VisibilityChangeMask,
                         &Event)) {
    DispatchEvent();
  }

  scwm_run_hook(after_viewport_position_change_hook, 
                gh_list(gh_int2scm(Scr.Vx), gh_int2scm(Scr.Vy),
                        gh_int2scm(diffx), gh_int2scm(diffy),
                        SCM_UNDEFINED)); 

  checkPanFrames();
  fInMoveViewport_internal = False;
}


void 
MoveViewport(int newx, int newy)
{
  /* ChangeVirtualPosition is different for
     constraint-enabled vs. not-constraint-enabled
     versions of Scwm */
  ChangeVirtualPosition(newx,newy);
}


void 
changeDesks(int val1, int val2)
{
  int oldDesk;
  ScwmWindow *FocusWin = 0, *psw;
  static ScwmWindow *StickyWin = 0;

  oldDesk = Scr.CurrentDesk;

  if (val1 != 0) {
    Scr.CurrentDesk = Scr.CurrentDesk + val1;
  } else {
    Scr.CurrentDesk = val2;
    if (Scr.CurrentDesk == oldDesk)
      return;
  }

  scwm_run_hook2(change_desk_hook, 
                gh_int2scm(Scr.CurrentDesk), gh_int2scm(oldDesk));
  
  Broadcast(M_NEW_DESK, 1, Scr.CurrentDesk, 0, 0, 0, 0, 0, 0);
  /* Scan the window list, mapping windows on the new Desk,
   * unmapping windows on the old Desk */
  XGrabServer_withSemaphore(dpy);
  for (psw = Scr.ScwmRoot.next; psw != NULL; psw = psw->next) {
    /* Only change mapping for non-sticky windows */
    if (!(psw->fIconified && psw->fStickyIcon) &&
	!psw->fSticky && !psw->fIconUnmapped) {
      if (psw->Desk == oldDesk) {
	if (Scr.Focus == psw)
	  psw->FocusDesk = oldDesk;
	else
	  psw->FocusDesk = -1;
	UnmapScwmWindow(psw);
      } else if (psw->Desk == Scr.CurrentDesk) {
        if (psw->fIconified)
          MovePswIconToCurrentPosition(psw);
        else
          MovePswToCurrentPosition(psw);
	MapIt(psw);
	if (psw->FocusDesk == Scr.CurrentDesk) {
	  FocusWin = psw;
	}
      }
    } else {
      /* Window is sticky */
      psw->Desk = Scr.CurrentDesk;

      notify_new_desk(psw, val1, oldDesk);

      if (Scr.Focus == psw) {
	psw->FocusDesk = oldDesk;
	StickyWin = psw;
      }
    }
  }
  XUngrabServer_withSemaphore(dpy);
  for (psw = Scr.ScwmRoot.next; psw != NULL; psw = psw->next) {
    /* If its an icon, and its sticking, autoplace it so
       that it doesn't wind up on top of a stationary icon */
    if ((psw->fSticky || psw->fStickyIcon) &&
	psw->fIconified && !psw->fIconMoved && 
  	!psw->fIconUnmapped) {
      AutoPlace(psw);
    }
  }

  if (FocusWin && FocusWin->fClickToFocus) {
    SetFocus(FocusWin->w, FocusWin, False);
  } else {
    SetFocus(Scr.NoFocusWin, NULL, True);
  }

  /* be sure the correct window gets focus
     if we are in focus-follow-mouse mode */
  CoerceEnterNotifyOnCurrentWindow();

  scwm_run_hook2(after_change_desk_hook, 
                 gh_int2scm(Scr.CurrentDesk), gh_int2scm(oldDesk));
}


void
init_virtual()
{
  SCWM_VAR_INIT(scroll_region, "scroll-region", gh_int2scm(2));
  /** The number of pixels at the edge of the screen within which virtual scrolling will occur. */

#ifndef SCM_MAGIC_SNARFER
#include "virtual.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */
