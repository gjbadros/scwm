/* $Id$
 * placement.c
 * (C) 1998 Maciej Stachowiak and Greg J. Badros
 */
 
/****************************************************************************
 * This module is derived from code by by Rob Nation 
 *
 * This code does smart-placement initial window placement stuff
 *
 * Copyright 1994 Robert Nation. No restrictions are placed on this code,
 * as long as the copyright notice is preserved . No guarantees or
 * warrantees of any sort whatsoever are given or implied or anything.
 ****************************************************************************/

#include <config.h>

#include <stdio.h>
#include <unistd.h>
#include <string.h>

#define PLACEMENT_IMPLEMENTATION
#include "placement.h"

#include "scwm.h"
#include "misc.h"
#include "move.h"
#include "screen.h"
#include "Grab.h"
#include "icons.h"
#include "virtual.h"
#include "callbacks.h"

int get_next_x(ScwmWindow * t, int x, int y);
int get_next_y(ScwmWindow * t, int y);
int test_fit(ScwmWindow * t, int test_x, int test_y, int aoimin);
void CleverPlacement(ScwmWindow * t, int *x, int *y);


/************************************************************************
 *
 *  Procedure:
 *	GetGravityOffsets - map gravity to (x,y) offset signs for adding
 *		to x and y when window is mapped to get proper placement.
 * 
 ************************************************************************/
struct _gravity_offset {
  int x, y;
};

static
void 
GetGravityOffsets(ScwmWindow * tmp, int *xp, int *yp)
{
  static struct _gravity_offset gravity_offsets[11] =
  {
    {0, 0},			/* ForgetGravity */
    {-1, -1},			/* NorthWestGravity */
    {0, -1},			/* NorthGravity */
    {1, -1},			/* NorthEastGravity */
    {-1, 0},			/* WestGravity */
    {0, 0},			/* CenterGravity */
    {1, 0},			/* EastGravity */
    {-1, 1},			/* SouthWestGravity */
    {0, 1},			/* SouthGravity */
    {1, 1},			/* SouthEastGravity */
    {0, 0},			/* StaticGravity */
  };
  register int g = ((tmp->hints.flags & PWinGravity)
		    ? tmp->hints.win_gravity : NorthWestGravity);

  if (g < ForgetGravity || g > StaticGravity) {
    *xp = *yp = 0;
  } else {
    *xp = (int) gravity_offsets[g].x;
    *yp = (int) gravity_offsets[g].y;
  }
  return;
}


/* The following factors represent the amount of area that these types of
 * windows are counted as.  For example, by default the area of fOnTop windows
 * is counted 5 times as much as normal windows.  So CleverPlacement will
 * cover 5 times as much area of another window before it will cover an fOnTop
 * window.  To treat fOnTop windows the same as other windows, set this to 1.
 * To really, really avoid putting windows under fOnTop windows, set this to a
 * high value, say 1000.  A value of 5 will try to avoid fOnTop windows if
 * practical, but if it saves a reasonable amount of area elsewhere, it will
 * place one there.  The same rules apply for the other "AVOID" factors.
 * (for CleverPlacement)
 */
#define AVOIDONTOP 5
#define AVOIDSTICKY 1
#ifdef NO_STUBBORN_PLACEMENT
#define AVOIDICON 0		/*  Ignore Icons.  Place windows over them  */
#else
#define AVOIDICON 10		/*  Try hard no to place windows over icons */
#endif

static
void 
SmartPlacement(ScwmWindow *psw, int width, int height, int *x, int *y)
{
  int temp_h, temp_w;
  int test_x = 0, test_y = 0;
  int loc_ok = False, tw, tx, ty, th;
  ScwmWindow *pswTest;

  if (Scr.SmartPlacementIsClever) {	/* call clever placement instead? */
    CleverPlacement(psw, x, y);
    return;
  }
  temp_h = height;
  temp_w = width;

  while (((test_y + temp_h) < (Scr.MyDisplayHeight)) && (!loc_ok)) {
    test_x = 0;
    while (((test_x + temp_w) < (Scr.MyDisplayWidth)) && (!loc_ok)) {
      loc_ok = True;
      pswTest = Scr.ScwmRoot.next;
      while ((pswTest != (ScwmWindow *) 0) && (loc_ok == True)) {
	if (pswTest->Desk == Scr.CurrentDesk) {
#ifndef NO_STUBBORN_PLACEMENT
	  if (pswTest->fIconified &&
	      !pswTest->fIconUnmapped &&
	      pswTest->icon_w &&
	      pswTest != psw) {
	    tw = pswTest->icon_p_width;
	    th = pswTest->icon_p_height + pswTest->icon_w_height;
	    tx = pswTest->icon_x_loc;
	    ty = pswTest->icon_y_loc;

	    if ((tx < (test_x + width)) && ((tx + tw) > test_x) &&
		(ty < (test_y + height)) && ((ty + th) > test_y)) {
	      loc_ok = False;
	      test_x = tx + tw;
	    }
	  }
#endif /* !NO_STUBBORN_PLACEMENT */
	  if (!pswTest->fIconified && (pswTest != psw)) {
	    tw = FRAME_WIDTH(pswTest) + 2 * pswTest->bw;
	    th = FRAME_HEIGHT(pswTest) + 2 * pswTest->bw;
	    tx = FRAME_X(pswTest);
	    ty = FRAME_Y(pswTest);
	    if ((tx <= (test_x + width)) && ((tx + tw) >= test_x) &&
		(ty <= (test_y + height)) && ((ty + th) >= test_y)) {
	      loc_ok = False;
	      test_x = tx + tw;
	    }
	  }
	}
	pswTest = pswTest->next;
      }
      test_x += 1;
    }
    test_y += 1;
  }
  if (loc_ok == False) {
    *x = -1;
    *y = -1;
    return;
  }
  *x = test_x;
  *y = test_y;
}


/* CleverPlacement by Anthony Martin <amartin@engr.csulb.edu>
 * This function will place a new window such that there is a minimum amount
 * of interference with other windows.  If it can place a window without any
 * interference, fine.  Otherwise, it places it so that the area of of
 * interference between the new window and the other windows is minimized */
void 
CleverPlacement(ScwmWindow *psw, int *x, int *y)
{
  int test_x = 0, test_y = 0;
  int xbest, ybest;
  int aoi, aoimin;		/* area of interference */

  aoi = aoimin = test_fit(psw, test_x, test_y, -1);
  xbest = test_x;
  ybest = test_y;

  while ((aoi != 0) && (aoi != -1)) {
    if (aoi > 0) {		/* Windows interfere.  Try next x. */
      test_x = get_next_x(psw, test_x, test_y);
    } else {			/* Out of room in x direction. Try next y. Reset x. */
      test_x = 0;
      test_y = get_next_y(psw, test_y);
    }
    aoi = test_fit(psw, test_x, test_y, aoimin);
    if ((aoi >= 0) && (aoi < aoimin)) {
      xbest = test_x;
      ybest = test_y;
      aoimin = aoi;
    }
  }
  *x = xbest;
  *y = ybest;
}

int 
get_next_x(ScwmWindow *psw, int x, int y)
{
  int xnew;
  int xtest;
  ScwmWindow *testw;

  /* Test window at far right of screen */
  xnew = Scr.MyDisplayWidth;
  xtest = Scr.MyDisplayWidth - (FRAME_WIDTH(psw) + 2 * psw->bw);
  if (xtest > x)
    xnew = min(xnew, xtest);
  /* Test the values of the right edges of every window */
  for (testw = Scr.ScwmRoot.next; testw != NULL; testw = testw->next) {
    if ((testw->Desk != Scr.CurrentDesk) || (testw == psw))
      continue;
    if (testw->fIconified) {
      if ((y < testw->icon_p_height + testw->icon_w_height + testw->icon_y_loc) &&
	  (testw->icon_y_loc < (FRAME_HEIGHT(psw) + 2 * psw->bw + y))) {
	xtest = testw->icon_p_width + testw->icon_x_loc;
	if (xtest > x)
	  xnew = min(xnew, xtest);
	xtest = testw->icon_x_loc - (FRAME_WIDTH(psw) + 2 * psw->bw);
	if (xtest > x)
	  xnew = min(xnew, xtest);
      }
    } else if ((y < (FRAME_HEIGHT(testw) + 2 * testw->bw + FRAME_Y(testw))) &&
	       (FRAME_Y(testw) < (FRAME_HEIGHT(psw) + 2 * psw->bw + y))) {
      xtest = FRAME_WIDTH(testw) + 2 * testw->bw + FRAME_X(testw);
      if (xtest > x)
	xnew = min(xnew, xtest);
      xtest = FRAME_X(testw) - (FRAME_WIDTH(psw) + 2 * psw->bw);
      if (xtest > x)
	xnew = min(xnew, xtest);
    }
  }
  return xnew;
}
int 
get_next_y(ScwmWindow * psw, int y)
{
  int ynew;
  int ytest;
  ScwmWindow *testw;

  /* Test window at far bottom of screen */
  ynew = Scr.MyDisplayHeight;
  ytest = Scr.MyDisplayHeight - (FRAME_HEIGHT(psw) + 2 * psw->bw);
  if (ytest > y)
    ynew = min(ynew, ytest);
  /* Test the values of the bottom edge of every window */
  for (testw = Scr.ScwmRoot.next; testw != NULL; testw = testw->next) {
    if ((testw->Desk != Scr.CurrentDesk) || (testw == psw))
      continue;
    if (testw->fIconified) {
      ytest = testw->icon_p_height + testw->icon_w_height + testw->icon_y_loc;
      if (ytest > y)
	ynew = min(ynew, ytest);
      ytest = testw->icon_y_loc - (FRAME_HEIGHT(psw) + 2 * psw->bw);
      if (ytest > y)
	ynew = min(ynew, ytest);
    } else {
      ytest = FRAME_HEIGHT(testw) + 2 * testw->bw + FRAME_Y(testw);
      if (ytest > y)
	ynew = min(ynew, ytest);
      ytest = FRAME_Y(testw) - (FRAME_HEIGHT(psw) + 2 * psw->bw);
      if (ytest > y)
	ynew = min(ynew, ytest);
    }
  }
  return ynew;
}

int 
test_fit(ScwmWindow * psw, int x11, int y11, int aoimin)
{
  ScwmWindow *testw;
  int x12, x21, x22;
  int y12, y21, y22;
  int xl, xr, yt, yb;		/* xleft, xright, ytop, ybottom */
  int aoi = 0;			/* area of interference */
  int anew;
  int avoidance_factor;

  x12 = x11 + FRAME_WIDTH(psw) + 2 * psw->bw;
  y12 = y11 + FRAME_HEIGHT(psw) + 2 * psw->bw;

  if (y12 > Scr.MyDisplayHeight)	/* No room in y direction */
    return -1;
  if (x12 > Scr.MyDisplayWidth)	/* No room in x direction */
    return -2;
  for (testw = Scr.ScwmRoot.next; testw != NULL; testw = testw->next) {
    if ((testw == psw) || (testw->Desk != Scr.CurrentDesk))
      continue;
    if ((testw->fIconified) &&
	(testw->icon_w)) {
      if (testw->fIconUnmapped)
	continue;
      x21 = testw->icon_x_loc;
      y21 = testw->icon_y_loc;
      x22 = x21 + testw->icon_p_width;
      y22 = y21 + testw->icon_p_height + testw->icon_w_height;
    } else {
      x21 = FRAME_X(testw);
      y21 = FRAME_Y(testw);
      x22 = x21 + FRAME_WIDTH(testw) + 2 * testw->bw;
      y22 = y21 + FRAME_HEIGHT(testw) + 2 * testw->bw;
    }
    if ((x11 < x22) && (x12 > x21) &&
	(y11 < y22) && (y12 > y21)) {
      /* Windows interfere */
      xl = max(x11, x21);
      xr = min(x12, x22);
      yt = max(y11, y21);
      yb = min(y12, y22);
      anew = (xr - xl) * (yb - yt);
      if (testw->fIconified)
	avoidance_factor = AVOIDICON;
      else if (testw->fOnTop)
	avoidance_factor = AVOIDONTOP;
      else if (testw->fSticky)
	avoidance_factor = AVOIDSTICKY;
      else
	avoidance_factor = 1;
      anew *= avoidance_factor;
      aoi += anew;
      if ((aoi > aoimin) && (aoimin != -1))
	return aoi;
    }
  }
  return aoi;
}


/**************************************************************************
 *
 * Handles initial placement and sizing of a new window
 * Returns False in the event of a lost window.
 *
 **************************************************************************/
Bool 
PlaceWindow(ScwmWindow *psw, int Desk)
{
  ScwmWindow *t;
  int xl = -1, yt, DragWidth, DragHeight;
  int gravx, gravy;		/* gravity signs for positioning */
  extern Bool PPosOverride;

  GetGravityOffsets(psw, &gravx, &gravy);


  /* Select a desk to put the window on (in list of priority):
   * 1. Sticky Windows stay on the current desk.
   * 2. Windows specified with StartsOnDesk go where specified
   * 3. Put it on the desk it was on before the restart.
   * 4. Transients go on the same desk as their parents.
   * 5. Window groups stay together (completely untested)
   */
  psw->Desk = Scr.CurrentDesk;
  if (psw->fSticky)
    psw->Desk = Scr.CurrentDesk;
  else if (psw->fStartsOnDesk) {
    psw->Desk = Desk;
  } else {
    Atom atype;
    int aformat;
    unsigned long nitems, bytes_remain;
    unsigned char *prop;

    if ((psw->wmhints) && (psw->wmhints->flags & WindowGroupHint) &&
	(psw->wmhints->window_group != None) &&
	(psw->wmhints->window_group != Scr.Root)) {
      /* Try to find the group leader or another window
       * in the group */
      for (t = Scr.ScwmRoot.next; t != NULL; t = t->next) {
	if ((t->w == psw->wmhints->window_group) ||
	    ((t->wmhints) && (t->wmhints->flags & WindowGroupHint) &&
	     (t->wmhints->window_group == psw->wmhints->window_group)))
	  psw->Desk = t->Desk;
      }
    }
    if (psw->fTransient && (psw->transientfor != None) &&
	(psw->transientfor != Scr.Root)) {
      /* Try to find the parent's desktop */
      for (t = Scr.ScwmRoot.next; t != NULL; t = t->next) {
	if (t->w == psw->transientfor)
	  psw->Desk = t->Desk;
      }
    }
    if ((XGetWindowProperty(dpy, psw->w, _XA_WM_DESKTOP, 0L, 1L, True,
			    _XA_WM_DESKTOP, &atype, &aformat, &nitems,
			    &bytes_remain, &prop)) == Success) {
      if (prop != NULL) {
	psw->Desk = *(unsigned long *) prop;
	XFree(prop);
      }
    }
  }
  /* I think it would be good to switch to the selected desk
   * whenever a new window pops up, except during initialization */  
  /* FIXGJB: this should be a callback, not a forced switch to the new
     desk --03/26/98 gjb */
  if ((!PPosOverride) && (!(psw->fShowOnMap)))
    changeDesks(0, psw->Desk);


  /* Desk has been selected, now pick a location for the window */
  /*
   *  If
   *     o  the window is a transient, or
   * 
   *     o  a USPosition was requested
   * 
   *   then put the window where requested.
   *
   *   If RandomPlacement was specified,
   *       then place the window in a psuedo-random location
   */
  if (!psw->fTransient &&
      !(psw->hints.flags & USPosition) &&
      ((psw->fNoPPosition) ||
       !(psw->hints.flags & PPosition)) &&
      !(PPosOverride) &&
      !((psw->wmhints) &&
	(psw->wmhints->flags & StateHint) &&
	(psw->wmhints->initial_state == IconicState))) {
    /* Get user's window placement, unless RandomPlacement is specified */
    if (psw->fRandomPlace) {
      if (psw->fSmartPlace) {
	SmartPlacement(psw, FRAME_WIDTH(psw) + 2 * psw->bw,
		       FRAME_HEIGHT(psw) + 2 * psw->bw,
		       &xl, &yt);
      }
      if (xl < 0) {
	/* plase window in a random location */
	if ((Scr.randomx += GetDecor(psw, TitleHeight)) > Scr.MyDisplayWidth / 2)
	  Scr.randomx = GetDecor(psw, TitleHeight);
	if ((Scr.randomy += 2 * GetDecor(psw, TitleHeight)) > Scr.MyDisplayHeight / 2)
	  Scr.randomy = 2 * GetDecor(psw, TitleHeight);
	psw->attr.x = Scr.randomx - psw->old_bw;
	psw->attr.y = Scr.randomy - psw->old_bw;
      } else {
	psw->attr.x = xl - psw->old_bw + psw->bw;
	psw->attr.y = yt - psw->old_bw + psw->bw;
      }
      /* patches 11/93 to try to keep the window on the
       * screen */
      { 
        int xNew = psw->attr.x + psw->old_bw - psw->bw;
        int yNew = psw->attr.y + psw->old_bw - psw->bw;
      
#ifdef USE_CASSOWARY
        psw->frame_x.set_value(xNew);
        psw->frame_y.set_value(yNew);
#else
        FRAME_X(psw) = xNew;
        FRAME_Y(psw) = yNew;
#endif
      }

      if (FRAME_X(psw) + FRAME_WIDTH(psw) +
	  2 * psw->boundary_width > Scr.MyDisplayWidth) {
	psw->attr.x = Scr.MyDisplayWidth - psw->attr.width
	  - psw->old_bw + psw->bw - 2 * psw->boundary_width;
	Scr.randomx = 0;
      }
      if (FRAME_Y(psw) + 2 * psw->boundary_width + psw->title_height
	  + FRAME_HEIGHT(psw) > Scr.MyDisplayHeight) {
	psw->attr.y = Scr.MyDisplayHeight - psw->attr.height
	  - psw->old_bw + psw->bw - psw->title_height -
	  2 * psw->boundary_width;;
	Scr.randomy = 0;
      }
      psw->xdiff = psw->attr.x - psw->bw;
      psw->ydiff = psw->attr.y - psw->bw;
    } else {
      xl = -1;
      yt = -1;
      if (psw->fSmartPlace)
	SmartPlacement(psw, FRAME_WIDTH(psw) + 2 * psw->bw,
		       FRAME_HEIGHT(psw) + 2 * psw->bw,
		       &xl, &yt);
      if (xl < 0) {
	if (GrabEm(CURSOR_POSITION)) {
	  /* Grabbed the pointer - continue */
	  XGrabServer_withSemaphore(dpy);
	  if (XGetGeometry(dpy, psw->w, &JunkRoot, &JunkX, &JunkY,
			   (unsigned int *) &DragWidth,
			   (unsigned int *) &DragHeight,
			   &JunkBW, &JunkDepth) == 0) {
            invalidate_window(psw->schwin);
	    FREECPP(psw);
	    XUngrabServer_withSemaphore(dpy);
	    return False;
	  }
	  DragWidth = FRAME_WIDTH(psw);
	  DragHeight = FRAME_HEIGHT(psw);

	  XMapRaised(dpy, Scr.SizeWindow);
	  moveLoop(psw, 0, 0, DragWidth, DragHeight,
		   &xl, &yt, False, True);
	  XUnmapWindow(dpy, Scr.SizeWindow);
	  XUngrabServer_withSemaphore(dpy);
	  UngrabEm();
	} else {
	  /* couldn't grab the pointer - better do something */
          call0_hooks(cannot_grab_hook);
	  xl = 0;
	  yt = 0;
	}
      }
      psw->attr.y = yt - psw->old_bw + psw->bw;
      psw->attr.x = xl - psw->old_bw + psw->bw;
      psw->xdiff = xl;
      psw->ydiff = yt;
    }
  } else {
    /* the USPosition was specified, or the window is a transient, 
     * or it starts iconic so place it automatically */

    psw->xdiff = psw->attr.x;
    psw->ydiff = psw->attr.y;
    /* put it where asked, mod title bar */
    /* if the gravity is towards the top, move it by the title height */
    psw->attr.y -= gravy * (psw->bw - psw->old_bw);
    psw->attr.x -= gravx * (psw->bw - psw->old_bw);
    if (gravy > 0)
      psw->attr.y -= 2 * psw->boundary_width + psw->title_height;
    if (gravx > 0)
      psw->attr.x -= 2 * psw->boundary_width;
  }
  return True;
}



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
