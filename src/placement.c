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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <guile/gh.h>

#define PLACEMENT_IMPLEMENTATION
#include "placement.h"

#include "scwm.h"
#include "move.h"
#include "screen.h"
#include "Grab.h"
#include "icons.h"
#include "virtual.h"
#include "callbacks.h"
#include "window.h"
#include "borders.h"

int get_next_x(ScwmWindow * psw, int x, int y);
int get_next_y(ScwmWindow * psw, int y);
int test_fit(ScwmWindow * psw, int test_x, int test_y, int aoimin);
void CleverPlacement(ScwmWindow * psw, int *x, int *y);

extern Bool PPosOverride;

SCM_SYMBOL(sym_placement_proc,"placement-proc");
SCM_SYMBOL(sym_transient_placement_proc,"transient-placement-proc");


/*
 *  Procedure:
 *	GetGravityOffsets - map gravity to (x,y) offset signs for adding
 *		to x and y when window is mapped to get proper placement.
 */


static struct _gravity_info grav_table[11] =
{
  {1, 1, 1},			/* ForgetGravity */
  {0, 0, 0},			/* NorthWestGravity */
  {1, 0, 0},			/* NorthGravity */
  {2, 0, 0},			/* NorthEastGravity */
  {0, 1, 1},			/* WestGravity */
  {1, 1, 1},			/* CenterGravity */
  {2, 1, 1},			/* EastGravity */
  {0, 2, 2},			/* SouthWestGravity */
  {1, 2, 2},			/* SouthGravity */
  {2, 2, 2},			/* SouthEastGravity */
  {1, 1, 2},			/* StaticGravity */
};


static
void 
GetGravityOffsets(ScwmWindow *psw)
{
  int g = ((psw->hints.flags & PWinGravity)
	   ? psw->hints.win_gravity : NorthWestGravity);

  if (g < ForgetGravity || g > StaticGravity) {
    psw->grav=grav_table[CenterGravity];
  } else {
    psw->grav=grav_table[g];
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

  if (Scr.fSmartPlacementIsClever) {	/* call clever placement instead? */
    CleverPlacement(psw, x, y);
    return;
  }
  temp_h = height;
  temp_w = width;

  while (((test_y + temp_h) < (Scr.DisplayHeight)) && (!loc_ok)) {
    test_x = 0;
    while (((test_x + temp_w) < (Scr.DisplayWidth)) && (!loc_ok)) {
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
  xnew = Scr.DisplayWidth;
  xtest = Scr.DisplayWidth - (FRAME_WIDTH(psw) + 2 * psw->bw);
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
  ynew = Scr.DisplayHeight;
  ytest = Scr.DisplayHeight - (FRAME_HEIGHT(psw) + 2 * psw->bw);
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

  if (y12 > Scr.DisplayHeight)	/* No room in y direction */
    return -1;
  if (x12 > Scr.DisplayWidth)	/* No room in x direction */
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

void 
default_select_desk(ScwmWindow *psw, int Desk)
{
  Atom atype;
  int aformat;
  unsigned long nitems, bytes_remain;
  unsigned char *prop;
  ScwmWindow *tpsw;

  /* Select a desk to put the window on (in list of priority):
   * 1. Sticky Windows stay on the current desk.
   * 2. Windows specified with StartsOnDesk go where specified
   * 3. Put it on the desk it was on before the restart.
   * 4. Transients go on the same desk as their parents.
   * 5. Window groups stay together (completely untested) */

  psw->Desk = Scr.CurrentDesk;

  if (psw->fSticky) {
    /* leave it alone. */
  } else if (psw->fStartsOnDesk) {
    psw->Desk = Desk;
  } else if ((XGetWindowProperty(dpy, psw->w, XA_WM_DESKTOP, 0L, 1L, True,
				 XA_WM_DESKTOP, &atype, &aformat, &nitems,
				 &bytes_remain, &prop) == Success)
	     && (NULL != prop)) {
    psw->Desk = *(unsigned long *) prop;
    XFree(prop);
  } else if (psw->fTransient && (psw->transientfor != None) &&
	     (psw->transientfor != Scr.Root) && 
	     (NULL != (tpsw = PswFromWindow(dpy, psw->transientfor)))) {
    /* Try to find the parent's desktop */
    psw->Desk = tpsw->Desk;
  } else if ((psw->wmhints) && (psw->wmhints->flags & WindowGroupHint) &&
	     (psw->wmhints->window_group != None) &&
	     (psw->wmhints->window_group != Scr.Root) &&
	     (NULL != (tpsw=PswFromWindow(dpy, psw->wmhints->window_group)))) {
    /* Try to find the group leader or another window
     * in the group */
    psw->Desk = tpsw->Desk;
  }

  /* I think it would be good to switch to the selected desk
   * whenever a new window pops up, except during initialization */  
  /* FIXGJB: this should be a callback, not a forced switch to the new
     desk --03/26/98 gjb */
  if ((!PPosOverride) && (!(psw->fShowOnMap)))
    changeDesks(0, psw->Desk);
}



SCWM_PROC(smart_place_window, "smart-place-window", 1, 0, 0, 
           (SCM win))
     /** Places WIN using fvwm2's SmartPlacement algorithm.
The placement is just as if SmartPlacementIsReallySmart were not in
effect. That is, it tries to place the window so that it does not
overlap any other. If it fails to do so, it returns #f; otherwise it
returns #t. */
#define FUNC_NAME s_smart_place_window
{
  ScwmWindow *psw;
  int x, y;

  if (!WINDOWP(win)) {
    scm_wrong_type_arg(FUNC_NAME, 1, win);
  }

  psw=PSWFROMSCMWIN(win);

  /* FIXMS: hackish workaround for now to not mess with the smart
     placement function itself, but make it not call CleverPlacement
     when called from here.. */

  { /* scope */
    /* save state of fSmartPlacementIsClever */
    Bool fSmartPlacementIsClever = Scr.fSmartPlacementIsClever;
    Scr.fSmartPlacementIsClever = False;
    SmartPlacement(psw, FRAME_WIDTH(psw) + 2 * psw->bw,
                   FRAME_HEIGHT(psw) + 2 * psw->bw, &x, &y);
    Scr.fSmartPlacementIsClever = fSmartPlacementIsClever;
  }

  if (x < 0) {
    return SCM_BOOL_F;
  } else {
    psw->attr.x = x = x - psw->old_bw + psw->bw;
    psw->attr.y = y = y - psw->old_bw + psw->bw;

    move_finalize(psw->frame,psw, psw->attr.x, psw->attr.y);
    return SCM_BOOL_T;
  }
}
#undef FUNC_NAME

SCWM_PROC(clever_place_window, "clever-place-window", 1, 0, 0, 
           (SCM win))
     /** Places WIN using fvwm2's "ReallySmart" algorithm.
The placement is just as if being placed by fvwm2's SmartPlacement,
as if SmartPlacementIsReallySmart were in effect. That is, it
tries to place the window so as to minimize its area of
overlap with other windows. Several parameters give different
weight to various kinds of windows, but they are not tunable
at runtime currently. If it fails to place the window, it
returns #f; otherwise it returns #t. */
#define FUNC_NAME s_clever_place_window
{
  ScwmWindow *psw;
  int x, y;

  if (!WINDOWP(win)) {
    scm_wrong_type_arg(FUNC_NAME, 1, win);
  }

  psw=PSWFROMSCMWIN(win);

  CleverPlacement(psw, &x, &y);

  if (x < 0) {
    return SCM_BOOL_F;
  } else {
    psw->attr.x = x = x - psw->old_bw + psw->bw;
    psw->attr.y = y = y - psw->old_bw + psw->bw;

    move_finalize(psw->frame,psw, psw->attr.x, psw->attr.y);
    return SCM_BOOL_T;
  }
}
#undef FUNC_NAME


SCWM_PROC(random_place_window, "random-place-window", 1, 0, 0, 
           (SCM win))
     /** Places WIN just as if being placed by fvwm2's RandomPlacement.
This placement is not truly random; it is based on two state variables
which are incremented for the x and y coordinates, and which wrap
around once a window would be forced off the screen. The placement is
fairly arbitrary, but always succeeds, and so avoids user
interaction. #t is always returned. */
#define FUNC_NAME s_random_place_window
{
  ScwmWindow *psw;

  if (!WINDOWP(win)) {
    scm_wrong_type_arg(FUNC_NAME, 1, win);
  }

  psw=PSWFROMSCMWIN(win);
  
  /* place window in a random location */
  if ((Scr.randomx += GET_DECOR(psw, TitleHeight)) > Scr.DisplayWidth / 2) {
    Scr.randomx = GET_DECOR(psw, TitleHeight);
  }
  if ((Scr.randomy += 2 * GET_DECOR(psw, TitleHeight)) >
      Scr.DisplayHeight / 2) {
    Scr.randomy = 2 * GET_DECOR(psw, TitleHeight);
  }

  psw->attr.x = Scr.randomx - psw->old_bw;
  psw->attr.y = Scr.randomy - psw->old_bw;

  move_finalize(psw->frame,psw, psw->attr.x, psw->attr.y);
  return SCM_BOOL_T; 
}
#undef FUNC_NAME


SCWM_PROC(default_placement_proc, "default-placement-proc", 1, 0, 0, 
           (SCM win))
     /** Use various flags to call an appropriate placement function.
This is the default placement procedure for non-transient windows. It
tries `smart-place-window', `clever-place-window',
`random-place-window', or `interactive-move' (to achieve interactive
placement) on WIN depending on several style flags. However,
if one of the following factors holds, the window will instead be
placed exactly as requested by the program: the position was specified
by the user, the position was specified by the program, and
#:no-PPosition-hint is not set, or the window starts iconic. */
#define FUNC_NAME s_default_placement_proc
{ 
  ScwmWindow *psw;

  if (!WINDOWP(win)) {
    scm_wrong_type_arg(FUNC_NAME, 1, win);
  }

  psw=PSWFROMSCMWIN(win);

  if (PPosOverride ||
      (psw->hints.flags & USPosition) ||
      (!psw->fNoPPosition && (psw->hints.flags & PPosition)) ||
      ((psw->wmhints) &&
       (psw->wmhints->flags & StateHint) &&
       (psw->wmhints->initial_state == IconicState))) {
    /* Do nothing. */

  } else {
    SCM result=SCM_BOOL_F;

    if (psw->fSmartPlace) {
      if (Scr.fSmartPlacementIsClever) {
	result=clever_place_window(win);
      } else {
	result=smart_place_window(win);
      }
    }
    
    if (SCM_BOOL_F==result) {
      if (psw->fRandomPlace) {
	random_place_window(win);
      } else {
        int finalx, finaly;     /* unused for now */
        extern Bool have_orig_position;
        extern int orig_x, orig_y;
        /* FIXGJB: passing thru globals */
        have_orig_position = True;
        orig_x = 0; orig_y = 0; 
	InteractiveMove(psw, False, &finalx, &finaly);
      }
    }
  }
  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCWM_PROC(default_transient_placement_proc, "default-transient-placement-proc", 1, 0, 0, 
           (SCM win))
     /** This is the default placement procedure for transient windows.
It simply leaves the window WIN in place, exactly as requested. */
#define FUNC_NAME s_default_transient_placement_proc
{
  /* Just doing nothing should dtrt. */

  return SCM_BOOL_T;
}
#undef FUNC_NAME





/*
 * Handles initial placement and sizing of a new window
 * Returns False in the event of a lost window.
 */
Bool 
PlaceWindow(ScwmWindow *psw, int Desk)
{
  SCM place_proc;
  SCM win;

  GetGravityOffsets(psw);

  
  /* FIXMS: The desk selection stuff should be folded into the
     placement-procs, but let's leave it as it is for now. */

  default_select_desk(psw,Desk);

  move_finalize(psw->frame,psw, 
		psw->attr.x + GRAV_X_ADJUSTMENT(psw), 
		psw->attr.y + GRAV_Y_ADJUSTMENT(psw));


  win=psw->schwin;

  if (psw->fTransient) {
    place_proc=scm_object_property(win,sym_transient_placement_proc);
    if (SCM_BOOL_F==place_proc || 
	SCM_BOOL_F == scwm_safe_call1(place_proc, win)) {
      default_transient_placement_proc(win);
    }
  } else {
    place_proc=scm_object_property(win,sym_placement_proc);
    if (SCM_BOOL_F==place_proc || 
	SCM_BOOL_F == scwm_safe_call1(place_proc, win)) {
      default_placement_proc(win);
    }
  }

  return True;
}


void init_placement()
{
#ifndef SCM_MAGIC_SNARFER
#include "placement.x"
#endif
}



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
