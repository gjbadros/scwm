/* $Id$
 * placement.c
 * Copyright (C) 1998, 1999, 2000  Greg J. Badros and Maciej Stachowiak 
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
#include "scwmconfig.h"
#endif

#include <stdio.h>
#include <unistd.h>
#include <string.h>

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
#include "xmisc.h"

#undef DEBUG_PLACE_WINDOW

extern Bool PPosOverride;

SCM_VARIABLE_INIT(scm_default_placement_proc,"default-placement-proc",SCM_BOOL_F);
/** The default procedure to use to place windows 
    that do not have a placement proc. */
SCM_VARIABLE_INIT(scm_default_transient_placement_proc,"default-transient-placement-proc",SCM_BOOL_F);
/** The default procedure to use to place transient windows 
    that do not have a transient placement proc. */
SCM_SYMBOL(sym_placement_proc,"placement-proc");
SCM_SYMBOL(sym_transient_placement_proc,"transient-placement-proc");


/*
 *  Procedure:
 *	GetGravityOffsets - map gravity to (x,y) offset signs for adding
 *		to x and y when window is mapped to get proper placement.
 */

extern SCM sym_grav_forget,
  sym_grav_northwest,
  sym_grav_north,
  sym_grav_northeast,
  sym_grav_west,
  sym_grav_center,
  sym_grav_east,
  sym_grav_southwest,
  sym_grav_south,
  sym_grav_southeast,
  sym_grav_static;
  

static struct gravity_info_tag grav_table[11] =
{
  {1, 1, 1, &sym_grav_forget},    /* ForgetGravity */
  {0, 0, 0, &sym_grav_northwest}, /* NorthWestGravity */
  {1, 0, 0, &sym_grav_north},     /* NorthGravity */
  {2, 0, 0, &sym_grav_northeast}, /* NorthEastGravity */
  {0, 1, 1, &sym_grav_west},      /* WestGravity */
  {1, 1, 1, &sym_grav_center},    /* CenterGravity */
  {2, 1, 1, &sym_grav_east},      /* EastGravity */
  {0, 2, 2, &sym_grav_southwest}, /* SouthWestGravity */
  {1, 2, 2, &sym_grav_south},     /* SouthGravity */
  {2, 2, 2, &sym_grav_southeast}, /* SouthEastGravity */
  {1, 1, 2, &sym_grav_static},    /* StaticGravity */
};


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

void
SetPswGravity(ScwmWindow *psw, int g)
{
  XSetWindowAttributes attrib;
  attrib.win_gravity = g;
  XChangeWindowAttributes(dpy,psw->frame,CWWinGravity,&attrib);
  psw->hints.flags |= PWinGravity;
  assert(g >= ForgetGravity && g <= StaticGravity);
  psw->hints.win_gravity = g;
  psw->grav = grav_table[g];
  XSetWMNormalHints(dpy, psw->w, &psw->hints);
}

/* Return -1 for no match */
int
GravityFromSym(SCM sym)
{
  int i;
  for (i=0; i <= StaticGravity; ++i) {
    if (*(grav_table[i].psym) == sym)
      return i;
  }
  return -1;
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
#define AVOIDICON 10		/*  Try hard to not place windows over icons */

static
Bool
SmartPlacement(ScwmWindow *psw, int width, int height, int *x, int *y)
{
  int temp_h, temp_w;
  int test_x = *x, test_y = *y;
  int loc_ok = False, tw, tx, ty, th;
  ScwmWindow *pswTest;

  temp_h = height;
  temp_w = width;

  while (((test_y + temp_h) < (Scr.DisplayHeight)) && (!loc_ok)) {
    test_x = 0;
    while (((test_x + temp_w) < (Scr.DisplayWidth)) && (!loc_ok)) {
      loc_ok = True;
      pswTest = Scr.ScwmRoot.next;
      while ((pswTest != (ScwmWindow *) 0) && (loc_ok == True)) {
	if (pswTest->Desk == Scr.CurrentDesk) {
	  if (pswTest->fIconified &&
	      !pswTest->fIconUnmapped &&
	      pswTest->icon_w &&
	      pswTest != psw) {
	    tw = pswTest->icon_p_width;
	    th = pswTest->icon_p_height + pswTest->icon_w_height;
	    tx = ICON_X_VP(pswTest);
	    ty = ICON_Y_VP(pswTest);

	    if ((tx < (test_x + width)) && ((tx + tw) > test_x) &&
		(ty < (test_y + height)) && ((ty + th) > test_y)) {
	      loc_ok = False;
	      test_x = tx + tw;
	    }
	  }
	  if (!pswTest->fIconified && (pswTest != psw)) {
	    tw = FRAME_WIDTH(pswTest) + 2 * pswTest->bw;
	    th = FRAME_HEIGHT(pswTest) + 2 * pswTest->bw;
	    tx = FRAME_X_VP(pswTest);
	    ty = FRAME_Y_VP(pswTest);
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
    return False;
  }
  *x = test_x;
  *y = test_y;
  return True;
}


static int get_next_x(ScwmWindow * psw, int x, int y);
static int get_next_y(ScwmWindow * psw, int y);
static int test_fit(ScwmWindow * psw, int test_x, int test_y, int aoimin);

/* CleverPlacement by Anthony Martin <amartin@engr.csulb.edu>
 * This function will place a new window such that there is a minimum amount
 * of interference with other windows.  If it can place a window without any
 * interference, fine.  Otherwise, it places it so that the area of of
 * interference between the new window and the other windows is minimized */
Bool
CleverPlacement(ScwmWindow *psw, int *x, int *y)
{
  int test_x = *x, test_y = *y;
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
  return True;
}

static int 
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
      if ((y < testw->icon_p_height + testw->icon_w_height + ICON_Y_VP(testw)) &&
	  (ICON_Y_VP(testw) < (FRAME_HEIGHT(psw) + 2 * psw->bw + y))) {
	xtest = testw->icon_p_width + ICON_X_VP(testw);
	if (xtest > x)
	  xnew = min(xnew, xtest);
	xtest = ICON_X_VP(testw) - (FRAME_WIDTH(psw) + 2 * psw->bw);
	if (xtest > x)
	  xnew = min(xnew, xtest);
      }
    } else if ((y < (FRAME_HEIGHT(testw) + 2 * testw->bw + FRAME_Y_VP(testw))) &&
	       (FRAME_Y_VP(testw) < (FRAME_HEIGHT(psw) + 2 * psw->bw + y))) {
      xtest = FRAME_WIDTH(testw) + 2 * testw->bw + FRAME_X_VP(testw);
      if (xtest > x)
	xnew = min(xnew, xtest);
      xtest = FRAME_X_VP(testw) - (FRAME_WIDTH(psw) + 2 * psw->bw);
      if (xtest > x)
	xnew = min(xnew, xtest);
    }
  }
  return xnew;
}

static int 
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
      ytest = testw->icon_p_height + testw->icon_w_height + ICON_Y_VP(testw);
      if (ytest > y)
	ynew = min(ynew, ytest);
      ytest = ICON_Y_VP(testw) - (FRAME_HEIGHT(psw) + 2 * psw->bw);
      if (ytest > y)
	ynew = min(ynew, ytest);
    } else {
      ytest = FRAME_HEIGHT(testw) + 2 * testw->bw + FRAME_Y_VP(testw);
      if (ytest > y)
	ynew = min(ynew, ytest);
      ytest = FRAME_Y_VP(testw) - (FRAME_HEIGHT(psw) + 2 * psw->bw);
      if (ytest > y)
	ynew = min(ynew, ytest);
    }
  }
  return ynew;
}

static int 
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
      x21 = ICON_X_VP(testw);
      y21 = ICON_Y_VP(testw);
      x22 = x21 + testw->icon_p_width;
      y22 = y21 + testw->icon_p_height + testw->icon_w_height;
    } else {
      x21 = FRAME_X_VP(testw);
      y21 = FRAME_Y_VP(testw);
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
default_select_desk(ScwmWindow *psw)
{
  Atom atype;
  int aformat;
  unsigned long nitems, bytes_remain;
  unsigned char *prop;
  ScwmWindow *tpsw;
  int Desk;

  /* Select a desk to put the window on (in list of priority):
   * 1. Sticky Windows stay on the current desk.
   * 2. Windows specified with StartsOnDesk go where specified
   * 3. Put it on the desk it was on before the restart.
   * 4. Transients go on the same desk as their parents.
   * 5. Window groups stay together (completely untested) */

  Desk=psw->StartDesk;
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
}


SCM_DEFINE(place_on_default_desk, "place-on-default-desk", 1, 0, 0, 
          (SCM win),
"Place WIN on a desk according to the default algorithm.\n\
Place a window with it's window group, with a window it is transient\n\
for, on the desk it was on previous to a restart, on the desk\n\
specified with the starts-on-desk flag,\n\
\n\
This is called as part of `default-placement-proc'.  It could also be\n\
used in user-defined placement procedures (see \n\
`set-window-placement-proc!').")
#define FUNC_NAME s_place_on_default_desk
{
  ScwmWindow *psw;
  VALIDATE_ARG_WIN_COPY(1,win,psw);
  default_select_desk(psw);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE(smart_place_window, "smart-place-window", 1, 3, 0, 
          (SCM win, SCM no_move, SCM x_sugg, SCM y_sugg),
"Places WIN using fvwm2's SmartPlacement algorithm.\n\
The placement is just as if SmartPlacementIsReallySmart were not in\n\
effect. That is, it tries to place the window so that it does not\n\
overlap any other. If it fails to do so, it returns #f; otherwise it\n\
returns #t.\n\
\n\
If NO-MOVE is #t, then the position is returned instead of\n\
actually moving the window to that position.  This can\n\
be useful for finding a new location for an existing window.\n\
X-SUGG and Y-SUGG are suggested coordinates that `clever-place-window'\n\
may try to use as a preferred location for WIN.")
#define FUNC_NAME s_smart_place_window
{
  ScwmWindow *psw;
  int x, y;
  Bool fNoMove;
  Bool fPlacedOk = False;

  VALIDATE_ARG_WIN_COPY(1,win,psw);
  VALIDATE_ARG_BOOL_COPY_USE_F(2,no_move,fNoMove);
  VALIDATE_ARG_INT_COPY_USE_DEF(3,x_sugg,x,0);
  VALIDATE_ARG_INT_COPY_USE_DEF(4,y_sugg,y,0);

  fPlacedOk = SmartPlacement(psw, FRAME_WIDTH(psw) + 2 * psw->bw,
                             FRAME_HEIGHT(psw) + 2 * psw->bw, &x, &y);

  if (fNoMove) {
    return scm_list_n(scm_from_int(x),scm_from_int(y),SCM_UNDEFINED);
  }

  if (!fPlacedOk) {
    return SCM_BOOL_F;
  } else {
    /* GJB:FIXME:MS: Why fix for bw diffs when clever placement is placing frame,
       not client window?  This is not cst w/ the below. */
    psw->attr.x = x = x - psw->old_bw + psw->bw;
    psw->attr.y = y = y - psw->old_bw + psw->bw;

    move_finalize(psw->frame, psw, x, y);
    return SCM_BOOL_T;
  }
}
#undef FUNC_NAME

SCM_DEFINE(clever_place_window, "clever-place-window", 1, 3, 0, 
          (SCM win, SCM no_move, SCM x_sugg, SCM y_sugg),
"Places WIN using fvwm2's \"ReallySmart\" algorithm.\n\
The placement is just as if being placed by fvwm2's SmartPlacement,\n\
as if SmartPlacementIsReallySmart were in effect. That is, it\n\
tries to place the window so as to minimize its area of\n\
overlap with other windows. Several parameters give different\n\
weight to various kinds of windows, but they are not tunable\n\
at runtime currently. If it fails to place the window, it\n\
returns #f; otherwise it returns #t.\n\
\n\
If NO-MOVE is #t, then the position is returned instead of\n\
actually moving the window to that position.  This can\n\
be useful for finding a new location for an existing window.\n\
X-SUGG and Y-SUGG are suggested coordinates that `clever-place-window'\n\
may try to use as a preferred location for WIN.")
#define FUNC_NAME s_clever_place_window
{
  ScwmWindow *psw;
  int x, y;
  Bool fNoMove;
  Bool fPlacedOk = False;

  VALIDATE_ARG_WIN_COPY(1,win,psw);
  VALIDATE_ARG_BOOL_COPY_USE_F(2,no_move,fNoMove);
  VALIDATE_ARG_INT_COPY_USE_DEF(3,x_sugg,x,0);
  VALIDATE_ARG_INT_COPY_USE_DEF(4,y_sugg,y,0);

  fPlacedOk = CleverPlacement(psw, &x, &y);

  if (!fPlacedOk)
    return SCM_BOOL_F;

  if (fNoMove) {
    return scm_list_n(scm_from_int(x),scm_from_int(y),SCM_UNDEFINED);
  } else {
    /* GJB:FIXME:MS: Why fix for bw diffs when clever placement is placing frame,
       not client window?  This is not cst w/ the above. */
    psw->attr.x = x - psw->old_bw + psw->bw;
    psw->attr.y = y - psw->old_bw + psw->bw;

    move_finalize(psw->frame,psw, x, y);
    return SCM_BOOL_T;
  }
}
#undef FUNC_NAME


SCM_DEFINE(random_place_window, "random-place-window", 1, 3, 0, 
          (SCM win, SCM no_move, ARG_UNUSED(SCM x_sugg), ARG_UNUSED(SCM y_sugg)),
"Places WIN just as if being placed by fvwm2's RandomPlacement.\n\
This placement is not truly random; it is based on two state variables\n\
which are incremented for the x and y coordinates, and which wrap\n\
around once a window would be forced off the screen. The placement is\n\
fairly arbitrary, but always succeeds, and so avoids user\n\
interaction. #t is always returned.\n\
\n\
If NO-MOVE is #t, then just return the new position but do not\n\
move WIN. X-SUGG and Y-SUGG are ignored.")
#define FUNC_NAME s_random_place_window
{
  ScwmWindow *psw;
  Bool fNoMove;

  VALIDATE_ARG_WIN_COPY(1,win,psw);
  VALIDATE_ARG_BOOL_COPY_USE_F(2,no_move,fNoMove);
  
  /* place window in a random location;
     uses Scr.randomx, Scr.randomy as holders of the
     last pseudo-random location */
  if ((Scr.randomx += GET_DECOR(psw, TitleHeight)) > Scr.DisplayWidth / 2) {
    Scr.randomx = GET_DECOR(psw, TitleHeight);
  }
  if ((Scr.randomy += 2 * GET_DECOR(psw, TitleHeight)) >
      Scr.DisplayHeight / 2) {
    Scr.randomy = 2 * GET_DECOR(psw, TitleHeight);
  }

  if (fNoMove) {
    return scm_list_n(scm_from_int(Scr.randomx),scm_from_int(Scr.randomy),SCM_UNDEFINED);
  } else {
    psw->attr.x = Scr.randomx - psw->old_bw + psw->bw;
    psw->attr.y = Scr.randomy - psw->old_bw + psw->bw;

    move_finalize(psw->frame,psw, Scr.randomx, Scr.randomy);
  }
  return SCM_BOOL_T; 
}
#undef FUNC_NAME


SCM_DEFINE(initial_place_window, "initial-place-window", 1, 0, 0, 
          (SCM win),
"Pick a desk for WIN and return #t iff WIN should be placed.\n"
"N.B. the return value is different from the return value\n"
"of the various placement procedures.  This return values\n"
"is #f to signify that no further placement is necessary\n"
"according to the various positioning hints attached to WIN.")
#define FUNC_NAME s_initial_place_window
{ 
  ScwmWindow *psw;
  VALIDATE_ARG_WIN_COPY(1,win,psw);

  if (!PPosOverride && !psw->fShowOnMap)
    changeDesks(0, psw->Desk);

  default_select_desk(psw);

  if (PPosOverride ||
      (psw->hints.flags & USPosition) ||
      (!psw->fNoPPosition && (psw->hints.flags & PPosition)) ||
      ((psw->wmhints) &&
       (psw->wmhints->flags & StateHint) &&
       (psw->wmhints->initial_state == IconicState))) {
    /* Do nothing. */
    return SCM_BOOL_F;
  }
  return SCM_BOOL_T;
}
#undef FUNC_NAME


SCM_DEFINE(interactive_place_window, "interactive-place-window", 1, 3, 0, 
          (SCM win, SCM no_move, SCM x_sugg, SCM y_sugg),
"Interactively place WIN.\n\
Start at X-SUGG, Y-SUGG. If NO-MOVE is #t, just return the\n\
final resting place instead of actually moving the window there.")
#define FUNC_NAME s_interactive_place_window
{ 
  ScwmWindow *psw;
  Bool fNoMove;
  Bool fPlacedOk = False;
  int finalx, finaly;
  int x, y;

  VALIDATE_ARG_WIN_COPY(1,win,psw);
  VALIDATE_ARG_BOOL_COPY_USE_F(2,no_move,fNoMove);

  { /* scope */
    /* GJB:FIXME:: ugh! passing args thru globals */
    extern Bool have_orig_position;
    extern int orig_x, orig_y;
    Bool old_have_orig_pos = have_orig_position;
    have_orig_position = True;
    if (UNSET_SCM(x_sugg) || UNSET_SCM(y_sugg))
      FXGetWindowTopLeft(WFrameOrIcon(psw), &x, &y);
    VALIDATE_ARG_INT_COPY_USE_DEF(3,x_sugg,orig_x,x);
    VALIDATE_ARG_INT_COPY_USE_DEF(4,y_sugg,orig_y,y);
    fPlacedOk = InteractiveMove(psw, False, &finalx, &finaly);
    have_orig_position = old_have_orig_pos;
  }

  if (!fPlacedOk)
    return SCM_BOOL_F;

  if (fNoMove) {
    return scm_list_n(scm_from_int(finalx),scm_from_int(finaly),SCM_UNDEFINED);
  } else {
    /* move_finalize(psw->frame,psw,finalx,finaly); */
    MovePswToCurrentPosition(psw);
  }
  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM_DEFINE(null_place_window, "null-place-window", 1, 0, 0, 
          (SCM win),
"This is simplest, fallback placement procedure for windows.\n\
It simply leaves the window WIN in place, exactly as requested.")
#define FUNC_NAME s_null_place_window
{
  initial_place_window(win);
  return SCM_BOOL_T;
}
#undef FUNC_NAME

/*
 * Handles initial placement and sizing of a new window
 */
Bool 
PlaceWindow(ScwmWindow *psw)
{
  SCM place_proc;
  SCM win;
  GetGravityOffsets(psw);

  
  /* MS:FIXME:: The desk selection stuff should be folded into the
     placement-procs, but let's leave it as it is for now. */

  default_select_desk(psw);

#ifdef DEBUG_PLACE_WINDOW
  scwm_msg(DBG,"PlaceWindow","placing %s, attr = (%d,%d), Global PPosOverride = %d, fShowOnMap = %d",
           psw->name,psw->attr.x,psw->attr.y,PPosOverride,psw->fShowOnMap);
#endif

  { /* scope */
    int cpixX = GRAV_X_ADJUSTMENT(psw);
    int cpixY = GRAV_Y_ADJUSTMENT(psw);
    
    move_finalize(psw->frame,psw, 
                  psw->attr.x + cpixX,
                  psw->attr.y + cpixY);
  }

  win = SCM_FROM_PSW(psw);

  if (psw->fTransient) {
#ifdef DEBUG_PLACE_WINDOW
    scwm_msg(DBG,"PlaceWindow","%s is a transient window",psw->name);
#endif
    /* we need to use the default placement procedure 
       if we're doing the initial window capture, otherwise we get into
       a deadlock --12/07/99 gjb */
    if (!Scr.fWindowsCaptured)
      return scm_is_true(null_place_window(win));
    else {
      place_proc = scm_object_property(win,sym_transient_placement_proc);

      return
        /* either use the place_proc ... */
        ((scm_is_true(scm_procedure_p(place_proc)) && 
          scm_is_true(scwm_safe_call1(place_proc, win))) ||
         
         /* or the default_transient_placement_proc ... */
         (scm_is_true(scm_procedure_p(scm_variable_ref(scm_default_transient_placement_proc))) &&
          scm_is_true(scwm_safe_call1(scm_variable_ref(scm_default_transient_placement_proc), win))) ||
         
         /* or the null placement_proc */
         scm_is_true(null_place_window(win)));
    }
  } else {
    if (!Scr.fWindowsCaptured )
      return scm_is_true(null_place_window(win));
    else {
      place_proc = scm_object_property(win,sym_placement_proc);

      return
        /* either use the place_proc ... */
        ((scm_is_true(scm_procedure_p(place_proc)) && 
          scm_is_true(scwm_safe_call1(place_proc, win))) ||
         
         /* or the default_transient_placement_proc ... */
         (scm_is_true(scm_procedure_p(scm_variable_ref(scm_default_placement_proc))) &&
          scm_is_true(scwm_safe_call1(scm_variable_ref(scm_default_placement_proc), win))) ||
         
         /* or the null placement_proc */
         scm_is_true(null_place_window(win)));
    }
  }
}


void init_placement()
{
#include "placement.x"
}



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */
