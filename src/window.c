/* $Id */
#define WINDOW_IMPLEMENTATION

/****************************************************************************
 * This module has been significantly modified by Maciej Stachowiak.
 * It may be used under the terms of the fvwm copyright (see COPYING.FVWM).
 * Changes Copyright 1997, Maciej stachowiak
 ****************************************************************************/
#include <config.h>
#include "scwm.h"
#include "screen.h"
#include "misc.h"
#include "parse.h"
#include "module.h"
#include <stdio.h>
#include <guile/gh.h>
#include <X11/keysym.h>
#include "ICCCM.h"
#include "window.h"
#include "color.h"
#include "util.h"
#include "errors.h"
#include "Grab.h"

extern ScwmDecor *last_decor, *cur_decor;

size_t 
free_window(SCM obj)
{
  free(WINDOW(obj));
  return (0);
}

SCM 
mark_window(SCM obj)
{

  SCM_SETGC8MARK(obj);

  if (VALIDWINP(obj) && SCWMWINDOW(obj)->fl != NULL) {
    scm_gc_mark(SCWMWINDOW(obj)->fl->scmdecor);
  }
  return SCM_BOOL_F;
}


int 
print_window(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_puts("#<window ", port);
  if (VALIDWINP(obj)) {
    scm_write(gh_ulong2scm((unsigned long) (SCWMWINDOW(obj)->w)), port);
  } else {
    scm_puts("(invalid)", port);
  }
  scm_putc('>', port);

  return 1;
}


SCM 
make_window(ScwmWindow * win)
{
  scwm_window *schwin;
  SCM answer;

  schwin = safemalloc(sizeof(scwm_window));

  /* FIXGJB: we should decide on right way to do memory allocation;
     this check is redundant w/ safemalloc */
  if (schwin == NULL) {
    scm_memory_error("make_window");
  }
  SCM_DEFER_INTS;

  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, scm_tc16_scwm_window);
  SCM_SETCDR(answer, (SCM) schwin);
  SCWMWINDOW(answer) = win;
  VALIDWINP(answer) = 1;
  scm_protect_object(answer);

  SCM_ALLOW_INTS;
  return answer;
}

void 
invalidate_window(SCM schwin)
{
  VALIDWINP(schwin) = 0;
  SCWMWINDOW(schwin) = NULL;
  scm_unprotect_object(schwin);
}


SCM 
ensure_valid(SCM win, int n, char *subr, SCM kill_p)
{
  if (win == SCM_UNDEFINED) {
    win = get_window(kill_p, SCM_BOOL_T);
    if (win == SCM_BOOL_F || win == SCM_UNDEFINED) {
      return SCM_BOOL_F;
    }
  }
  if (!(SCM_NIMP(win) && WINDOWP(win))) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(subr, n, win);
  }
  if (!VALIDWINP(win)) {
    SCM_ALLOW_INTS;
    scwm_error(subr, 6);
    /* maybe should just return SCM_BOOL_F; */
  }
  return (win);
}

SCM 
window_p(SCM obj)
{
  return ((SCM_NIMP(obj) && WINDOWP(obj)) ? SCM_BOOL_T : SCM_BOOL_F);
}

SCM 
get_window(SCM kill_p, SCM select_p)
{
  if (kill_p == SCM_UNDEFINED) {
    kill_p = SCM_BOOL_F;
  } else if (!gh_boolean_p(kill_p)) {
    scm_wrong_type_arg("get-window", 1, kill_p);
  }
  if (select_p == SCM_UNDEFINED) {
    select_p = SCM_BOOL_T;
  } else if (!gh_boolean_p(select_p)) {
    scm_wrong_type_arg("get-window", 2, select_p);
  }
  if (window_context == SCM_UNDEFINED) {
    if (select_p == SCM_BOOL_T) {
      return select_window(kill_p);
    } else {
      return SCM_BOOL_F;
    }
  }
  return window_context;
}


/**************************************************************************
 *
 * Moves focus to specified window 
 *
 *************************************************************************/
void 
FocusOn(ScwmWindow * t, int DeIconifyOnly)
{
#ifndef NON_VIRTUAL
  int dx, dy;
  int cx, cy;

#endif
  int x, y;

  if (t == (ScwmWindow *) 0)
    return;

  if (t->Desk != Scr.CurrentDesk) {
    changeDesks(0, t->Desk);
  }
#ifndef NON_VIRTUAL
  if (t->flags & ICONIFIED) {
    cx = t->icon_xl_loc + t->icon_w_width / 2;
    cy = t->icon_y_loc + t->icon_p_height + ICON_HEIGHT / 2;
  } else {
    cx = t->frame_x + t->frame_width / 2;
    cy = t->frame_y + t->frame_height / 2;
  }

  dx = (cx + Scr.Vx) / Scr.MyDisplayWidth * Scr.MyDisplayWidth;
  dy = (cy + Scr.Vy) / Scr.MyDisplayHeight * Scr.MyDisplayHeight;

  MoveViewport(dx, dy, True);
#endif

  if (t->flags & ICONIFIED) {
    x = t->icon_xl_loc + t->icon_w_width / 2;
    y = t->icon_y_loc + t->icon_p_height + ICON_HEIGHT / 2;
  } else {
    x = t->frame_x;
    y = t->frame_y;
  }
  RaiseWindow(t);
  KeepOnTop();

  /* If the window is still not visible, make it visible! */
  if (((t->frame_x + t->frame_height) < 0) || (t->frame_y + t->frame_width < 0) ||
  (t->frame_x > Scr.MyDisplayWidth) || (t->frame_y > Scr.MyDisplayHeight)) {
    SetupFrame(t, 0, 0, t->frame_width, t->frame_height, False);
    if (!(t->flags & ClickToFocus))
      XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, 2, 2);
  }
  UngrabEm();
  SetFocus(t->w, t, 0);
}



/**************************************************************************
 *
 * Moves pointer to specified window 
 *
 *************************************************************************/
void 
WarpOn(ScwmWindow * t, int warp_x, int x_unit, int warp_y, int y_unit)
{
#ifndef NON_VIRTUAL
  int dx, dy;
  int cx, cy;

#endif
  int x, y;

  if (t == (ScwmWindow *) 0 || (t->flags & ICONIFIED && t->icon_w == None))
    return;

  if (t->Desk != Scr.CurrentDesk) {
    changeDesks(0, t->Desk);
  }
#ifndef NON_VIRTUAL
  if (t->flags & ICONIFIED) {
    cx = t->icon_xl_loc + t->icon_w_width / 2;
    cy = t->icon_y_loc + t->icon_p_height + ICON_HEIGHT / 2;
  } else {
    cx = t->frame_x + t->frame_width / 2;
    cy = t->frame_y + t->frame_height / 2;
  }

  dx = (cx + Scr.Vx) / Scr.MyDisplayWidth * Scr.MyDisplayWidth;
  dy = (cy + Scr.Vy) / Scr.MyDisplayHeight * Scr.MyDisplayHeight;

  MoveViewport(dx, dy, True);
#endif

  if (t->flags & ICONIFIED) {
    x = t->icon_xl_loc + t->icon_w_width / 2 + 2;
    y = t->icon_y_loc + t->icon_p_height + ICON_HEIGHT / 2 + 2;
  } else {
    if (x_unit != Scr.MyDisplayWidth)
      x = t->frame_x + 2 + warp_x;
    else
      x = t->frame_x + 2 + (t->frame_width - 4) * warp_x / 100;
    if (y_unit != Scr.MyDisplayHeight)
      y = t->frame_y + 2 + warp_y;
    else
      y = t->frame_y + 2 + (t->frame_height - 4) * warp_y / 100;
  }
  if (warp_x >= 0 && warp_y >= 0) {
    XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, x, y);
  }
  RaiseWindow(t);
  KeepOnTop();

  /* If the window is still not visible, make it visible! */
  if (((t->frame_x + t->frame_height) < 0) || (t->frame_y + t->frame_width < 0) ||
  (t->frame_x > Scr.MyDisplayWidth) || (t->frame_y > Scr.MyDisplayHeight)) {
    SetupFrame(t, 0, 0, t->frame_width, t->frame_height, False);
    XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, 2, 2);
  }
  UngrabEm();
}

/***********************************************************************
 *
 *  Procedure:
 *	DeferExecution - defer the execution of a function to the
 *	    next button press if the context is C_ROOT
 *
 *  Inputs:
 *      eventp  - pointer to XEvent to patch up
 *      w       - pointer to Window to patch up
 *      tmp_win - pointer to ScwmWindow Structure to patch up
 *	context	- the context in which the mouse button was pressed
 *	func	- the function to defer
 *	cursor	- the cursor to display while waiting
 *      finishEvent - ButtonRelease or ButtonPress; tells what kind of event to
 *                    terminate on.
 *
 ***********************************************************************/
int 
DeferExecution(XEvent * eventp, Window * w, ScwmWindow ** tmp_win,
	       unsigned long *context, int cursor, int FinishEvent)
{
  Bool fDone = False;
  Bool fFinished = False;
  Window dummy;
  Window original_w;

  original_w = *w;

  if ((*context != C_ROOT) && (*context != C_NO_CONTEXT)) {
    if ((FinishEvent == ButtonPress) || ((FinishEvent == ButtonRelease) &&
					 (eventp->type != ButtonPress))) {
      return False;
    }
  }

  if (!GrabEm(cursor)) {
    /* FIXGJB: call a scheme hook, not XBell */
    XBell(dpy, Scr.screen);
    return True;
  }

  while (!fFinished) {
    fDone = False;
    /* block until there is an event */
/* FIXGJB: hard to know why this was looking for so many different
   events; I think we just need those in the new call below --10/25/97 gjb
    XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask |
	       ExposureMask | KeyPressMask | VisibilityChangeMask |
	       ButtonMotionMask | PointerMotionMask, eventp); */
    XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask |
	       ExposureMask | KeyPressMask | VisibilityChangeMask, eventp);
    StashEventTime(eventp);

    if (eventp->type == KeyPress)
      Keyboard_shortcuts(eventp, FinishEvent);
    if (eventp->type == FinishEvent)
      fFinished = True;
    if (eventp->type == ButtonPress) {
      XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask |
		 ExposureMask | KeyPressMask | VisibilityChangeMask |
		 ButtonMotionMask | PointerMotionMask, eventp);
      XAllowEvents(dpy, ReplayPointer, CurrentTime);
      fDone = True;
    }
    if (eventp->type == ButtonRelease || eventp->type == KeyPress) {
      fDone = True;
    }
    if (!fDone) {
      DispatchEvent();
    }
  }


  *w = eventp->xany.window;
  if (((*w == Scr.Root) || (*w == Scr.NoFocusWin))
      && (eventp->xbutton.subwindow != (Window) 0)) {
    *w = eventp->xbutton.subwindow;
    eventp->xany.window = *w;
  }
  if (*w == Scr.Root) {
    *context = C_ROOT;
    XBell(dpy, Scr.screen);
    UngrabEm();
    return TRUE;
  }
  if (XFindContext(dpy, *w, ScwmContext, (caddr_t *) tmp_win) == XCNOENT) {
    *tmp_win = NULL;
    XBell(dpy, Scr.screen);
    UngrabEm();
    return (TRUE);
  }
  if (*w == (*tmp_win)->Parent)
    *w = (*tmp_win)->w;

  if (original_w == (*tmp_win)->Parent)
    original_w = (*tmp_win)->w;

  /* this ugly mess attempts to ensure that the release and press
   * are in the same window. */
  if ((*w != original_w) && (original_w != Scr.Root) &&
      (original_w != None) && (original_w != Scr.NoFocusWin))
    if (!((*w == (*tmp_win)->frame) &&
	  (original_w == (*tmp_win)->w))) {
      *context = C_ROOT;
      XBell(dpy, Scr.screen);
      UngrabEm();
      return TRUE;
    }
  *context = GetContext(*tmp_win, eventp, &dummy);

  UngrabEm();
  return FALSE;
}

SCM 
select_window(SCM kill_p)
{
  XEvent ev;
  Window w;
  ScwmWindow *tmp_win;
  unsigned long context;

  SCM_REDEFER_INTS;
  w = Scr.Root;
  context = C_ROOT;

  tmp_win = &Scr.ScwmRoot;

  if (kill_p == SCM_UNDEFINED) {
    kill_p = SCM_BOOL_F;
  } else if (!gh_boolean_p(kill_p)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("select-window", 1, kill_p);
  }
  if (DeferExecution(&ev, &w, &tmp_win, &context,
		     (kill_p != SCM_BOOL_F ? DESTROY : SELECT),
		     ButtonRelease)) {
  }
  /* XXX - this needs to done right.  (Was != NULL before --10/24/97 gjb ) */
  if (tmp_win && tmp_win->schwin != SCM_UNDEFINED) {
    SCM_REALLOW_INTS;
    return (tmp_win->schwin);
  } else {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
}


SCM 
delete_window(SCM win)
{
  ScwmWindow *tmp_win;

  SCM_REDEFER_INTS;

  VALIDATEKILL(win, "delete-window");

  tmp_win = SCWMWINDOW(win);
  if (check_allowed_function2(F_DELETE, tmp_win) == 0) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  if (tmp_win->flags & DoesWmDeleteWindow) {
    send_clientmessage(dpy, tmp_win->w, _XA_WM_DELETE_WINDOW, CurrentTime);
    SCM_REALLOW_INTS;
    return SCM_BOOL_T;
  }
  SCM_REALLOW_INTS;
  return SCM_BOOL_F;
}

SCM 
destroy_window(SCM win)
{
  ScwmWindow *tmp_win;

  SCM_REDEFER_INTS;
  VALIDATEKILL(win, "destroy-window");
  tmp_win = SCWMWINDOW(win);
  if (check_allowed_function2(F_DESTROY, tmp_win) == 0) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  if (XGetGeometry(dpy, tmp_win->w, &JunkRoot, &JunkX, &JunkY,
		   &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth) == 0) {
    Destroy(tmp_win);
  } else {
    XKillClient(dpy, tmp_win->w);
  }
  XSync(dpy, 0);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
window_deletable_p(SCM win)
{
  VALIDATEKILL(win, "window-deletable?");
  return (SCWMWINDOW(win)->flags & DoesWmDeleteWindow) ?
    SCM_BOOL_T : SCM_BOOL_F;
}

void FocusOn(ScwmWindow * t, int DeIconifyOnly);

SCM 
focus(SCM win)
{
  ScwmWindow *tmp_win;

  SCM_REDEFER_INTS;
  VALIDATE(win, "focus");
  tmp_win = SCWMWINDOW(win);
  FocusOn(tmp_win, 0);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

void WarpOn(ScwmWindow * t, int warp_x, int x_unit, int warp_y, int y_unit);

SCM 
warp_to_window(SCM win)
{
  SCM_REDEFER_INTS;
  VALIDATE(win, "warp-to-window");
  WarpOn(SCWMWINDOW(win), 0, 0, 0, 0);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}



SCM 
raise_window(SCM win)
{
  ScwmWindow *tmp_win;
#ifdef FIXGJB_UNUSED_VARIABLES
  char *junk;
  char *junkC;
  unsigned long junkN;
  int junkD, method, BoxJunk[4];
#endif

  SCM_REDEFER_INTS;
  VALIDATE(win, "raise-window");

  tmp_win = SCWMWINDOW(win);

  RaiseWindow(tmp_win);
  /* FIXMS darn, this is not going to do what we want it to -- must
     start keeping a general stays on top flag as well a currently on
     top flag in the window struct, only the latter of which is
     changed by raises and lowers. */
  KeepOnTop();
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}


SCM 
lower_window(SCM win)
{
  SCM_REDEFER_INTS;
  VALIDATE(win, "lower-window");
  LowerWindow(SCWMWINDOW(win));
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}


SCM 
raised_p(SCM win)
{
  ScwmWindow *tmp_win;

  VALIDATE(win, "raised?");
  tmp_win = SCWMWINDOW(win);
  return ((tmp_win == Scr.LastWindowRaised) ||
	  (tmp_win->flags & VISIBLE) ? SCM_BOOL_T : SCM_BOOL_F);
}



SCM 
iconify(SCM win)
{
  ScwmWindow *tmp_win;

  SCM_REDEFER_INTS;
  VALIDATE(win, "iconify");
  tmp_win = SCWMWINDOW(win);

  if (check_allowed_function2(F_ICONIFY, tmp_win) == 0) {

    return SCM_BOOL_F;
  }
  Iconify(tmp_win, 0, 0);

  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
deiconify(SCM win)
{
  SCM_REDEFER_INTS;
  VALIDATE(win, "deiconify");
  DeIconify(SCWMWINDOW(win));
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
iconified_p(SCM win)
{
  VALIDATE(win, "iconified?");
  return ((SCWMWINDOW(win)->flags & ICONIFIED) ?
	  SCM_BOOL_T : SCM_BOOL_F);
}


SCM 
stick(SCM win)
{
  ScwmWindow *tmp_win;

  SCM_REDEFER_INTS;
  VALIDATE(win, "stick");
  tmp_win = SCWMWINDOW(win);
  tmp_win->flags |= STICKY;
  BroadcastConfig(M_CONFIGURE_WINDOW, tmp_win);
  SetTitleBar(tmp_win, (Scr.Hilite == tmp_win), True);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
unstick(SCM win)
{
  ScwmWindow *tmp_win;

  SCM_REDEFER_INTS;
  VALIDATE(win, "unstick");
  tmp_win = SCWMWINDOW(win);
  tmp_win->flags &= ~STICKY;
  BroadcastConfig(M_CONFIGURE_WINDOW, tmp_win);
  SetTitleBar(tmp_win, (Scr.Hilite == tmp_win), True);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
sticky_p(SCM win)
{
  VALIDATE(win, "sticky?");
  return (SCWMWINDOW(win)->flags & STICKY) ? SCM_BOOL_T : SCM_BOOL_F;
}




/***********************************************************************
 *
 *  WindowShade -- shades or unshades a window (veliaa@rpi.edu)
 ***********************************************************************/

/* Modified for scwm by mstachow@mit.edu */

SCM 
window_shade(SCM win)
{
  ScwmWindow *tmp_win;

  SCM_REDEFER_INTS;
  VALIDATE(win, "window-shade");
  tmp_win = SCWMWINDOW(win);

  if (!(tmp_win->flags & TITLE) || (tmp_win->flags & MAXIMIZED)) {
    return SCM_BOOL_F;
  }
  tmp_win->buttons |= WSHADE;
  SetupFrame(tmp_win,
	     tmp_win->frame_x,
	     tmp_win->frame_y,
	     tmp_win->frame_width,
	     tmp_win->title_height + tmp_win->boundary_width,
	     False);
  Broadcast(M_WINDOWSHADE, 1, tmp_win->w, 0, 0, 0, 0, 0, 0);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
un_window_shade(SCM win)
{
  ScwmWindow *tmp_win;

  SCM_REDEFER_INTS;
  VALIDATE(win, "un-window-shade");
  tmp_win = SCWMWINDOW(win);

  tmp_win->buttons &= ~WSHADE;
  SetupFrame(tmp_win,
	     tmp_win->frame_x,
	     tmp_win->frame_y,
	     tmp_win->orig_wd,
	     tmp_win->orig_ht,
	     True);
  Broadcast(M_DEWINDOWSHADE, 1, tmp_win->w, 0, 0, 0, 0, 0, 0);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
window_shaded_p(SCM win)
{
  VALIDATE(win, "window-shaded?");
  return ((SCWMWINDOW(win)->buttons & WSHADE) ? SCM_BOOL_T : SCM_BOOL_F);
}



void 
move_finalize(Window w, ScwmWindow * tmp_win, int x, int y)
{
  if (w == tmp_win->frame) {
    SetupFrame(tmp_win, x, y,
	       tmp_win->frame_width, tmp_win->frame_height, FALSE);
  } else {			/* icon window */
    tmp_win->flags |= ICON_MOVED;
    tmp_win->icon_x_loc = x;
    tmp_win->icon_xl_loc = y -
      (tmp_win->icon_w_width - tmp_win->icon_p_width) / 2;
    tmp_win->icon_y_loc = y;
    Broadcast(M_ICON_LOCATION, 7, tmp_win->w, tmp_win->frame,
	      (unsigned long) tmp_win,
	      tmp_win->icon_x_loc, tmp_win->icon_y_loc,
	      tmp_win->icon_w_width, tmp_win->icon_w_height
	      + tmp_win->icon_p_height);
    XMoveWindow(dpy, tmp_win->icon_w,
		tmp_win->icon_xl_loc, y + tmp_win->icon_p_height);
    if (tmp_win->icon_pixmap_w != None) {
      XMapWindow(dpy, tmp_win->icon_w);
      XMoveWindow(dpy, tmp_win->icon_pixmap_w, tmp_win->icon_x_loc, y);
      XMapWindow(dpy, w);
    }
  }
}



extern float rgpctMovementDefault[32];
extern int cpctMovementDefault;
extern int cmsDelayDefault;
extern int c10msDelaysBeforePopup;


/* set animation parameters */
SCM
set_animation_x(SCM vector)
{
  int citems;
  int i;
  int iarg = 1;
/*
  FIXGJB: make a scheme-variable move-animation-delay get used instead
  if (!gh_int_p(delay) && !gh_boolean_p(delay)) {
    scm_wrong_type_arg(__FUNCTION__,iarg++,delay);
  }
  */
  if (!gh_vector_p(vector)) {
    scm_wrong_type_arg(__FUNCTION__,iarg++,vector);
  }
/*
  if (gh_int_p(delay)) {
    cmsDelayDefault = gh_scm2int(delay);
  }
  */
  citems = gh_vector_length(vector);
  for (i=0; i<citems; i++) {
#ifdef HAVE_GH_VECTOR_REF
    SCM val = gh_vector_ref(vector,gh_int2scm(i));    
#else /* HAVE_GH_VECTOR_REF */
    SCM val = gh_vref(vector,gh_int2scm(i));
#endif
    if (!gh_number_p(val)) {
      scm_wrong_type_arg(__FUNCTION__,iarg-1,vector);
    }
    /* FIXGJB: also check < 2, perhaps (don't want to
      check < 1, since we might want to overshoot and then come back) */
    rgpctMovementDefault[i] = (float) gh_scm2double(val);
  }
  /* Ensure that we end up 100% of the way to our destination */
  if (i>0 && rgpctMovementDefault[i-1] != 1.0) {
    rgpctMovementDefault[i++] = 1.0;
  }
  return SCM_UNDEFINED;
}
 

SCM 
move_to(SCM x, SCM y, SCM win, SCM animated_p, SCM move_pointer_too_p)
{
  ScwmWindow *tmp_win;
  Window w;
  Bool fMovePointer = False;
  Bool fAnimated = False;
  int startX, startY;
  int destX, destY;

  SCM_REDEFER_INTS;
  VALIDATEN(win, 3, "move-to");
  if (!gh_number_p(x)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("move-to", 1, x);
  }
  if (!gh_number_p(y)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("move-to", 2, y);
  }
#ifdef GJB_BE_ANAL_ABOUT_BOOLS
  /* FIXGJB: I took this code out so I can say:
     (move-to x y (get-window) 'animated 'move-pointer)
     Is there a better way? */
  if (!gh_boolean_p(animated_p)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("move-to", 4, animated_p);
  }
  if (!gh_boolean_p(move_pointer_too_p)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("move-to", 5, move_pointer_too_p);
  }
#endif
  if (animated_p == SCM_UNDEFINED) {
    /* FIXGJB: make an option for allowing the default to be animated */
    animated_p = SCM_BOOL_F;
  }
  if (move_pointer_too_p == SCM_UNDEFINED) {
    /* This is the only sensible default */
    move_pointer_too_p = SCM_BOOL_F;
  }
  tmp_win = SCWMWINDOW(win);
  w = tmp_win->frame;
  if (tmp_win->flags & ICONIFIED) {
    if (tmp_win->icon_pixmap_w != None) {
      XUnmapWindow(dpy, tmp_win->icon_w);
      w = tmp_win->icon_pixmap_w;
    } else
      w = tmp_win->icon_w;
  }
  destX = gh_scm2int(x);
  destY = gh_scm2int(y);
  fMovePointer = gh_scm2bool(move_pointer_too_p);
  fAnimated = gh_scm2bool(animated_p);
  if (fMovePointer || fAnimated) {
    XGetGeometry(dpy,w,&JunkRoot,
		 &startX, &startY,
		 &JunkX,&JunkY,&JunkX,&JunkY);
  }
  if (fAnimated) {
    SCM animation_ms_delay = gh_lookup("animation-ms-delay");
    int cmsDelay = -1;
    if (animation_ms_delay != SCM_UNDEFINED &&
	gh_number_p(animation_ms_delay)) {
      cmsDelay = gh_scm2int(animation_ms_delay);
    }
    AnimatedMoveWindow(w,startX,startY,destX,destY,
		       fMovePointer,cmsDelay,NULL);
  } else if (fMovePointer) {
    int x, y;
    XQueryPointer(dpy, Scr.Root, &JunkRoot, &JunkChild,
		  &x, &y, &JunkX, &JunkY, &JunkMask);
    XWarpPointer(dpy, Scr.Root, Scr.Root, 0, 0, Scr.MyDisplayWidth,
		 Scr.MyDisplayHeight, x + destX - startX, y + destY - startY);
  }

  move_finalize(w, tmp_win, destX, destY);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

extern int orig_x, orig_y, have_orig_position;

SCM 
interactive_move(SCM win)
{
  ScwmWindow *tmp_win;
  Window w;
  XEvent event;
  int x, y;

  SCM_REDEFER_INTS;
  VALIDATE(win, "interactive-move");
  tmp_win = SCWMWINDOW(win);
  w = tmp_win->frame;
  if (tmp_win->flags & ICONIFIED) {
    if (tmp_win->icon_pixmap_w != None) {
      XUnmapWindow(dpy, tmp_win->icon_w);
      w = tmp_win->icon_pixmap_w;
    } else {
      w = tmp_win->icon_w;
    }
  }
  if (have_orig_position) {
    event.xbutton.x_root = orig_x;
    event.xbutton.y_root = orig_y;
  } else {
    XQueryPointer(dpy, Scr.Root, &JunkRoot, &JunkChild,
		  &event.xbutton.x_root, &event.xbutton.y_root,
		  &JunkX, &JunkY, &JunkMask);
  }
  InteractiveMove(&w, tmp_win, &x, &y, &event);
  move_finalize(w, tmp_win, x, y);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
resize_to(SCM w, SCM h, SCM win)
{
  int width, height;
  ScwmWindow *tmp_win;

  SCM_REDEFER_INTS;
  VALIDATEN(win, 3, "resize-to");
  tmp_win = SCWMWINDOW(win);

  if (check_allowed_function2(F_RESIZE, tmp_win) == 0
      || (tmp_win->buttons & WSHADE)
    ) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  tmp_win->flags &= ~MAXIMIZED;

  /* can't resize icons */
  if (tmp_win->flags & ICONIFIED) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  width = gh_scm2int(w);
  height = gh_scm2int(h);

  /* took the next two lines out because we can do that in scheme if we 
     really want, and maximize gets broken otherwise. */
  /*
     width += (2*tmp_win->boundary_width);
     height += (tmp_win->title_height + 2*tmp_win->boundary_width);
   */

  ConstrainSize(tmp_win, &width, &height);
  SetupFrame(tmp_win, tmp_win->frame_x,
	     tmp_win->frame_y, width, height, FALSE);

  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}



extern int dragx;		/* all these variables are used */
extern int dragy;		/* in resize operations */
extern int dragWidth;
extern int dragHeight;

extern int origx;
extern int origy;
extern int origWidth;
extern int origHeight;

extern int ymotion, xmotion;
extern int last_width, last_height;
extern int menuFromFrameOrWindowOrTitlebar;
extern Window PressedW;

SCM 
interactive_resize(SCM win)
{
  ScwmWindow *tmp_win;
  Bool finished = FALSE, done = FALSE, abort = FALSE;
  int x, y, delta_x, delta_y;
  Window ResizeWindow;
  extern int Stashed_X, Stashed_Y;
  Bool flags;

  VALIDATE(win, "interactive-resize");
  tmp_win = SCWMWINDOW(win);


  if (check_allowed_function2(F_RESIZE, tmp_win) == 0
      || (tmp_win->buttons & WSHADE)
    ) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  tmp_win->flags &= ~MAXIMIZED;

  if (tmp_win->flags & ICONIFIED) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  ResizeWindow = tmp_win->frame;


  InstallRootColormap();
  if (menuFromFrameOrWindowOrTitlebar) {
    /* warp the pointer to the cursor position from before menu appeared */
    XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, Stashed_X, Stashed_Y);
    XFlush(dpy);
  }
  if (!GrabEm(MOVE)) {
    XBell(dpy, Scr.screen);
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  XGrabServer_withSemaphore(dpy);

  /* handle problems with edge-wrapping while resizing */
  flags = Scr.flags;
  Scr.flags &= ~(EdgeWrapX | EdgeWrapY);

  XGetGeometry(dpy, (Drawable) ResizeWindow, &JunkRoot,
	       &dragx, &dragy, (unsigned int *) &dragWidth,
	       (unsigned int *) &dragHeight, &JunkBW, &JunkDepth);

  dragx += tmp_win->bw;
  dragy += tmp_win->bw;
  origx = dragx;
  origy = dragy;
  origWidth = dragWidth;
  origHeight = dragHeight;
  ymotion = xmotion = 0;

  /* pop up a resize dimensions window */
  XMapRaised(dpy, Scr.SizeWindow);
  last_width = 0;
  last_height = 0;
  DisplaySize(tmp_win, origWidth, origHeight, True);

  /* Get the current position to determine which border to resize */
  if ((PressedW != Scr.Root) && (PressedW != None)) {
    if (PressedW == tmp_win->sides[0])	/* top */
      ymotion = 1;
    if (PressedW == tmp_win->sides[1])	/* right */
      xmotion = -1;
    if (PressedW == tmp_win->sides[2])	/* bottom */
      ymotion = -1;
    if (PressedW == tmp_win->sides[3])	/* left */
      xmotion = 1;
    if (PressedW == tmp_win->corners[0]) {	/* upper-left */
      ymotion = 1;
      xmotion = 1;
    }
    if (PressedW == tmp_win->corners[1]) {	/* upper-right */
      xmotion = -1;
      ymotion = 1;
    }
    if (PressedW == tmp_win->corners[2]) {	/* lower left */
      ymotion = -1;
      xmotion = 1;
    }
    if (PressedW == tmp_win->corners[3]) {	/* lower right */
      ymotion = -1;
      xmotion = -1;
    }
  }
  /* draw the rubber-band window */
  MoveOutline(Scr.Root, dragx - tmp_win->bw, dragy - tmp_win->bw,
	      dragWidth + 2 * tmp_win->bw,
	      dragHeight + 2 * tmp_win->bw);

  /* loop to resize */
  while (!finished) {
    XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask | KeyPressMask |
	       ButtonMotionMask | PointerMotionMask | ExposureMask, &Event);
    StashEventTime(&Event);

    if (Event.type == MotionNotify)
      /* discard any extra motion events before a release */
      while (XCheckMaskEvent(dpy, ButtonMotionMask | ButtonReleaseMask |
			     PointerMotionMask, &Event)) {
	StashEventTime(&Event);
	if (Event.type == ButtonRelease)
	  break;
      }
    done = FALSE;
    /* Handle a limited number of key press events to allow mouseless
     * operation */
    if (Event.type == KeyPress)
      Keyboard_shortcuts(&Event, ButtonRelease);
    switch (Event.type) {
    case ButtonPress:
      XAllowEvents(dpy, ReplayPointer, CurrentTime);
    case KeyPress:
      /* simple code to bag out of move - CKH */
      if (XLookupKeysym(&(Event.xkey), 0) == XK_Escape) {
	abort = TRUE;
	finished = TRUE;
      }
      done = TRUE;
      break;

    case ButtonRelease:
      finished = TRUE;
      done = TRUE;
      break;

    case MotionNotify:
      x = Event.xmotion.x_root;
      y = Event.xmotion.y_root;
      /* resize before paging request to prevent resize from lagging mouse - mab */
      DoResize(x, y, tmp_win);
      /* need to move the viewport */
      HandlePaging(Scr.EdgeScrollX, Scr.EdgeScrollY, &x, &y,
		   &delta_x, &delta_y, False);
      /* redraw outline if we paged - mab */
      if ((delta_x != 0) || (delta_y != 0)) {
	origx -= delta_x;
	origy -= delta_y;
	dragx -= delta_x;
	dragy -= delta_y;

	DoResize(x, y, tmp_win);
      }
      done = TRUE;
    default:
      break;
    }
    if (!done) {
      MoveOutline(Scr.Root, 0, 0, 0, 0);

      DispatchEvent();

      MoveOutline(Scr.Root, dragx - tmp_win->bw, dragy - tmp_win->bw,
		  dragWidth + 2 * tmp_win->bw, dragHeight + 2 * tmp_win->bw);

    }
  }

  /* erase the rubber-band */
  MoveOutline(Scr.Root, 0, 0, 0, 0);

  /* pop down the size window */
  XUnmapWindow(dpy, Scr.SizeWindow);

  if (!abort) {
    ConstrainSize(tmp_win, &dragWidth, &dragHeight);
    SetupFrame(tmp_win, dragx - tmp_win->bw,
	       dragy - tmp_win->bw, dragWidth, dragHeight, FALSE);
  }
  UninstallRootColormap();
  ResizeWindow = None;
  XUngrabServer_withSemaphore(dpy);
  UngrabEm();
  xmotion = 0;
  ymotion = 0;

  Scr.flags |= flags & (EdgeWrapX | EdgeWrapY);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
refresh_window(SCM win)
{
  ScwmWindow *tmp_win;

  SCM_REDEFER_INTS;
  VALIDATE(win, "refresh-window");
  tmp_win = SCWMWINDOW(win);

  refresh_common((tmp_win->flags & ICONIFIED) ?
		 (tmp_win->icon_w) : (tmp_win->frame));

  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}


SCM 
move_window_to_desk(SCM which, SCM win)
{
  ScwmWindow *t;
  int val1;

  SCM_REDEFER_INTS;
  if (!gh_number_p(which) && which != SCM_BOOL_F) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("move-window-to-desk", 1, which);
  }
  VALIDATEN(win, 2, "move-window-to-desk");

  t = SCWMWINDOW(win);

  val1 = gh_scm2int(which);

  /* Mapping window on its new Desk,
     unmapping it from the old Desk */
  /* Only change mapping for non-sticky windows */
  if (!((t->flags & ICONIFIED) && (t->flags & StickyIcon)) &&
      (!(t->flags & STICKY)) && (!(t->flags & ICON_UNMAPPED))) {
    if (t->Desk == Scr.CurrentDesk) {
      t->Desk = val1;
      if (val1 != Scr.CurrentDesk) {
	UnmapIt(t);
      }
    } else if (val1 == Scr.CurrentDesk) {
      t->Desk = val1;
      /* If its an icon, auto-place it */
      if (t->flags & ICONIFIED)
	AutoPlace(t);
      MapIt(t);
    } else {
      t->Desk = val1;
    }
  }
  BroadcastConfig(M_CONFIGURE_WINDOW, t);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;;
}


SCM 
window_position(SCM win)
{
  ScwmWindow *tmp_win;

  VALIDATE(win, "window-position");
  tmp_win = SCWMWINDOW(win);

  return scm_listify(SCM_MAKINUM(tmp_win->frame_x),
		     SCM_MAKINUM(tmp_win->frame_y),
		     SCM_UNDEFINED);
}

SCM 
window_size(SCM win)
{
  ScwmWindow *tmp_win;

  VALIDATE(win, "window-size");
  tmp_win = SCWMWINDOW(win);

  return scm_listify(SCM_MAKINUM(tmp_win->frame_width),
		     SCM_MAKINUM(tmp_win->frame_height),
		     SCM_UNDEFINED);
}

SCM 
window_id(SCM win)
{
  VALIDATE(win, "window-id");
  return SCM_MAKINUM(SCWMWINDOW(win)->w);
}

SCM 
window_desk(SCM win)
{
  VALIDATE(win, "window-desk");
  return SCM_MAKINUM(SCWMWINDOW(win)->Desk);
}

SCM 
window_title(SCM win)
{
  VALIDATE(win, "window-title");
  return gh_str02scm(SCWMWINDOW(win)->name);
}

SCM 
window_class(SCM win)
{
  VALIDATE(win, "window-class");
  return gh_str02scm(SCWMWINDOW(win)->class.res_class);
}

SCM 
window_resource(SCM win)
{
  VALIDATE(win, "window-resource");
  return gh_str02scm(SCWMWINDOW(win)->class.res_name);
}


SCM 
list_all_windows()
{
  ScwmWindow *t;
  SCM result = SCM_EOL;

  SCM_REDEFER_INTS;
  for (t = Scr.ScwmRoot.next; NULL != t; t = t->next) {
    result = scm_cons(t->schwin, result);
  }
  SCM_REALLOW_INTS;
  return result;
}


SCM 
keep_on_top(SCM win)
{
  ScwmWindow *tmp_win;

  SCM_REDEFER_INTS;
  VALIDATE(win, "keep-on-top");
  tmp_win = SCWMWINDOW(win);
  tmp_win->flags |= ONTOP;
  /* is this needed? */
  BroadcastConfig(M_CONFIGURE_WINDOW, tmp_win);
  raise_window(win);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
un_keep_on_top(SCM win)
{
  ScwmWindow *tmp_win;

  SCM_REDEFER_INTS;
  VALIDATE(win, "un-keep-on-top");
  tmp_win = SCWMWINDOW(win);
  tmp_win->flags &= ~ONTOP;
  /* is this needed? */
  BroadcastConfig(M_CONFIGURE_WINDOW, tmp_win);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
kept_on_top_p(SCM win)
{
  VALIDATE(win, "kept-on-top?");
  return (SCWMWINDOW(win)->flags & ONTOP) ? SCM_BOOL_T : SCM_BOOL_F;
}


/* maybe all of this can be replaced with set-title-height 
   (a per-window version) ? */

SCM 
show_titlebar(SCM win)
{
  ScwmWindow *tmp_win;
  ScwmDecor *fl;

  SCM_REDEFER_INTS;
  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  VALIDATE(win, "show-titlebar");
  tmp_win = SCWMWINDOW(win);
  if (!(tmp_win->flags & TITLE)) {
    tmp_win->flags |= TITLE;
    BroadcastConfig(M_CONFIGURE_WINDOW, tmp_win);
    SetupFrame(tmp_win, tmp_win->frame_x, tmp_win->frame_y,
	       tmp_win->frame_width,
	       tmp_win->frame_height + fl->TitleHeight,
	       True);
    /* SetTitleBar(tmp_win,(Scr.Hilite==tmp_win),True); */
  }
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
hide_titlebar(SCM win)
{
  ScwmWindow *tmp_win;
  ScwmDecor *fl;

  SCM_REDEFER_INTS;
  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  VALIDATE(win, "hide-titlebar");
  tmp_win = SCWMWINDOW(win);
  if (tmp_win->flags & TITLE) {
    tmp_win->flags &= ~TITLE;
    tmp_win->title_height = 0;
    BroadcastConfig(M_CONFIGURE_WINDOW, tmp_win);
    SetupFrame(tmp_win, tmp_win->frame_x, tmp_win->frame_y,
	       tmp_win->frame_width,
	       tmp_win->frame_height - fl->TitleHeight,
	       True);
  }
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
titlebar_shown_p(SCM win)
{
  VALIDATE(win, "titlebar-shown?");
  return (SCWMWINDOW(win)->flags & TITLE) ? SCM_BOOL_T : SCM_BOOL_F;
}


SCM 
normal_border(SCM win)
{
  ScwmWindow *tmp_win;
  ScwmDecor *fl;
  int i;

  SCM_REDEFER_INTS;
  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  VALIDATE(win, "normal-border");
  tmp_win = SCWMWINDOW(win);
  tmp_win->flags |= BORDER;
  BroadcastConfig(M_CONFIGURE_WINDOW, tmp_win);
  for (i = 0; i < 4; i++) {
    XMapWindow(dpy, tmp_win->corners[i]);
    XMapWindow(dpy, tmp_win->sides[i]);
  }

  SetBorderX(tmp_win, (Scr.Hilite == tmp_win), True, True, None, True);

  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
plain_border(SCM win)
{
  ScwmWindow *tmp_win;
#ifdef FIXGJB_UNUSED_VARIABLES
  ScwmDecor *fl;
#endif
  int i;

  SCM_REDEFER_INTS;
  /* FIXGJB: what the heck does fl get used for?? */
#ifdef FIXGJB_UNUSED_VARIABLES
  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;
#endif

  VALIDATE(win, "plain-border");
  tmp_win = SCWMWINDOW(win);
  tmp_win->flags &= ~BORDER;
  BroadcastConfig(M_CONFIGURE_WINDOW, tmp_win);

  for (i = 0; i < 4; i++) {
    XUnmapWindow(dpy, tmp_win->corners[i]);
    XUnmapWindow(dpy, tmp_win->sides[i]);
  }

  SetBorderX(tmp_win, (Scr.Hilite == tmp_win), True, True, None, True);

  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
border_normal_p(SCM win)
{
  VALIDATE(win, "border-normal?");
  return (SCWMWINDOW(win)->flags & BORDER) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM 
set_border_width_x(SCM width, SCM win)
{
  ScwmWindow *tmp_win;
  ScwmDecor *fl;
  int w, oldw;

  SCM_REDEFER_INTS;
  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (!gh_number_p(width)) {
    scm_wrong_type_arg("set-border-width!", 1, width);
  }
  w = gh_scm2int(width);

  VALIDATEN(win, 2, "set-border-width!");
  tmp_win = SCWMWINDOW(win);
  oldw = tmp_win->boundary_width;
  tmp_win->boundary_width = w;

  SetupFrame(tmp_win, tmp_win->frame_x, tmp_win->frame_y,
	     tmp_win->frame_width + 2 * (w - oldw),
	     tmp_win->frame_height + 2 * (w - oldw),
	     True);


  BroadcastConfig(M_CONFIGURE_WINDOW, tmp_win);
  return SCM_BOOL_T;
}

SCM 
stick_icon(SCM win)
{
  ScwmWindow *tmp_win;

  SCM_REDEFER_INTS;
  VALIDATE(win, "stick-icon");
  tmp_win = SCWMWINDOW(win);
  tmp_win->flags |= StickyIcon;
  BroadcastConfig(M_CONFIGURE_WINDOW, tmp_win);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
unstick_icon(SCM win)
{
  ScwmWindow *tmp_win;

  SCM_REDEFER_INTS;
  VALIDATE(win, "unstick-icon");
  tmp_win = SCWMWINDOW(win);
  tmp_win->flags &= ~StickyIcon;
  BroadcastConfig(M_CONFIGURE_WINDOW, tmp_win);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
icon_sticky_p(SCM win)
{
  VALIDATE(win, "icon-sticky?");
  return (SCWMWINDOW(win)->flags & StickyIcon) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM 
set_icon_box_x(SCM sx, SCM sy, SCM sw, SCM sh, SCM win)
{
  /* XXX - should probably move existing window icons */
  int x, y, w, h;
  ScwmWindow *tmp_win;

  if (!gh_number_p(sx)) {
    scm_wrong_type_arg("set-icon-box!", 1, sx);
  }
  if (!gh_number_p(sy)) {
    scm_wrong_type_arg("set-icon-box!", 2, sy);
  }
  if (!gh_number_p(sw)) {
    scm_wrong_type_arg("set-icon-box!", 3, sw);
  }
  if (!gh_number_p(sh)) {
    scm_wrong_type_arg("set-icon-box!", 4, sh);
  }
  VALIDATEN(win, 5, "set-icon-box!");
  tmp_win = SCWMWINDOW(win);
  x = gh_scm2int(sx);
  y = gh_scm2int(sy);
  w = gh_scm2int(sw);
  h = gh_scm2int(sh);
  tmp_win->IconBox[0] = x;
  tmp_win->IconBox[1] = y;
  tmp_win->IconBox[2] = x + w;
  tmp_win->IconBox[3] = y + h;
  return (SCM_BOOL_T);
}


SCM sym_mouse, sym_click, sym_sloppy, sym_none;

void 
init_window()
{
  sym_mouse = gh_symbol2scm("mouse");
  scm_protect_object(sym_mouse);
  sym_click = gh_symbol2scm("click");
  scm_protect_object(sym_click);
  sym_sloppy = gh_symbol2scm("sloppy");
  scm_protect_object(sym_sloppy);
  sym_none = gh_symbol2scm("none");
  scm_protect_object(sym_none);
}


SCM 
set_window_focus_x(SCM sym, SCM win)
{
  ScwmWindow *tmp_win;

  if (!gh_symbol_p(sym)) {
    scm_wrong_type_arg("set-window-focus!", 1, sym);
  }
  VALIDATEN(win, 2, "set-window-focus!");
  tmp_win = SCWMWINDOW(win);

  if (gh_eq_p(sym, sym_mouse)) {
    tmp_win->flags &= ~ClickToFocus & ~SloppyFocus;
  } else if (gh_eq_p(sym, sym_click)) {
    tmp_win->flags &= ~SloppyFocus;
    tmp_win->flags |= ClickToFocus;
  } else if (gh_eq_p(sym, sym_sloppy)) {
    tmp_win->flags &= ~ClickToFocus;
    tmp_win->flags |= SloppyFocus;
  } else if (gh_eq_p(sym, sym_none)) {
    tmp_win->flags |= ClickToFocus | SloppyFocus;
  } else {
    scwm_error("set-window-focus!", 13);
  }
  return sym;
}

SCM 
set_window_colors_x(SCM fg, SCM bg, SCM win)
{
  ScwmWindow *tmp_win;

  if (gh_string_p(fg)) {
    fg = load_color(fg);
  } else if (fg == SCM_UNDEFINED) {
    fg = SCM_BOOL_F;
  } else if (!((SCM_NIMP(fg) && COLORP(fg)) || fg == SCM_BOOL_F)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-window-colors!", 1, fg);
  }
  if (gh_string_p(bg)) {
    bg = load_color(bg);
  } else if (bg == SCM_UNDEFINED) {
    bg = SCM_BOOL_F;
  } else if (!((SCM_NIMP(bg) && COLORP(bg)) || bg == SCM_BOOL_F)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("set-window-colors!", 2, bg);
  }
  VALIDATEN(win, 3, "set-window-colors!");
  tmp_win = SCWMWINDOW(win);

  if (fg != SCM_BOOL_F) {
    tmp_win->TextPixel = COLOR(fg);
  }
  if (bg != SCM_BOOL_F) {
    tmp_win->BackPixel = COLOR(bg);
    tmp_win->ShadowPixel = GetShadow(tmp_win->BackPixel);
    tmp_win->ReliefPixel = GetHilite(tmp_win->BackPixel);
  }
  SetBorderX(tmp_win, (Scr.Hilite == tmp_win), True, True, None, True);

  return SCM_BOOL_T;
}

SCM 
set_icon_title_x(SCM title, SCM win)
{
  int dummy;
  ScwmWindow *tmp_win;

  /* XXX - this should redraw the icon if necessary */

  VALIDATEN(win, 2, "set-icon-title!");
  tmp_win = SCWMWINDOW(win);

  if (!gh_string_p(title)) {
    if (title == SCM_BOOL_F) {
      tmp_win->flags |= NOICON_TITLE;
      return SCM_BOOL_T;
    } else {
      scm_wrong_type_arg("set-icon-title!", 1, title);
    }
  }
  tmp_win->icon_name = gh_scm2newstr(title, &dummy);
  tmp_win->flags &= ~NOICON_TITLE;

  return SCM_BOOL_T;
}



SCM 
set_random_placement_x(SCM val, SCM win)
{
  VALIDATEN(win, 2, "set-random-placement!");
  if (val == SCM_BOOL_T) {
    SCWMWINDOW(win)->flags |= RANDOM_PLACE_FLAG;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->flags &= ~RANDOM_PLACE_FLAG;
  } else {
    scm_wrong_type_arg("set-random-placement!", 1, val);
  }
  return SCM_BOOL_T;
}

SCM 
set_smart_placement_x(SCM val, SCM win)
{
  VALIDATEN(win, 2, "set-smart-pacement!");
  if (val == SCM_BOOL_T) {
    SCWMWINDOW(win)->flags |= MWM_OVERRIDE_FLAG;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->flags &= ~MWM_OVERRIDE_FLAG;
  } else {
    scm_wrong_type_arg("set-smart-placment!", 1, val);
  }
  return SCM_BOOL_T;
}


SCM 
set_window_button_x(SCM butt, SCM val, SCM win)
{
  VALIDATEN(win, 2, "set-window-button!");
  if (val == SCM_BOOL_T) {
    SCWMWINDOW(win)->buttons &= ~(1 << (butt - 1));
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->buttons |= (1 << (butt - 1));
  } else {
    scm_wrong_type_arg("set-window-button!", 1, val);
  }
  /* XXX - This won't really work for any case unless it is a hint.
     Handling of the number of buttons is kind of broken in
     general for now, but will be fixed. */

  return SCM_BOOL_T;
}

SCM 
set_mwm_buttons_x(SCM val, SCM win)
{
  ScwmWindow *t;

  VALIDATEN(win, 2, "set-mwm-buttons!");
  t = SCWMWINDOW(win);

  if (val == SCM_BOOL_T) {
    SCWMWINDOW(win)->flags |= MWM_BUTTON_FLAG;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->flags &= ~MWM_BUTTON_FLAG;
  } else {
    scm_wrong_type_arg("set-mwm-buttons!", 1, val);
  }

  /* SetBorder(t,(Scr.Hilite==t),True,True,None); */
  return SCM_BOOL_T;
}

SCM 
set_mwm_border_x(SCM val, SCM win)
{
  ScwmWindow *t;

  VALIDATEN(win, 2, "set-mwm-border!");
  t = SCWMWINDOW(win);

  if (val == SCM_BOOL_T) {
    SCWMWINDOW(win)->flags |= MWMBorders;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->flags &= ~MWMBorders;
  } else {
    scm_wrong_type_arg("set-mwm-border!", 1, val);
  }

  SetBorderX(t, (Scr.Hilite == t), True, True, None, True);

  return SCM_BOOL_T;
}

SCM 
set_icon_x(SCM picture, SCM win)
{
  ScwmWindow *tmp_win;

  VALIDATEN(win, 2, "set-icon!");
  tmp_win = SCWMWINDOW(win);
  if (picture == SCM_BOOL_F) {
    tmp_win->flags |= SUPPRESSICON_FLAG;
    XDestroyWindow(dpy, tmp_win->icon_w);
    tmp_win->icon_w = None;
    tmp_win->szIconFile = NULL;
  } else if (picture == SCM_BOOL_T) {
    tmp_win->flags &= ~SUPPRESSICON_FLAG;
  } else if (PICTURE_P(picture)) {
    tmp_win->flags &= ~SUPPRESSICON_FLAG;
    tmp_win->flags |= ICON_FLAG;
    /* XXX -This is silly, we really should have an "icon" or "image" type. */
    if (!((tmp_win->wmhints)
	  && (tmp_win->wmhints->flags &
	      (IconWindowHint | IconPixmapHint)))) {
      tmp_win->picIcon = PICTURE(picture)->pic;
      /* FIXGJB: shouldn't have to set the filename */
      tmp_win->szIconFile = tmp_win->picIcon->name;
      XDestroyWindow(dpy, tmp_win->icon_w);
      tmp_win->icon_w = None;
    }
  } else {
    scm_wrong_type_arg("set-icon!", 1, picture);
  }
  /* also it should redraw automatically */
  if (tmp_win->flags & ICONIFIED) {
    Iconify(tmp_win, 0, 0);
  }
  return SCM_BOOL_T;
}

SCM 
set_mini_icon_x(SCM picture, SCM win)
{
  ScwmWindow *tmp_win;

  VALIDATEN(win, 2, "set-mini-icon!");
  tmp_win = SCWMWINDOW(win);
  if (picture == SCM_BOOL_F) {
    tmp_win->picMiniIcon = NULL;
  } else if (PICTURE_P(picture)) {
    tmp_win->picMiniIcon = PICTURE(picture)->pic;
  } else {
    scm_wrong_type_arg("set-mini-icon!", 1, picture);
  }

  /* FIXNOWGJB: also it should redraw automatically */

  return SCM_BOOL_T;

}

SCM 
set_hint_override_x(SCM val, SCM win)
{
  VALIDATEN(win, 2, "set-hint-override!");
  if (val == SCM_BOOL_T) {
    SCWMWINDOW(win)->flags |= MWM_OVERRIDE_FLAG;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->flags &= ~MWM_OVERRIDE_FLAG;
  } else {
    scm_wrong_type_arg("set-hint-override!", 1, val);
  }
  return SCM_BOOL_T;
}

SCM 
set_decorate_transient_x(SCM val, SCM win)
{
  VALIDATEN(win, 2, "set-decorate-transient!");
  if (val == SCM_BOOL_T) {
    SCWMWINDOW(win)->flags |= DECORATE_TRANSIENT_FLAG;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->flags &= ~DECORATE_TRANSIENT_FLAG;
  } else {
    scm_wrong_type_arg("set-decorate-transient!", 1, val);
  }
  return SCM_BOOL_T;
}

SCM 
set_mwm_decor_hint_x(SCM val, SCM win)
{
  VALIDATEN(win, 2, "set-mwm-decor-hint!");
  if (val == SCM_BOOL_T) {
    SCWMWINDOW(win)->flags |= MWM_DECOR_FLAG;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->flags &= ~MWM_DECOR_FLAG;
  } else {
    scm_wrong_type_arg("set-mwm-decor-hint!", 1, val);
  }
  return SCM_BOOL_T;
}

SCM 
set_mwm_func_hint_x(SCM val, SCM win)
{
  VALIDATEN(win, 2, "set-mwm-func-hint!");
  if (val == SCM_BOOL_T) {
    SCWMWINDOW(win)->flags |= MWM_FUNCTIONS_FLAG;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->flags &= ~MWM_FUNCTIONS_FLAG;
  } else {
    scm_wrong_type_arg("set-mwm-func-hint!", 1, val);
  }
  return SCM_BOOL_T;
}

SCM 
set_PPosition_hint_x(SCM val, SCM win)
{
  VALIDATEN(win, 2, "set-PPosition-hint!");
  if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->flags |= NO_PPOSITION_FLAG;
  } else if (val == SCM_BOOL_T) {
    SCWMWINDOW(win)->flags &= ~NO_PPOSITION_FLAG;
  } else {
    scm_wrong_type_arg("set-PPosition-hint!", 1, val);
  }
  return SCM_BOOL_T;
}

SCM 
set_OL_decor_hint_x(SCM val, SCM win)
{
  VALIDATEN(win, 2, "set-OL-decor-hint!");
  if (val == SCM_BOOL_T) {
    SCWMWINDOW(win)->flags |= OL_DECOR_FLAG;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->flags &= ~OL_DECOR_FLAG;
  } else {
    scm_wrong_type_arg("set-OL-decor-hint!", 1, val);
  }
  return SCM_BOOL_T;
}

SCM 
set_start_on_desk_x(SCM desk, SCM win)
{
  ScwmWindow *tmp_win;

  VALIDATEN(win, 2, "set-start-on-desk!");
  tmp_win = SCWMWINDOW(win);
  if (desk == SCM_BOOL_F) {
    tmp_win->flags &= ~STARTSONDESK_FLAG;
  } else if (gh_number_p(desk)) {
    tmp_win->flags |= STARTSONDESK_FLAG;
    tmp_win->StartDesk = gh_scm2int(desk);
  } else {
    scm_wrong_type_arg("set-start-on-desk!", 1, desk);
  }
  return SCM_BOOL_T;
}

SCM 
set_skip_mapping_x(SCM val, SCM win)
{
  VALIDATEN(win, 2, "set-skip-mapping!");
  if (val == SCM_BOOL_T) {
    SCWMWINDOW(win)->flags |= SHOW_MAPPING;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->flags &= ~SHOW_MAPPING;
  } else {
    scm_wrong_type_arg("set-skip-mapping!", 1, val);
  }
  return SCM_BOOL_T;
}

SCM 
set_lenience_x(SCM val, SCM win)
{
  VALIDATEN(win, 2, "set-lenience!");
  if (val == SCM_BOOL_T) {
    SCWMWINDOW(win)->flags |= LENIENCE_FLAG;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->flags &= ~LENIENCE_FLAG;
  } else {
    scm_wrong_type_arg("set-lenience!", 1, val);
  }
  return SCM_BOOL_T;
}
