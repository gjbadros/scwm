/* $Id$
 * window.c
 *
 */

#define WINDOW_IMPLEMENTATION

/****************************************************************************
 * This module has been significantly modified by Maciej Stachowiak.
 * It may be used under the terms of the fvwm copyright (see COPYING.FVWM).
 * Changes Copyright 1997, Maciej stachowiak
 ****************************************************************************/
#include <stdio.h>
#include <guile/gh.h>
#include <X11/keysym.h>
#include <config.h>
#include "scwm.h"
#include "screen.h"
#include "misc.h"
#include "focus.h"
#include "move.h"
#include "icons.h"
#include "ICCCM.h"
#include "window.h"
#include "color.h"
#include "util.h"
#include "errors.h"
#include "Grab.h"
#include "resize.h"
#include "borders.h"
#include "decor.h"
#include "decorations.h"
#include "colors.h"
#include "colormaps.h"
#include "image.h"
#include "events.h"
#include "module-interface.h"
#include "virtual.h"
#include "font.h"
#include <assert.h>

unsigned long 
FlagsBitsFromSw(ScwmWindow *psw)
{
  unsigned long flags = 0;
  int i = 0;

#define SET_BIT_FOR(x) do { if (psw->x) flags |= (1 << i); i++; } while (0)
  SET_BIT_FOR(fStartIconic);
  SET_BIT_FOR(fOnTop);
  SET_BIT_FOR(fSticky);
  SET_BIT_FOR(fWindowListSkip);
  SET_BIT_FOR(fSuppressIcon);
  SET_BIT_FOR(fNoIconTitle);
  SET_BIT_FOR(fLenience);
  SET_BIT_FOR(fSticky);
  SET_BIT_FOR(fCirculateSkipIcon);
  SET_BIT_FOR(fCirculateSkip);
  SET_BIT_FOR(fClickToFocus);
  SET_BIT_FOR(fSloppyFocus);
  SET_BIT_FOR(fShowOnMap);
  SET_BIT_FOR(fBorder);
  SET_BIT_FOR(fTitle);
  SET_BIT_FOR(fMapped);
  SET_BIT_FOR(fIconified);
  SET_BIT_FOR(fTransient);
  SET_BIT_FOR(fRaised);
  SET_BIT_FOR(fVisible);
  SET_BIT_FOR(fIconOurs);
  SET_BIT_FOR(fPixmapOurs);
  SET_BIT_FOR(fShapedIcon);
  SET_BIT_FOR(fMaximized);
  SET_BIT_FOR(fDoesWmTakeFocus);
  SET_BIT_FOR(fDoesWmDeleteWindow);
  SET_BIT_FOR(fIconMoved);
  SET_BIT_FOR(fIconUnmapped);
  SET_BIT_FOR(fMapPending);
  SET_BIT_FOR(fHintOverride);
  SET_BIT_FOR(fMWMButtons);
  SET_BIT_FOR(fMWMBorders);
#undef SET_BIT_FOR

  assert(i == 32);
  return flags;
}


void
ResetCommonFlags(ScwmWindow *psw)
{
  psw->fStartIconic =
    psw->fOnTop = 
    psw->fSticky = 
    psw->fWindowListSkip =
    psw->fSuppressIcon =
    psw->fNoIconTitle =
    psw->fLenience =
    psw->fStickyIcon =
    psw->fCirculateSkip =
    psw->fCirculateSkipIcon =
    psw->fClickToFocus =
    psw->fSloppyFocus =
    psw->fShowOnMap = False;
}

void
ResetAllFlags(ScwmWindow *psw)
{
  ResetCommonFlags(psw);

  psw->fBorder = 
    psw->fTitle =
    psw->fMapped =
    psw->fIconified =
    psw->fTransient =
    psw->fRaised =
    psw->fVisible =
    psw->fIconOurs =
    psw->fPixmapOurs =
    psw->fShapedIcon =
    psw->fMaximized =
    psw->fDoesWmTakeFocus =
    psw->fDoesWmDeleteWindow =
    psw->fIconMoved =
    psw->fIconUnmapped =
    psw->fMapPending =
    psw->fHintOverride =
    psw->fMWMButtons =
    psw->fMWMBorders =
    psw->fMWMFunctions =
    psw->fMWMDecor =
    psw->fDecorateTransient =
    psw->fWindowShaded =
    psw->fStartsOnDesk =
    psw->fOLDecorHint =
    psw->fNoPPosition =
    psw->fForceIcon = 
    False;
}

void
CopyCommonFlags(ScwmWindow *psw, const ScwmWindow *pswSrc)
{
  psw->fStartIconic = pswSrc->fStartIconic;
  psw->fOnTop = pswSrc->fOnTop;
  psw->fSticky = pswSrc->fSticky;
  psw->fWindowListSkip = pswSrc->fWindowListSkip;
  psw->fSuppressIcon = pswSrc->fSuppressIcon;
  psw->fNoIconTitle = pswSrc->fNoIconTitle;
  psw->fLenience = pswSrc->fLenience;
  psw->fStickyIcon = pswSrc->fStickyIcon;
  psw->fCirculateSkip = pswSrc->fCirculateSkip;
  psw->fCirculateSkipIcon = pswSrc->fCirculateSkipIcon;
  psw->fClickToFocus = pswSrc->fClickToFocus;
  psw->fSloppyFocus = pswSrc->fSloppyFocus;
  psw->fShowOnMap = pswSrc->fShowOnMap;
}

void
CopyAllFlags(ScwmWindow *psw, const ScwmWindow *pswSrc)
{
  CopyCommonFlags(psw,pswSrc);

  psw->fBorder = pswSrc->fBorder;
  psw->fTitle = pswSrc->fTitle;
  psw->fMapped = pswSrc->fMapped;
  psw->fIconified = pswSrc->fIconified;
  psw->fTransient = pswSrc->fTransient;
  psw->fRaised = pswSrc->fRaised;
  psw->fVisible = pswSrc->fVisible;
  psw->fIconOurs = pswSrc->fIconOurs;
  psw->fPixmapOurs = pswSrc->fPixmapOurs;
  psw->fShapedIcon = pswSrc->fShapedIcon;
  psw->fMaximized = pswSrc->fMaximized;
  psw->fDoesWmTakeFocus = pswSrc->fDoesWmTakeFocus;
  psw->fDoesWmDeleteWindow = pswSrc->fDoesWmDeleteWindow;
  psw->fIconMoved = pswSrc->fIconMoved;
  psw->fIconUnmapped = pswSrc->fIconUnmapped;
  psw->fMapPending = pswSrc->fMapPending;
  psw->fHintOverride = pswSrc->fHintOverride;
  psw->fMWMButtons = pswSrc->fMWMButtons;
  psw->fMWMBorders = pswSrc->fMWMBorders;
  psw->fMWMFunctions = pswSrc->fMWMFunctions;
  psw->fMWMDecor = pswSrc->fMWMDecor;
  psw->fDecorateTransient = pswSrc->fDecorateTransient;
  psw->fWindowShaded = pswSrc->fWindowShaded;
  psw->fStartsOnDesk = pswSrc->fStartsOnDesk;
  psw->fOLDecorHint = pswSrc->fOLDecorHint;
  psw->fNoPPosition = pswSrc->fNoPPosition;
  psw->fForceIcon = pswSrc->fForceIcon;
}

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
    
  /* FIXGJB: revisit this */
  if (VALIDWINP(obj) && SCWMWINDOW(obj)->fl != NULL) {
    ScwmWindow *psw = SCWMWINDOW(obj);
    scm_gc_mark(psw->fl->scmdecor);
    scm_gc_mark(psw->mini_icon_image);
    scm_gc_mark(psw->icon_req_image);
    scm_gc_mark(psw->icon_image);
    scm_gc_mark(psw->ReliefColor);
    scm_gc_mark(psw->ShadowColor);
    scm_gc_mark(psw->TextColor);
    scm_gc_mark(psw->BackColor);
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
ensure_valid(SCM win, int n, char *subr, SCM kill_p, SCM release_p)
{
  if (win == SCM_UNDEFINED) {
    win = get_window(kill_p, SCM_BOOL_T, release_p);
    if (win == SCM_BOOL_F || win == SCM_UNDEFINED) {
      return SCM_BOOL_F;
    }
  }
  if (!WINDOWP(win)) {
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
  return SCM_BOOL_FromBool(WINDOWP(obj));
}

SCM 
get_window(SCM kill_p, SCM select_p, SCM release_p)
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
  if (release_p == SCM_UNDEFINED) {
    release_p = SCM_BOOL_T;
  } else if (!gh_boolean_p(release_p)) {
    scm_wrong_type_arg("get-window", 3, release_p);
  }
  if (window_context == SCM_UNDEFINED) {
    if (select_p == SCM_BOOL_T) {
      return select_window(kill_p,release_p);
    } else {
      return SCM_BOOL_F;
    }
  }
  return window_context;
}

SCM
current_window_with_focus()
{
  return Scr.Hilite? Scr.Hilite->schwin : SCM_BOOL_F;
}


SCM
current_window_with_pointer()
{
  ScwmWindow *sw = SwFromPointerLocation(dpy);
  return sw? sw->schwin: SCM_BOOL_F;
}

SCM
select_window_interactively()
{
  ScwmWindow *sw = SwSelectInteractively(dpy);
  return sw? sw->schwin: SCM_BOOL_F;
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

  if (!t)
    return;

  if (t->Desk != Scr.CurrentDesk) {
    changeDesks(0, t->Desk);
  }
#ifndef NON_VIRTUAL
  if (t->fIconified) {
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

  if (t->fIconified) {
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
    if (!t->fClickToFocus)
      XWarpPointer(dpy, None, Scr.Root, 0, 0, 0, 0, 2, 2);
  }
  UngrabEm();
  SetFocus(t->w, t, False);
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

  if (!t || (t->fIconified && t->icon_w == None))
    return;

  if (t->Desk != Scr.CurrentDesk) {
    changeDesks(0, t->Desk);
  }
#ifndef NON_VIRTUAL
  if (t->fIconified) {
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

  if (t->fIconified) {
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

ScwmWindow *
SwFromWindow(Display *dpy, Window w)
{
  ScwmWindow *sw;
  if (XFindContext(dpy, w, ScwmContext, (caddr_t *) &sw) == XCNOENT) {
    return NULL;
  }
  return sw;
}

ScwmWindow *
SwFromPointerLocation(Display *dpy)
{
  Window wChild;
  XQueryPointer(dpy, Scr.Root, &JunkRoot, &wChild,
		&JunkX, &JunkY, &JunkX, &JunkY, &JunkMask);
  if (wChild == None) {
    return NULL;
  }
  return SwFromWindow(dpy,wChild);
}

ScwmWindow *
SwSelectInteractively(Display *dpy)
{
  /* FIXGJB: this needs to be written, but could
     benefit from better, more general, event handling */
  return NULL;
}
  


extern int orig_x, orig_y, have_orig_position;

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
static
int 
DeferExecution(XEvent * eventp, Window * w, ScwmWindow ** tmp_win,
	       unsigned long *context, enum cursor cursor, int FinishEvent)
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
    /* FIXGJB: this code causes a bug:  when a "(get-window)" is sent by 
       scwmsend, the ButtonMotionMask below causes scwm to re-eval the
       string received via the PropertyNotify event for each mouse movement
       observed;  this results in a helluva lot of get-window calls being
       made, when only one was desired;  The best way to deal with this
       is get rid of the DeferExecution garbade, but that has to come later
    */
    XMaskEvent(dpy, ButtonPressMask | ButtonReleaseMask |
	       ExposureMask | KeyPressMask | VisibilityChangeMask |
	       ButtonMotionMask | PointerMotionMask, eventp);
    StashEventTime(eventp);

    if (eventp->type == KeyPress) {
      Keyboard_shortcuts(eventp, FinishEvent);
    }
    if (eventp->type == FinishEvent) {
      fFinished = True;
    }
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
      /* copy the event to the global current event structure so the
	 right event is dispatched on. */
      Event = *eventp;
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
    return True;
  }
  *tmp_win = SwFromWindow(dpy,*w);
  if (*tmp_win == NULL) {
    XBell(dpy, Scr.screen);
    UngrabEm();
    return (True);
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
      return True;
    }
  *context = GetContext(*tmp_win, eventp, &dummy);

  UngrabEm();
  /* interactive operations should not use the stashed mouse position
     if we just selected the window. */
  have_orig_position = 0; 
  return False; 
}

SCM 
select_window(SCM kill_p, SCM release_p)
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

  if (release_p == SCM_UNDEFINED) {
    release_p = SCM_BOOL_T;
  } else if (!gh_boolean_p(release_p)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("select-window", 2, release_p);
  }


  if (DeferExecution(&ev, &w, &tmp_win, &context,
		     (kill_p != SCM_BOOL_F ? CURSOR_DESTROY : CURSOR_SELECT),
		     (release_p != SCM_BOOL_F ? ButtonRelease :
		      ButtonPress))) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
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






/*
 * Unmaps a window on transition to a new desktop
 */
void 
UnmapScwmWindow(ScwmWindow * t)
{
  XWindowAttributes winattrs;
  unsigned long eventMask;

  /*
   * Prevent the receipt of an UnmapNotify, since that would
   * cause a transition to the Withdrawn state.
   */
  XGetWindowAttributes(dpy, t->w, &winattrs);
  eventMask = winattrs.your_event_mask;
  XSelectInput(dpy, t->w, eventMask & ~StructureNotifyMask);
  if (t->fIconified) {
    if (t->icon_pixmap_w != None)
      XUnmapWindow(dpy, t->icon_pixmap_w);
    if (t->icon_w != None)
      XUnmapWindow(dpy, t->icon_w);
  } else if (t->fMapped || t->fMapPending) {
    XUnmapWindow(dpy, t->frame);
  }
  XSelectInput(dpy, t->w, eventMask);
}

/*
 * Maps a window on transition to a new desktop
 */
void 
MapIt(ScwmWindow * t)
{
  if (t->fIconified) {
    if (t->icon_pixmap_w != None)
      XMapWindow(dpy, t->icon_pixmap_w);
    if (t->icon_w != None)
      XMapWindow(dpy, t->icon_w);
  } else if (t->fMapped) {
    XMapWindow(dpy, t->frame);
    t->fMapPending = True;
    XMapWindow(dpy, t->Parent);
  }
}

/*
 * Raise a window in the stacking order
 */ 
void 
RaiseWindow(ScwmWindow * t)
{
  ScwmWindow *t2;
  int count, i;
  Window *wins;

  /* raise the target, at least */
  count = 1;
  Broadcast(M_RAISE_WINDOW, 3, t->w, t->frame, (unsigned long) t, 0, 0, 0, 0);

  for (t2 = Scr.ScwmRoot.next; t2 != NULL; t2 = t2->next) {
    if (t2->fOnTop)
      count++;
    if (t2->fTransient && (t2->transientfor == t->w) &&
	(t2 != t)) {
      count++;
      Broadcast(M_RAISE_WINDOW, 3, t2->w, t2->frame, (unsigned long) t2,
		0, 0, 0, 0);
      if (t2->fIconified && !t2->fSuppressIcon) {
	count += 2;
      }
    }
  }
  if (t->fIconified && !t->fSuppressIcon) {
    count += 2;
  }
  wins = (Window *) safemalloc(count * sizeof(Window));

  i = 0;

  /* fOnTop windows on top */
  for (t2 = Scr.ScwmRoot.next; t2 != NULL; t2 = t2->next) {
    if (t2->fOnTop) {
      Broadcast(M_RAISE_WINDOW, 3, t2->w, t2->frame, (unsigned long) t2,
		0, 0, 0, 0);
      wins[i++] = t2->frame;
    }
  }

  /* now raise transients */
#ifndef DONT_RAISE_TRANSIENTS
  for (t2 = Scr.ScwmRoot.next; t2 != NULL; t2 = t2->next) {
    if (t2->fTransient &&
	(t2->transientfor == t->w) &&
	(t2 != t) &&
	(!t2->fOnTop)) {
      wins[i++] = t2->frame;
      if (t2->fIconified && !t2->fSuppressIcon) {
	if (!t2->fNoIconTitle)
	  wins[i++] = t2->icon_w;
	if (!(t2->icon_pixmap_w))
	  wins[i++] = t2->icon_pixmap_w;
      }
    }
  }
#endif
  if (t->fIconified && !t->fSuppressIcon) {
    if (!t->fNoIconTitle)
      wins[i++] = t->icon_w;
    if (t->icon_pixmap_w)
      wins[i++] = t->icon_pixmap_w;
  }
  if (!t->fOnTop) {
    wins[i++] = t->frame;
    Scr.LastWindowRaised = t;
  }

  if (i > 0)
    XRaiseWindow(dpy, wins[0]);

  XRestackWindows(dpy, wins, i);
  free(wins);
  raisePanFrames();
}


void 
LowerWindow(ScwmWindow * t)
{
  XLowerWindow(dpy, t->frame);

  Broadcast(M_LOWER_WINDOW, 3, t->w, t->frame, (unsigned long) t, 0, 0, 0, 0);

  if (t->fIconified && !t->fSuppressIcon) {
    XLowerWindow(dpy, t->icon_w);
    XLowerWindow(dpy, t->icon_pixmap_w);
  }
  Scr.LastWindowRaised = NULL;
}

/*
 * Handles destruction of a window 
 */
void 
DestroyScwmWindow(ScwmWindow * sw)
{
  int i;
  extern ScwmWindow *ButtonWindow;
  extern ScwmWindow *colormap_win;
  extern Bool PPosOverride;

  /*
   * Warning, this is also called by HandleUnmapNotify; if it ever needs to
   * look at the event, HandleUnmapNotify will have to mash the UnmapNotify
   * into a DestroyNotify.
   */
  if (!sw)
    return;

  XUnmapWindow(dpy, sw->frame);

  if (!PPosOverride)
    XSync(dpy, 0);

  if (sw == Scr.Hilite)
    Scr.Hilite = NULL;

  Broadcast(M_DESTROY_WINDOW, 3, sw->w, sw->frame,
	    (unsigned long) sw, 0, 0, 0, 0);

  if (Scr.PreviousFocus == sw)
    Scr.PreviousFocus = NULL;

  if (ButtonWindow == sw)
    ButtonWindow = NULL;

  if ((sw == Scr.Focus) && sw->fClickToFocus) {
    if (sw->next) {
      HandleHardFocus(sw->next);
    } else {
      SetFocus(Scr.NoFocusWin, NULL, 1);
    }
  } else if (Scr.Focus == sw) {
    SetFocus(Scr.NoFocusWin, NULL, 1);
  }

  if (sw == FocusOnNextTimeStamp)
    FocusOnNextTimeStamp = NULL;

  if (sw == Scr.Ungrabbed)
    Scr.Ungrabbed = NULL;

  if (sw == Scr.pushed_window)
    Scr.pushed_window = NULL;

  if (sw == colormap_win)
    colormap_win = NULL;

  XDestroyWindow(dpy, sw->frame);
  XDeleteContext(dpy, sw->frame, ScwmContext);

  XDestroyWindow(dpy, sw->Parent);

  XDeleteContext(dpy, sw->Parent, ScwmContext);

  XDeleteContext(dpy, sw->w, ScwmContext);

  if (sw->icon_w && sw->fPixmapOurs &&
      sw->icon_image != SCM_BOOL_F) {
    XFreePixmap(dpy, IMAGE(sw->icon_image)->image);
  }

  if (sw->icon_w) {
    XDestroyWindow(dpy, sw->icon_w);
    XDeleteContext(dpy, sw->icon_w, ScwmContext);
  }
  if (sw->fIconOurs && (sw->icon_pixmap_w != None))
    XDestroyWindow(dpy, sw->icon_pixmap_w);
  if (sw->icon_pixmap_w != None)
    XDeleteContext(dpy, sw->icon_pixmap_w, ScwmContext);

  if (sw->fTitle) {
    XDeleteContext(dpy, sw->title_w, ScwmContext);
    for (i = 0; i < Scr.nr_left_buttons; i++)
      XDeleteContext(dpy, sw->left_w[i], ScwmContext);
    for (i = 0; i < Scr.nr_right_buttons; i++)
      if (sw->right_w[i] != None)
	XDeleteContext(dpy, sw->right_w[i], ScwmContext);
  }
  if (sw->fBorder) {
    for (i = 0; i < 4; i++)
      XDeleteContext(dpy, sw->sides[i], ScwmContext);
    for (i = 0; i < 4; i++)
      XDeleteContext(dpy, sw->corners[i], ScwmContext);
  }
  sw->prev->next = sw->next;
  if (sw->next != NULL)
    sw->next->prev = sw->prev;
  free_window_names(sw, True, True);
  if (sw->wmhints)
    XFree((char *) sw->wmhints);
  /* removing NoClass change for now... */
  if (sw->classhint.res_name && sw->classhint.res_name != NoResource)
    XFree((char *) sw->classhint.res_name);
  if (sw->classhint.res_class && sw->classhint.res_class != NoClass)
    XFree((char *) sw->classhint.res_class);
  if (sw->mwm_hints)
    XFree((char *) sw->mwm_hints);

  if (sw->cmap_windows != (Window *) NULL)
    XFree((void *) sw->cmap_windows);

  /* XSCM */
  invalidate_window(sw->schwin);

  free((char *) sw);

  if (!PPosOverride)
    XSync(dpy, 0);
  return;
}


SCM 
delete_window(SCM win)
{
  ScwmWindow *tmp_win;

  SCM_REDEFER_INTS;

  VALIDATEKILL(win, "delete-window");

  tmp_win = SCWMWINDOW(win);
  if (check_allowed_function(F_DELETE, tmp_win) == 0) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  if (tmp_win->fDoesWmDeleteWindow) {
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
  ScwmWindow *sw;

  SCM_REDEFER_INTS;
  VALIDATEKILL(win, "destroy-window");
  sw = SCWMWINDOW(win);
  if (check_allowed_function(F_DESTROY, sw) == 0) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  if (XGetGeometry(dpy, sw->w, &JunkRoot, &JunkX, &JunkY,
		   &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth) == 0) {
    DestroyScwmWindow(sw);
  } else {
    XKillClient(dpy, sw->w);
  }
  XSync(dpy, 0);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}



SCM 
window_deletable_p(SCM win)
{
  VALIDATEKILL(win, "window-deletable?");
  return SCM_BOOL_FromBool(SCWMWINDOW(win)->fDoesWmDeleteWindow);
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

SCM
unfocus()
{
  SCM_REDEFER_INTS;
  Unfocus();
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
  return SCM_BOOL_FromBool(tmp_win == Scr.LastWindowRaised ||
			   tmp_win->fVisible);
}



SCM 
iconify(SCM win)
{
  ScwmWindow *tmp_win;

  SCM_REDEFER_INTS;
  VALIDATE(win, "iconify");
  tmp_win = SCWMWINDOW(win);

  if (check_allowed_function(F_ICONIFY, tmp_win) == 0) {

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
  return SCM_BOOL_FromBool(SCWMWINDOW(win)->fIconified);
}


SCM 
stick(SCM win)
{
  ScwmWindow *tmp_win;

  SCM_REDEFER_INTS;
  VALIDATE(win, "stick");
  tmp_win = SCWMWINDOW(win);
  tmp_win->fSticky = True;
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
  tmp_win->fSticky = False;
  BroadcastConfig(M_CONFIGURE_WINDOW, tmp_win);
  SetTitleBar(tmp_win, (Scr.Hilite == tmp_win), True);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
sticky_p(SCM win)
{
  VALIDATE(win, "sticky?");
  return SCM_BOOL_FromBool(SCWMWINDOW(win)->fSticky);
}




/***********************************************************************
 *
 *  WindowShade -- shades or unshades a window (veliaa@rpi.edu)
 ***********************************************************************/

/* Modified for scwm by mstachow@mit.edu, 
   animation added by gjb@cs.washington.edu */

SCM 
window_shade(SCM win, SCM animated_p)
{
  ScwmWindow *sw;
  Bool fAnimated = False;

  SCM_REDEFER_INTS;
  VALIDATE(win, "window-shade");
  sw = SCWMWINDOW(win);

  if (!sw->fTitle || sw->fMaximized) {
    return SCM_BOOL_F;
  }
#ifdef GJB_BE_ANAL_ABOUT_BOOLS
  /* FIXGJB: I took this code out so I can say:
     (window-shade (get-window) 'animated)
     Is there a better way? */
  if (!gh_boolean_p(animated_p)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("window-shade", 2, animated_p);
  }
#endif
  if (animated_p == SCM_UNDEFINED) {
    /* FIXGJB: make an option for allowing the default to be animated */
    animated_p = SCM_BOOL_F;
  }
  fAnimated = gh_scm2bool(animated_p);

  SET_SHADED(sw);
  
  if (fAnimated) {
    AnimatedShadeWindow(sw,True /* roll up */, -1, NULL);
    /* discard resize events */
    while (XCheckMaskEvent(dpy,  ResizeRedirectMask, &Event))
      { }
    /* We discard events so we don't propagate a resize
       event that will call setupframe again */
    /* Note sometimes the event we're trying to discard won't be
       generated in time for the above to discard it, so I had to hack
       the HandleConfigureNotify() routine to avoid resizing the
       frame; I left the XSync in for performance, since there's no
       reason to propagate that event if we can avoid it; perhaps
       substructure redirection is a solution here, but I don't know
       much about it --11/11/97 gjb */
  }
  SetupFrame(sw, sw->frame_x, sw->frame_y, sw->frame_width,
	     sw->title_height + sw->boundary_width, False);
  if (fAnimated) {
    /* need to reset the client window offset so that if
       if it's un-window-shaded w/o animation, things are ok */
    XMoveWindow(dpy,sw->w,0,0);
  }

  CoerceEnterNotifyOnCurrentWindow();
  Broadcast(M_WINDOWSHADE, 1, sw->w, 0, 0, 0, 0, 0, 0);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
un_window_shade(SCM win, SCM animated_p)
{
  ScwmWindow *sw;
  Bool fAnimated = False;

  SCM_REDEFER_INTS;
  VALIDATE(win, "un-window-shade");
  sw = SCWMWINDOW(win);

#ifdef GJB_BE_ANAL_ABOUT_BOOLS
  /* FIXGJB: I took this code out so I can say:
     (window-shade (get-window) 'animated)
     Is there a better way? */
  if (!gh_boolean_p(animated_p)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("un-window-shade", 2, animated_p);
  }
#endif
  if (animated_p == SCM_UNDEFINED) {
    /* FIXGJB: make an option for allowing the default to be animated */
    animated_p = SCM_BOOL_F;
  }
  fAnimated = gh_scm2bool(animated_p);

  SET_UNSHADED(sw);
  if (fAnimated) {
    AnimatedShadeWindow(sw,False /* !roll up */, -1, NULL);
  }
  SetupFrame(sw, sw->frame_x, sw->frame_y, 
	     sw->orig_wd, sw->orig_ht, True);
  Broadcast(M_DEWINDOWSHADE, 1, sw->w, 0, 0, 0, 0, 0, 0);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
window_shaded_p(SCM win)
{
  VALIDATE(win, "window-shaded?");
  return SCM_BOOL_FromBool(SHADED_P(SCWMWINDOW(win)));
}


void 
move_finalize(Window w, ScwmWindow * sw, int x, int y)
{
  if (w == sw->frame) {
    SetupFrame(sw, x, y,
	       sw->frame_width, sw->frame_height, False);
  } else {			/* icon window */
    sw->fIconMoved = True;
    sw->icon_x_loc = x;
    sw->icon_xl_loc = y - (sw->icon_w_width - sw->icon_p_width) / 2;
    sw->icon_y_loc = y;
    Broadcast(M_ICON_LOCATION, 7, sw->w, sw->frame,
	      (unsigned long) sw,
	      sw->icon_x_loc, sw->icon_y_loc,
	      sw->icon_w_width, sw->icon_w_height
	      + sw->icon_p_height);
    XMoveWindow(dpy, sw->icon_w,
		sw->icon_xl_loc, y + sw->icon_p_height);
    if (sw->icon_pixmap_w != None) {
      XMapWindow(dpy, sw->icon_w);
      XMoveWindow(dpy, sw->icon_pixmap_w, sw->icon_x_loc, y);
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
  if (tmp_win->fIconified) {
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


SCM 
interactive_move(SCM win)
{
  ScwmWindow *tmp_win;
  Window w;
  XEvent event;
  int x, y;

  SCM_REDEFER_INTS;
  VALIDATE_PRESS_ONLY(win, "interactive-move");
  tmp_win = SCWMWINDOW(win);
  w = tmp_win->frame;
  if (tmp_win->fIconified) {
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

  if (check_allowed_function(F_RESIZE, tmp_win) == 0
      || SHADED_P(tmp_win)) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  tmp_win->fMaximized = False;

  /* can't resize icons */
  if (tmp_win->fIconified) {
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
	     tmp_win->frame_y, width, height, False);

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
  Bool finished = False, done = False, abort = False;
  int x, y, delta_x, delta_y;
  Window ResizeWindow;
  Bool flags;

  VALIDATE_PRESS_ONLY(win, "interactive-resize");
  tmp_win = SCWMWINDOW(win);


  if (check_allowed_function(F_RESIZE, tmp_win) == 0
      || SHADED_P(tmp_win)) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  tmp_win->fMaximized = False;

  if (tmp_win->fIconified) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  ResizeWindow = tmp_win->frame;


  InstallRootColormap();
  if (!GrabEm(CURSOR_MOVE)) {
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
    done = False;
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
	abort = True;
	finished = True;
      }
      done = True;
      break;

    case ButtonRelease:
      finished = True;
      done = True;
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
      done = True;
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
	       dragy - tmp_win->bw, dragWidth, dragHeight, False);
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

  refresh_common(tmp_win->fIconified ?
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
  if (!(t->fIconified && t->fStickyIcon) &&
      !t->fSticky && !t->fIconUnmapped) {
    if (t->Desk == Scr.CurrentDesk) {
      t->Desk = val1;
      if (val1 != Scr.CurrentDesk) {
	UnmapScwmWindow(t);
      }
    } else if (val1 == Scr.CurrentDesk) {
      t->Desk = val1;
      /* If its an icon, auto-place it */
      if (t->fIconified)
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
window_frame_id(SCM win)
{
  VALIDATE(win, "window-frame-id");
  return SCM_MAKINUM(SCWMWINDOW(win)->frame);
}

SCM
window_from_window_id(SCM window_id)
{
  Window w;
  ScwmWindow *sw = NULL;
  if (!gh_number_p(window_id)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(__FUNCTION__, 1, window_id);
  }
  w = gh_scm2int(window_id);
  sw = SwFromWindow(dpy, (Window) w);
  return (sw?sw->schwin : SCM_BOOL_F);
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
  return gh_str02scm(SCWMWINDOW(win)->classhint.res_class);
}

SCM 
window_resource(SCM win)
{
  VALIDATE(win, "window-resource");
  return gh_str02scm(SCWMWINDOW(win)->classhint.res_name);
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
  tmp_win->fOnTop = True;
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
  tmp_win->fOnTop = False;
  /* is this needed? */
  BroadcastConfig(M_CONFIGURE_WINDOW, tmp_win);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
kept_on_top_p(SCM win)
{
  VALIDATE(win, "kept-on-top?");
  return SCM_BOOL_FromBool(SCWMWINDOW(win)->fOnTop);
}


/* maybe all of this can be replaced with set-title-height 
   (a per-window version) ? */

SCM 
show_titlebar(SCM win)
{
  ScwmWindow *tmp_win;
  ScwmDecor *fl;

  SCM_REDEFER_INTS;

  VALIDATE(win, "show-titlebar");
  tmp_win = SCWMWINDOW(win);
  fl = tmp_win->fl ? tmp_win->fl : &Scr.DefaultDecor;


  if (!tmp_win->fTitle) {
    tmp_win->fTitle = True;
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

  VALIDATE(win, "hide-titlebar");
  tmp_win = SCWMWINDOW(win);
  fl = tmp_win->fl ? tmp_win->fl : &Scr.DefaultDecor;

  if (tmp_win->fTitle) {
    tmp_win->fTitle = False;
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
  return SCM_BOOL_FromBool(SCWMWINDOW(win)->fTitle);
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
  tmp_win->fBorder = True;
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
  int i;

  SCM_REDEFER_INTS;

  VALIDATE(win, "plain-border");
  tmp_win = SCWMWINDOW(win);
  tmp_win->fBorder = False;
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
  return SCM_BOOL_FromBool(SCWMWINDOW(win)->fBorder);
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
  tmp_win->fStickyIcon = True;
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
  tmp_win->fStickyIcon = False;
  BroadcastConfig(M_CONFIGURE_WINDOW, tmp_win);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}

SCM 
icon_sticky_p(SCM win)
{
  VALIDATE(win, "icon-sticky?");
  return SCM_BOOL_FromBool(SCWMWINDOW(win)->fStickyIcon);
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


SCM sym_mouse, sym_sloppy, sym_none;
extern SCM sym_click;

void 
init_window()
{
  sym_mouse = gh_symbol2scm("mouse");
  scm_protect_object(sym_mouse);
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
    tmp_win->fClickToFocus = False;
    tmp_win->fSloppyFocus = False;
  } else if (gh_eq_p(sym, sym_click)) {
    tmp_win->fClickToFocus = True;
    tmp_win->fSloppyFocus = False;
  } else if (gh_eq_p(sym, sym_sloppy)) {
    tmp_win->fClickToFocus = False;
    tmp_win->fSloppyFocus = True;
  } else if (gh_eq_p(sym, sym_none)) {
    tmp_win->fClickToFocus = True;
    tmp_win->fSloppyFocus = True;
  } else {
    scwm_error("set-window-focus!", 13);
  }
  return sym;
}


SCM
set_window_foreground_x(SCM fg, SCM win)
{
  ScwmWindow *tmp_win;

  VALIDATE_COLOR(fg, "set-window-foreground!", 1);

  VALIDATEN(win, 2, "set-window-foreground!");
  tmp_win = SCWMWINDOW(win);

  tmp_win->TextColor = fg;
  SetBorderX(tmp_win, (Scr.Hilite == tmp_win), True, True, None, True);

  return SCM_BOOL_T;  
}

SCM
set_window_background_x(SCM bg, SCM win)
{
  ScwmDecor * fl;
  ScwmWindow *tmp_win;

  VALIDATE_COLOR(bg, "set-window-background!", 1);

  VALIDATEN(win, 2, "set-window-background!");
  tmp_win = SCWMWINDOW(win);
  fl = tmp_win->fl ? tmp_win->fl : &Scr.DefaultDecor;


  tmp_win->BackColor = bg;
  tmp_win->ShadowColor = adjust_brightness(tmp_win->BackColor, fl->shadow_factor);
  tmp_win->ReliefColor = adjust_brightness(tmp_win->BackColor, fl->hilight_factor);

  SetBorderX(tmp_win, (Scr.Hilite == tmp_win), True, True, None, True);

  return SCM_BOOL_T;
}


SCM 
set_random_placement_x(SCM val, SCM win)
{
  VALIDATEN(win, 2, "set-random-placement!");
  if (val == SCM_BOOL_T) {
    SCWMWINDOW(win)->fRandomPlace = True;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->fRandomPlace = False;
  } else {
    scm_wrong_type_arg("set-random-placement!", 1, val);
  }
  return SCM_BOOL_T;
}

SCM 
set_smart_placement_x(SCM val, SCM win)
{
  VALIDATEN(win, 2, "set-smart-placement!");
  if (val == SCM_BOOL_T) {
    SCWMWINDOW(win)->fSmartPlace = True;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->fSmartPlace = False;
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
    SCWMWINDOW(win)->fMWMButtons = True;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->fMWMButtons = False;
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
    SCWMWINDOW(win)->fMWMBorders = True;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->fMWMBorders = False;
  } else {
    scm_wrong_type_arg("set-mwm-border!", 1, val);
  }

  SetBorderX(t, (Scr.Hilite == t), True, True, None, True);

  return SCM_BOOL_T;
}


void
force_icon_redraw (ScwmWindow *tmp_win)
{
  XDestroyWindow(dpy, tmp_win->icon_w);
  tmp_win->icon_w = None;

  if (tmp_win->fIconified) {
    Iconify(tmp_win, 0, 0);
  }  
}

SCM 
set_icon_title_x(SCM title, SCM win)
{
  ScwmWindow *tmp_win;

  /* Should changing the icon title string be allowed? */

  VALIDATEN(win, 2, "set-icon-title!");
  tmp_win = SCWMWINDOW(win);

  if (title == SCM_BOOL_F) {
    tmp_win->fNoIconTitle = True;
  } else if (title == SCM_BOOL_T) {
    tmp_win->fNoIconTitle = False;
  } else {
    scm_wrong_type_arg("set-icon-title!", 1, title);
  }

  force_icon_redraw (tmp_win);

  return SCM_BOOL_T;
}


SCM
set_force_icon_x (SCM flag, SCM win)
{
  ScwmWindow *tmp_win;

  VALIDATEN(win, 2, "set-force-icon!");
  tmp_win = SCWMWINDOW(win);

  if (flag== SCM_BOOL_F) {
    tmp_win->fForceIcon=False; 
  } else if (flag== SCM_BOOL_T) {
    tmp_win->fForceIcon=True; 
  } else {
    scm_wrong_type_arg("set-force-icon!", 1, flag);
  }

  force_icon_redraw (tmp_win);
  return SCM_BOOL_T;
}

SCM 
set_show_icon_x (SCM flag, SCM win)
{
  ScwmWindow *tmp_win;

  VALIDATEN(win, 2, "set-show-icon!");
  tmp_win = SCWMWINDOW(win);

  if (flag== SCM_BOOL_F) {
    tmp_win->fSuppressIcon = True;
  } else if (flag == SCM_BOOL_T) {
    tmp_win->fSuppressIcon = False;
  } else {
    scm_wrong_type_arg("set-show-icon!", 1, flag);
  }

  force_icon_redraw (tmp_win);

  return SCM_BOOL_T;
}

SCM 
set_icon_x(SCM picture, SCM win)
{
  ScwmWindow *tmp_win;

  VALIDATEN(win, 2, "set-icon!");
  tmp_win = SCWMWINDOW(win);
  if (gh_string_p(picture)) {
    tmp_win->icon_req_image = make_image(picture);
  } else if (IMAGE_P(picture) || picture == SCM_BOOL_F) {
    tmp_win->icon_req_image = picture;
  } else {
    scm_wrong_type_arg("set-icon!", 1, picture);
  }

  force_icon_redraw (tmp_win);
  return SCM_BOOL_T;
}

SCM 
set_mini_icon_x(SCM image, SCM win)
{
  ScwmWindow *sw;

  VALIDATEN(win, 2, "set-mini-icon!");
  sw = SCWMWINDOW(win);
  if (image == SCM_BOOL_F) {
    sw->mini_icon_image = SCM_BOOL_F;
  } else if (gh_string_p(image)) {
    sw->mini_icon_image = make_image(image);
  } else if (IMAGE_P(image)) {
    sw->mini_icon_image = image;
  } else {
    scm_wrong_type_arg("set-mini-icon!", 1, image);
  }

  /* Broadcast the new mini-icon or something? */
  if (sw->mini_icon_image != SCM_BOOL_F) {
    Broadcast(M_MINI_ICON, 6,
	      sw->w,	/* Watch Out ! : I reduced the set of infos... */
	      IMAGE(sw->mini_icon_image)->image,
	      IMAGE(sw->mini_icon_image)->mask,
	      IMAGE(sw->mini_icon_image)->width,
	      IMAGE(sw->mini_icon_image)->height,
	      IMAGE(sw->mini_icon_image)->depth, 0);
  }

  SetBorderX(sw, Scr.Hilite == sw, True, sw->fMapped, None, True);

  return SCM_BOOL_T;

}

SCM 
set_hint_override_x(SCM val, SCM win)
{
  VALIDATEN(win, 2, "set-hint-override!");
  if (val == SCM_BOOL_T) {
    SCWMWINDOW(win)->fHintOverride = True;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->fHintOverride = False;
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
    SCWMWINDOW(win)->fDecorateTransient = True;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->fDecorateTransient = False;
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
    SCWMWINDOW(win)->fMWMDecor = True;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->fMWMDecor = False;
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
    SCWMWINDOW(win)->fMWMFunctions = True;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->fMWMFunctions = False;
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
    SCWMWINDOW(win)->fNoPPosition = True;
  } else if (val == SCM_BOOL_T) {
    SCWMWINDOW(win)->fNoPPosition = False;
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
    SCWMWINDOW(win)->fOLDecorHint = True;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->fOLDecorHint = False;
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
    tmp_win->fStartsOnDesk = False;
  } else if (gh_number_p(desk)) {
    tmp_win->fStartsOnDesk = True;
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
    SCWMWINDOW(win)->fShowOnMap = True;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->fShowOnMap = False;
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
    SCWMWINDOW(win)->fLenience = True;
  } else if (val == SCM_BOOL_F) {
    SCWMWINDOW(win)->fLenience = False;
  } else {
    scm_wrong_type_arg("set-lenience!", 1, val);
  }
  return SCM_BOOL_T;
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
