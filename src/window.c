/* $Id$
 * window.c
 *
 */

/* #define SCWM_DEBUG_MSGS */

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
#include <assert.h>
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
#include "xmisc.h"
#include "guile-compat.h"


SCM sym_mouse, sym_sloppy, sym_none;
extern SCM sym_click;


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


/* Copy only the set common flags from pswSrc into psw
   i.e., this will not reset any of psw's flags */
void
CopySetCommonFlags(ScwmWindow *psw, const ScwmWindow *pswSrc)
{
  if (pswSrc->fStartIconic)
    psw->fStartIconic = True;
  if (pswSrc->fOnTop)
    psw->fOnTop = True;
  if (pswSrc->fSticky)
    psw->fSticky = True;
  if (pswSrc->fWindowListSkip)
    psw->fWindowListSkip = True;
  if (pswSrc->fSuppressIcon)
    psw->fSuppressIcon = True;
  if (pswSrc->fNoIconTitle)
    psw->fNoIconTitle = True;
  if (pswSrc->fLenience)
    psw->fLenience = True;
  if (pswSrc->fStickyIcon)
    psw->fStickyIcon = True;
  if (pswSrc->fCirculateSkip)
    psw->fCirculateSkip = True;
  if (pswSrc->fCirculateSkipIcon)
    psw->fCirculateSkipIcon = True;
  if (pswSrc->fClickToFocus)
    psw->fClickToFocus = True;
  if (pswSrc->fSloppyFocus)
    psw->fSloppyFocus = True;
  if (pswSrc->fShowOnMap)
    psw->fShowOnMap = True;
}

void
CopySetAllFlags(ScwmWindow *psw, const ScwmWindow *pswSrc)
{
  CopySetCommonFlags(psw,pswSrc);

  if ( pswSrc->fBorder )
    psw->fBorder = True;
  if ( pswSrc->fTitle )
    psw->fTitle = True;
  if ( pswSrc->fMapped )
    psw->fMapped = True;
  if ( pswSrc->fIconified )
    psw->fIconified = True;
  if ( pswSrc->fTransient )
    psw->fTransient = True;
  if ( pswSrc->fRaised )
    psw->fRaised = True;
  if ( pswSrc->fVisible )
    psw->fVisible = True;
  if ( pswSrc->fIconOurs )
    psw->fIconOurs = True;
  if ( pswSrc->fPixmapOurs )
    psw->fPixmapOurs = True;
  if ( pswSrc->fShapedIcon )
    psw->fShapedIcon = True;
  if ( pswSrc->fMaximized )
    psw->fMaximized = True;
  if ( pswSrc->fDoesWmTakeFocus )
    psw->fDoesWmTakeFocus = True;
  if ( pswSrc->fDoesWmDeleteWindow )
    psw->fDoesWmDeleteWindow = True;
  if ( pswSrc->fIconMoved )
    psw->fIconMoved = True;
  if ( pswSrc->fIconUnmapped )
    psw->fIconUnmapped = True;
  if ( pswSrc->fMapPending )
    psw->fMapPending = True;
  if ( pswSrc->fHintOverride )
    psw->fHintOverride = True;
  if ( pswSrc->fMWMButtons )
    psw->fMWMButtons = True;
  if ( pswSrc->fMWMBorders )
    psw->fMWMBorders = True;
  if ( pswSrc->fMWMFunctions )
    psw->fMWMFunctions = True;
  if ( pswSrc->fMWMDecor )
    psw->fMWMDecor = True;
  if ( pswSrc->fDecorateTransient )
    psw->fDecorateTransient = True;
  if ( pswSrc->fWindowShaded )
    psw->fWindowShaded = True;
  if ( pswSrc->fStartsOnDesk )
    psw->fStartsOnDesk = True;
  if ( pswSrc->fOLDecorHint )
    psw->fOLDecorHint = True;
  if ( pswSrc->fNoPPosition )
    psw->fNoPPosition = True;
  if ( pswSrc->fForceIcon )
    psw->fForceIcon = True;
}

void
CopyAllFlags(ScwmWindow *psw, const ScwmWindow *pswSrc)
{
  CopyCommonFlags(psw, pswSrc);

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

  schwin = (scwm_window *) safemalloc(sizeof(scwm_window));

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


SCM_PROC(s_window_p, "window?", 1, 0, 0,  window_p);

SCM 
window_p(SCM obj)
{
  return SCM_BOOL_FromBool(WINDOWP(obj));
}


SCM_PROC(s_get_window, "get-window", 0, 3, 0,  get_window);

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

SCM_PROC(s_current_window_with_focus, "current-window-with-focus", 0, 0, 0, current_window_with_focus);

SCM
current_window_with_focus()
{
  return Scr.Hilite? Scr.Hilite->schwin : SCM_BOOL_F;
}


SCM_PROC(s_current_window_with_pointer, "current-window-with-pointer", 0, 0, 0, current_window_with_pointer);

SCM
current_window_with_pointer()
{
  ScwmWindow *psw = SwFromPointerLocation(dpy);
  return psw? psw->schwin: SCM_BOOL_F;
}


SCM_PROC(s_select_window_interactively, "select-window-interactively", 0, 0, 0, select_window_interactively);

SCM
select_window_interactively()
{
  ScwmWindow *psw = SwSelectInteractively(dpy);
  return psw? psw->schwin: SCM_BOOL_F;
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
  ScwmWindow *psw;
  if (XFindContext(dpy, w, ScwmContext, (caddr_t *) &psw) == XCNOENT) {
    return NULL;
  }
  return psw;
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
 *      ppsw    - pointer to pointer to ScwmWindow Structure to patch up
 *	context	- the context in which the mouse button was pressed
 *	func	- the function to defer
 *	cursor	- the cursor to display while waiting
 *      finishEvent - ButtonRelease or ButtonPress; tells what kind of event to
 *                    terminate on.
 *
 ***********************************************************************/
static
int 
DeferExecution(XEvent * eventp, Window * w, ScwmWindow **ppsw,
	       enum cursor cursor, int FinishEvent)
{
  Bool fDone = False;
  Bool fFinished = False;
  Window dummy;
  Window original_w;

  original_w = *w;

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
    /* FIXGJB: scheme callback, not bell */
    XBell(dpy, Scr.screen);
    UngrabEm();
    return True;
  }
  *ppsw = SwFromWindow(dpy,*w);
  if (*ppsw == NULL) {
    /* FIXGJB: scheme callback, not bell */
    XBell(dpy, Scr.screen);
    UngrabEm();
    return (True);
  }
  if (*w == (*ppsw)->Parent)
    *w = (*ppsw)->w;

  if (original_w == (*ppsw)->Parent)
    original_w = (*ppsw)->w;

  /* this ugly mess attempts to ensure that the release and press
   * are in the same window. */
  if ((*w != original_w) && (original_w != Scr.Root) &&
      (original_w != None) && (original_w != Scr.NoFocusWin))
    if (!((*w == (*ppsw)->frame) &&
	  (original_w == (*ppsw)->w))) {
      /* FIXGJB: scheme callback, not bell */
      XBell(dpy, Scr.screen);
      UngrabEm();
      return True;
    }

  UngrabEm();
  /* interactive operations should not use the stashed mouse position
     if we just selected the window. */
  have_orig_position = 0; 
  return False; 
}


SCM_PROC(s_select_window, "select-window", 0, 2, 0,  select_window);

SCM 
select_window(SCM kill_p, SCM release_p)
{
  XEvent ev;
  Window w;
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  w = Scr.Root;

  psw = &Scr.ScwmRoot;

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


  if (DeferExecution(&ev, &w, &psw,
		     (kill_p != SCM_BOOL_F ? CURSOR_DESTROY : CURSOR_SELECT),
		     (release_p != SCM_BOOL_F ? ButtonRelease :
		      ButtonPress))) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  /* XXX - this needs to done right.  (Was != NULL before --10/24/97 gjb ) */
  if (psw && psw->schwin != SCM_UNDEFINED) {
    SCM_REALLOW_INTS;
    return (psw->schwin);
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
  /* FIXGJB: this should be a runtime option */
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
DestroyScwmWindow(ScwmWindow *psw)
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
  if (!psw)
    return;

  XUnmapWindow(dpy, psw->frame);

  if (!PPosOverride)
    XSync(dpy, 0);

  if (psw == Scr.Hilite)
    Scr.Hilite = NULL;

  Broadcast(M_DESTROY_WINDOW, 3, psw->w, psw->frame,
	    (unsigned long) psw, 0, 0, 0, 0);

  if (Scr.PreviousFocus == psw)
    Scr.PreviousFocus = NULL;

  if (ButtonWindow == psw)
    ButtonWindow = NULL;

  if ((psw == Scr.Focus) && psw->fClickToFocus) {
    if (psw->next) {
      HandleHardFocus(psw->next);
    } else {
      SetFocus(Scr.NoFocusWin, NULL, 1);
    }
  } else if (Scr.Focus == psw) {
    SetFocus(Scr.NoFocusWin, NULL, 1);
  }

  if (psw == FocusOnNextTimeStamp)
    FocusOnNextTimeStamp = NULL;

  if (psw == Scr.Ungrabbed)
    Scr.Ungrabbed = NULL;

  if (psw == Scr.pushed_window)
    Scr.pushed_window = NULL;

  if (psw == colormap_win)
    colormap_win = NULL;

  XDestroyWindow(dpy, psw->frame);
  XDeleteContext(dpy, psw->frame, ScwmContext);

  XDestroyWindow(dpy, psw->Parent);

  XDeleteContext(dpy, psw->Parent, ScwmContext);

  XDeleteContext(dpy, psw->w, ScwmContext);

  if (psw->icon_w && psw->fPixmapOurs &&
      psw->icon_image != SCM_BOOL_F) {
    XFreePixmap(dpy, IMAGE(psw->icon_image)->image);
  }

  if (psw->icon_w) {
    XDestroyWindow(dpy, psw->icon_w);
    XDeleteContext(dpy, psw->icon_w, ScwmContext);
  }
  if (psw->fIconOurs && (psw->icon_pixmap_w != None))
    XDestroyWindow(dpy, psw->icon_pixmap_w);
  if (psw->icon_pixmap_w != None)
    XDeleteContext(dpy, psw->icon_pixmap_w, ScwmContext);

  if (psw->fTitle) {
    XDeleteContext(dpy, psw->title_w, ScwmContext);
    for (i = 0; i < Scr.nr_left_buttons; i++)
      XDeleteContext(dpy, psw->left_w[i], ScwmContext);
    for (i = 0; i < Scr.nr_right_buttons; i++)
      if (psw->right_w[i] != None)
	XDeleteContext(dpy, psw->right_w[i], ScwmContext);
  }
  if (psw->fBorder) {
    for (i = 0; i < 4; i++)
      XDeleteContext(dpy, psw->sides[i], ScwmContext);
    for (i = 0; i < 4; i++)
      XDeleteContext(dpy, psw->corners[i], ScwmContext);
  }
  psw->prev->next = psw->next;
  if (psw->next != NULL)
    psw->next->prev = psw->prev;
  free_window_names(psw, True, True);
  if (psw->wmhints)
    XFree((char *) psw->wmhints);
  /* removing NoClass change for now... */
  if (psw->classhint.res_name && psw->classhint.res_name != NoResource)
    XFree((char *) psw->classhint.res_name);
  if (psw->classhint.res_class && psw->classhint.res_class != NoClass)
    XFree((char *) psw->classhint.res_class);
  if (psw->mwm_hints)
    XFree((char *) psw->mwm_hints);

  if (psw->cmap_windows != (Window *) NULL)
    XFree((void *) psw->cmap_windows);

  /* XSCM */
  invalidate_window(psw->schwin);

  free((char *) psw);

  if (!PPosOverride)
    XSync(dpy, 0);
  return;
}


SCM_PROC(s_delete_window, "delete-window", 0, 1, 0,  delete_window);

SCM 
delete_window(SCM win)
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;

  VALIDATEKILL(win, "delete-window");

  psw = SCWMWINDOW(win);
  if (check_allowed_function(F_DELETE, psw) == 0) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  if (psw->fDoesWmDeleteWindow) {
    send_clientmessage(dpy, psw->w, _XA_WM_DELETE_WINDOW, CurrentTime);
    SCM_REALLOW_INTS;
    return SCM_BOOL_T;
  }
  SCM_REALLOW_INTS;
  return SCM_BOOL_F;
}


SCM_PROC(s_destroy_window, "destroy-window", 0, 1, 0,  destroy_window);

SCM 
destroy_window(SCM win)
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATEKILL(win, "destroy-window");
  psw = SCWMWINDOW(win);
  if (check_allowed_function(F_DESTROY, psw) == 0) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  if (XGetGeometry(dpy, psw->w, &JunkRoot, &JunkX, &JunkY,
		   &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth) == 0) {
    DestroyScwmWindow(psw);
  } else {
    XKillClient(dpy, psw->w);
  }
  XSync(dpy, 0);
  SCM_REALLOW_INTS;
  return SCM_BOOL_T;
}



SCM_PROC(s_window_deletable_p, "window-deletable?", 0, 1, 0,  window_deletable_p);

SCM 
window_deletable_p(SCM win)
{
  VALIDATEKILL(win, "window-deletable?");
  return SCM_BOOL_FromBool(SCWMWINDOW(win)->fDoesWmDeleteWindow);
}


SCM_PROC(s_focus, "focus", 0, 1, 0,  focus);

SCM 
focus(SCM win)
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, "focus");
  psw = SCWMWINDOW(win);
  FocusOn(psw, 0);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_unfocus, "unfocus", 0, 0, 0,  unfocus);

SCM
unfocus()
{
  SCM_REDEFER_INTS;
  Unfocus();
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}  


SCM_PROC(s_warp_to_window, "warp-to-window", 0, 1, 0,  warp_to_window);

SCM 
warp_to_window(SCM win)
{
  SCM_REDEFER_INTS;
  VALIDATE(win, "warp-to-window");
  WarpOn(SCWMWINDOW(win), 0, 0, 0, 0);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_raise_window, "raise-window", 0, 1, 0,  raise_window);

SCM 
raise_window(SCM win)
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, "raise-window");

  psw = SCWMWINDOW(win);

  RaiseWindow(psw);
  /* FIXMS darn, this is not going to do what we want it to -- must
     start keeping a general stays on top flag as well a currently on
     top flag in the window struct, only the latter of which is
     changed by raises and lowers. */
  KeepOnTop();
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_lower_window, "lower-window", 0, 1, 0,  lower_window);

SCM 
lower_window(SCM win)
{
  SCM_REDEFER_INTS;
  VALIDATE(win, "lower-window");
  LowerWindow(SCWMWINDOW(win));
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_raised_p, "raised?", 0, 1, 0,  raised_p);

SCM 
raised_p(SCM win)
{
  ScwmWindow *psw;

  VALIDATE(win, "raised?");
  psw = SCWMWINDOW(win);
  return SCM_BOOL_FromBool(psw == Scr.LastWindowRaised ||
			   psw->fVisible);
}


SCM_PROC(s_transient_p, "transient?", 0, 1, 0,  transient_p);

SCM 
transient_p(SCM win)
{
  ScwmWindow *psw;

  VALIDATE(win, "transient?");
  psw = SCWMWINDOW(win);
  return SCM_BOOL_FromBool(psw->fTransient);
}



SCM_PROC(s_iconify, "iconify", 0, 1, 0,  iconify);

SCM 
iconify(SCM win)
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, "iconify");
  psw = SCWMWINDOW(win);

  if (check_allowed_function(F_ICONIFY, psw) == 0) {

    return SCM_BOOL_F;
  }
  Iconify(psw, 0, 0);

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_deiconify, "deiconify", 0, 1, 0,  deiconify);

SCM 
deiconify(SCM win)
{
  SCM_REDEFER_INTS;
  VALIDATE(win, "deiconify");
  DeIconify(SCWMWINDOW(win));
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_iconified_p, "iconified?", 0, 1, 0,  iconified_p);

SCM 
iconified_p(SCM win)
{
  VALIDATE(win, "iconified?");
  return SCM_BOOL_FromBool(SCWMWINDOW(win)->fIconified);
}


SCM_PROC(s_stick, "stick", 0, 1, 0,  stick);

SCM 
stick(SCM win)
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, "stick");
  psw = SCWMWINDOW(win);
  psw->fSticky = True;
  BroadcastConfig(M_CONFIGURE_WINDOW, psw);
  SetTitleBar(psw, (Scr.Hilite == psw), True);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_unstick, "unstick", 0, 1, 0,  unstick);

SCM 
unstick(SCM win)
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, "unstick");
  psw = SCWMWINDOW(win);
  psw->fSticky = False;
  BroadcastConfig(M_CONFIGURE_WINDOW, psw);
  SetTitleBar(psw, (Scr.Hilite == psw), True);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}

SCM_PROC(s_sticky_p, "sticky?", 0, 1, 0,  sticky_p);

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

SCM_PROC(s_window_shade, "window-shade", 0, 2, 0,  window_shade);

SCM 
window_shade(SCM win, SCM animated_p)
{
  ScwmWindow *psw;
  Bool fAnimated = False;

  SCM_REDEFER_INTS;
  VALIDATE(win, "window-shade");
  psw = SCWMWINDOW(win);

  if (!psw->fTitle || psw->fMaximized) {
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

  SET_SHADED(psw);
  
  if (fAnimated) {
    AnimatedShadeWindow(psw,True /* roll up */, -1, NULL);
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
  SetupFrame(psw, psw->frame_x, psw->frame_y, psw->frame_width,
	     psw->title_height + psw->boundary_width, False);
  if (fAnimated) {
    /* need to reset the client window offset so that if
       if it's un-window-shaded w/o animation, things are ok */
    XMoveWindow(dpy,psw->w,0,0);
  }

  CoerceEnterNotifyOnCurrentWindow();
  Broadcast(M_WINDOWSHADE, 1, psw->w, 0, 0, 0, 0, 0, 0);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_un_window_shade, "un-window-shade", 0, 2, 0,  un_window_shade);

SCM 
un_window_shade(SCM win, SCM animated_p)
{
  ScwmWindow *psw;
  Bool fAnimated = False;

  SCM_REDEFER_INTS;
  VALIDATE(win, "un-window-shade");
  psw = SCWMWINDOW(win);

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

  SET_UNSHADED(psw);
  if (fAnimated) {
    AnimatedShadeWindow(psw,False /* !roll up */, -1, NULL);
  }
  SetupFrame(psw, psw->frame_x, psw->frame_y, 
	     psw->orig_wd, psw->orig_ht, True);
  Broadcast(M_DEWINDOWSHADE, 1, psw->w, 0, 0, 0, 0, 0, 0);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_window_shaded_p, "window-shaded?", 0, 1, 0,  window_shaded_p);

SCM 
window_shaded_p(SCM win)
{
  VALIDATE(win, "window-shaded?");
  return SCM_BOOL_FromBool(SHADED_P(SCWMWINDOW(win)));
}


void 
move_finalize(Window w, ScwmWindow * psw, int x, int y)
{
  if (w == psw->frame) {
    SetupFrame(psw, x, y,
	       psw->frame_width, psw->frame_height, False);
  } else {			/* icon window */
    psw->fIconMoved = True;
    psw->icon_x_loc = x;
    psw->icon_xl_loc = y - (psw->icon_w_width - psw->icon_p_width) / 2;
    psw->icon_y_loc = y;
    Broadcast(M_ICON_LOCATION, 7, psw->w, psw->frame,
	      (unsigned long) psw,
	      psw->icon_x_loc, psw->icon_y_loc,
	      psw->icon_w_width, psw->icon_w_height
	      + psw->icon_p_height);
    XMoveWindow(dpy, psw->icon_w,
		psw->icon_xl_loc, y + psw->icon_p_height);
    if (psw->icon_pixmap_w != None) {
      XMapWindow(dpy, psw->icon_w);
      XMoveWindow(dpy, psw->icon_pixmap_w, psw->icon_x_loc, y);
      XMapWindow(dpy, w);
    }
  }
}



extern float rgpctMovementDefault[32];
extern int cpctMovementDefault;
extern int cmsDelayDefault;
extern int c10msDelaysBeforePopup;


SCM_PROC(s_set_animation_x, "set-animation!", 1,0,0,  set_animation_x);

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
    SCM val = gh_vector_ref(vector,gh_int2scm(i));    
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
 

SCM_PROC(s_move_to, "move-to", 2, 3, 0,  move_to);

SCM 
move_to(SCM x, SCM y, SCM win, SCM animated_p, SCM move_pointer_too_p)
{
  ScwmWindow *psw;
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
  psw = SCWMWINDOW(win);
  w = psw->frame;
  if (psw->fIconified) {
    if (psw->icon_pixmap_w != None) {
      XUnmapWindow(dpy, psw->icon_w);
      w = psw->icon_pixmap_w;
    } else
      w = psw->icon_w;
  }
  destX = gh_scm2int(x);
  destY = gh_scm2int(y);
  fMovePointer = gh_scm2bool(move_pointer_too_p);
  fAnimated = gh_scm2bool(animated_p);
  if (fMovePointer || fAnimated) {
    XGetWindowTopLeft(w,&startX, &startY);
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
    XGetPointerWindowOffsets(Scr.Root, &x, &y);
    XWarpPointer(dpy, Scr.Root, Scr.Root, 0, 0, Scr.MyDisplayWidth,
		 Scr.MyDisplayHeight, x + destX - startX, y + destY - startY);
  }

  move_finalize(w, psw, destX, destY);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_interactive_move, "interactive-move", 0, 1, 0,  interactive_move);

SCM 
interactive_move(SCM win)
{
  ScwmWindow *psw;
  Window w;
  XEvent event;
  int x, y;

  SCM_REDEFER_INTS;
  VALIDATE_PRESS_ONLY(win, "interactive-move");
  psw = SCWMWINDOW(win);
  w = psw->frame;
  if (psw->fIconified) {
    if (psw->icon_pixmap_w != None) {
      XUnmapWindow(dpy, psw->icon_w);
      w = psw->icon_pixmap_w;
    } else {
      w = psw->icon_w;
    }
  }
  if (have_orig_position) {
    event.xbutton.x_root = orig_x;
    event.xbutton.y_root = orig_y;
  } else {
    XGetPointerWindowOffsets(Scr.Root, &event.xbutton.x_root, &event.xbutton.y_root);
  }
  InteractiveMove(&w, psw, &x, &y, &event);
  move_finalize(w, psw, x, y);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_resize_to, "resize-to", 2, 1, 0,  resize_to);

SCM 
resize_to(SCM w, SCM h, SCM win)
{
  int width, height;
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATEN(win, 3, "resize-to");
  psw = SCWMWINDOW(win);

  if (check_allowed_function(F_RESIZE, psw) == 0
      || SHADED_P(psw)) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  psw->fMaximized = False;

  /* can't resize icons */
  if (psw->fIconified) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  width = gh_scm2int(w);
  height = gh_scm2int(h);

  /* took the next two lines out because we can do that in scheme if we 
     really want, and maximize gets broken otherwise. */
  /*
     width += (2*psw->boundary_width);
     height += (psw->title_height + 2*psw->boundary_width);
   */

  ConstrainSize(psw, &width, &height);
  SetupFrame(psw, psw->frame_x,
	     psw->frame_y, width, height, False);

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
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


SCM_PROC(s_interactive_resize, "interactive-resize", 0, 1, 0,  interactive_resize);

SCM 
interactive_resize(SCM win)
{
  ScwmWindow *psw;
  Bool finished = False, done = False, abort = False;
  int x, y, delta_x, delta_y;
  Window ResizeWindow;
  Bool flags;

  VALIDATE_PRESS_ONLY(win, "interactive-resize");
  psw = SCWMWINDOW(win);


  if (check_allowed_function(F_RESIZE, psw) == 0
      || SHADED_P(psw)) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  psw->fMaximized = False;

  if (psw->fIconified) {
    SCM_REALLOW_INTS;
    return SCM_BOOL_F;
  }
  ResizeWindow = psw->frame;


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

  dragx += psw->bw;
  dragy += psw->bw;
  origx = dragx;
  origy = dragy;
  origWidth = dragWidth;
  origHeight = dragHeight;
  ymotion = xmotion = 0;

  /* pop up a resize dimensions window */
  XMapRaised(dpy, Scr.SizeWindow);
  last_width = 0;
  last_height = 0;
  DisplaySize(psw, origWidth, origHeight, True);

  /* Get the current position to determine which border to resize */
  if ((PressedW != Scr.Root) && (PressedW != None)) {
    if (PressedW == psw->sides[0])	/* top */
      ymotion = 1;
    if (PressedW == psw->sides[1])	/* right */
      xmotion = -1;
    if (PressedW == psw->sides[2])	/* bottom */
      ymotion = -1;
    if (PressedW == psw->sides[3])	/* left */
      xmotion = 1;
    if (PressedW == psw->corners[0]) {	/* upper-left */
      ymotion = 1;
      xmotion = 1;
    }
    if (PressedW == psw->corners[1]) {	/* upper-right */
      xmotion = -1;
      ymotion = 1;
    }
    if (PressedW == psw->corners[2]) {	/* lower left */
      ymotion = -1;
      xmotion = 1;
    }
    if (PressedW == psw->corners[3]) {	/* lower right */
      ymotion = -1;
      xmotion = -1;
    }
  }
  /* draw the rubber-band window */
  MoveOutline(Scr.Root, dragx - psw->bw, dragy - psw->bw,
	      dragWidth + 2 * psw->bw,
	      dragHeight + 2 * psw->bw);

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
      DoResize(x, y, psw);
      /* need to move the viewport */
      HandlePaging(Scr.EdgeScrollX, Scr.EdgeScrollY, &x, &y,
		   &delta_x, &delta_y, False);
      /* redraw outline if we paged - mab */
      if ((delta_x != 0) || (delta_y != 0)) {
	origx -= delta_x;
	origy -= delta_y;
	dragx -= delta_x;
	dragy -= delta_y;

	DoResize(x, y, psw);
      }
      done = True;
    default:
      break;
    }
    if (!done) {
      MoveOutline(Scr.Root, 0, 0, 0, 0);

      DispatchEvent();

      MoveOutline(Scr.Root, dragx - psw->bw, dragy - psw->bw,
		  dragWidth + 2 * psw->bw, dragHeight + 2 * psw->bw);

    }
  }

  /* erase the rubber-band */
  MoveOutline(Scr.Root, 0, 0, 0, 0);

  /* pop down the size window */
  XUnmapWindow(dpy, Scr.SizeWindow);

  if (!abort) {
    ConstrainSize(psw, &dragWidth, &dragHeight);
    SetupFrame(psw, dragx - psw->bw,
	       dragy - psw->bw, dragWidth, dragHeight, False);
  }
  UninstallRootColormap();
  ResizeWindow = None;
  XUngrabServer_withSemaphore(dpy);
  UngrabEm();
  xmotion = 0;
  ymotion = 0;

  Scr.flags |= flags & (EdgeWrapX | EdgeWrapY);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_refresh_window, "refresh-window", 0, 1, 0,  refresh_window);

SCM 
refresh_window(SCM win)
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, "refresh-window");
  psw = SCWMWINDOW(win);

  refresh_common(psw->fIconified ?
		 (psw->icon_w) : (psw->frame));

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_move_window_to_desk, "move-window-to-desk", 1, 1, 0,  move_window_to_desk);

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
  return SCM_UNSPECIFIED;;
}


SCM_PROC(s_window_position, "window-position", 0, 1, 0,  window_position);

SCM 
window_position(SCM win)
{
  ScwmWindow *psw;

  VALIDATE(win, "window-position");
  psw = SCWMWINDOW(win);

  return scm_listify(SCM_MAKINUM(psw->frame_x),
		     SCM_MAKINUM(psw->frame_y),
		     SCM_UNDEFINED);
}


SCM_PROC(s_window_size, "window-size", 0, 1, 0,  window_size);

SCM 
window_size(SCM win)
{
  ScwmWindow *psw;

  VALIDATE(win, "window-size");
  psw = SCWMWINDOW(win);

  return scm_listify(SCM_MAKINUM(psw->frame_width),
		     SCM_MAKINUM(psw->frame_height),
		     SCM_UNDEFINED);
}


SCM_PROC(s_window_id, "window-id", 0, 1, 0,  window_id);

SCM 
window_id(SCM win)
{
  VALIDATE(win, "window-id");
  return SCM_MAKINUM(SCWMWINDOW(win)->w);
}


SCM_PROC(s_window_frame_id, "window-frame-id", 0, 1, 0,  window_frame_id);

SCM 
window_frame_id(SCM win)
{
  VALIDATE(win, "window-frame-id");
  return SCM_MAKINUM(SCWMWINDOW(win)->frame);
}


SCM_PROC(s_window_from_window_id, "window-from-window-id", 0, 1, 0,  window_from_window_id);

SCM
window_from_window_id(SCM window_id)
{
  Window w;
  ScwmWindow *psw = NULL;
  if (!gh_number_p(window_id)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(__FUNCTION__, 1, window_id);
  }
  w = gh_scm2int(window_id);
  psw = SwFromWindow(dpy, (Window) w);
  return (psw?psw->schwin : SCM_BOOL_F);
}


SCM_PROC(s_window_desk, "window-desk", 0, 1, 0,  window_desk);

SCM 
window_desk(SCM win)
{
  VALIDATE(win, "window-desk");
  return SCM_MAKINUM(SCWMWINDOW(win)->Desk);
}


SCM_PROC(s_window_title, "window-title", 0, 1, 0,  window_title);

SCM 
window_title(SCM win)
{
  VALIDATE(win, "window-title");
  return gh_str02scm(SCWMWINDOW(win)->name);
}


SCM_PROC(s_window_class, "window-class", 0, 1, 0,  window_class);

SCM 
window_class(SCM win)
{
  VALIDATE(win, "window-class");
  return gh_str02scm(SCWMWINDOW(win)->classhint.res_class);
}


SCM_PROC(s_window_resource, "window-resource", 0, 1, 0,  window_resource);

SCM 
window_resource(SCM win)
{
  VALIDATE(win, "window-resource");
  return gh_str02scm(SCWMWINDOW(win)->classhint.res_name);
}


SCM_PROC(s_list_all_windows, "list-all-windows", 0, 0, 0,  list_all_windows);

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


SCM_PROC(s_keep_on_top, "keep-on-top", 0, 1, 0,  keep_on_top);

SCM 
keep_on_top(SCM win)
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, "keep-on-top");
  psw = SCWMWINDOW(win);
  psw->fOnTop = True;
  /* is this needed? */
  BroadcastConfig(M_CONFIGURE_WINDOW, psw);
  raise_window(win);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_un_keep_on_top, "un-keep-on-top", 0, 1, 0,  un_keep_on_top);

SCM 
un_keep_on_top(SCM win)
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, "un-keep-on-top");
  psw = SCWMWINDOW(win);
  psw->fOnTop = False;
  /* is this needed? */
  BroadcastConfig(M_CONFIGURE_WINDOW, psw);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_kept_on_top_p, "kept-on-top?", 0, 1, 0,  kept_on_top_p);

SCM 
kept_on_top_p(SCM win)
{
  VALIDATE(win, "kept-on-top?");
  return SCM_BOOL_FromBool(SCWMWINDOW(win)->fOnTop);
}


/* maybe all of this can be replaced with set-title-height 
   (a per-window version) ? */

SCM_PROC(s_show_titlebar, "show-titlebar", 0, 1, 0,  show_titlebar);

SCM 
show_titlebar(SCM win)
{
  ScwmWindow *psw;
  ScwmDecor *fl;

  SCM_REDEFER_INTS;

  VALIDATE(win, "show-titlebar");
  psw = SCWMWINDOW(win);
  fl = psw->fl ? psw->fl : &Scr.DefaultDecor;


  if (!psw->fTitle) {
    psw->fTitle = True;
    BroadcastConfig(M_CONFIGURE_WINDOW, psw);
    SetupFrame(psw, psw->frame_x, psw->frame_y,
	       psw->frame_width,
	       psw->frame_height + fl->TitleHeight,
	       True);
    /* SetTitleBar(psw,(Scr.Hilite==psw),True); */
  }
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_hide_titlebar, "hide-titlebar", 0, 1, 0,  hide_titlebar);

SCM 
hide_titlebar(SCM win)
{
  ScwmWindow *psw;
  ScwmDecor *fl;

  SCM_REDEFER_INTS;

  VALIDATE(win, "hide-titlebar");
  psw = SCWMWINDOW(win);
  fl = psw->fl ? psw->fl : &Scr.DefaultDecor;

  if (psw->fTitle) {
    psw->fTitle = False;
    psw->title_height = 0;
    BroadcastConfig(M_CONFIGURE_WINDOW, psw);
    SetupFrame(psw, psw->frame_x, psw->frame_y,
	       psw->frame_width,
	       psw->frame_height - fl->TitleHeight,
	       True);
  }
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_titlebar_shown_p, "titlebar-shown?", 0, 1, 0,  titlebar_shown_p);

SCM 
titlebar_shown_p(SCM win)
{
  VALIDATE(win, "titlebar-shown?");
  return SCM_BOOL_FromBool(SCWMWINDOW(win)->fTitle);
}


SCM_PROC(s_normal_border, "normal-border", 0, 1, 0,  normal_border);

SCM 
normal_border(SCM win)
{
  ScwmWindow *psw;
  ScwmDecor *fl;
  int i;

  SCM_REDEFER_INTS;
  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  VALIDATE(win, "normal-border");
  psw = SCWMWINDOW(win);
  psw->fBorder = True;
  BroadcastConfig(M_CONFIGURE_WINDOW, psw);
  for (i = 0; i < 4; i++) {
    XMapWindow(dpy, psw->corners[i]);
    XMapWindow(dpy, psw->sides[i]);
  }

  SetBorderX(psw, (Scr.Hilite == psw), True, True, None, True);

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_plain_border, "plain-border", 0, 1, 0,  plain_border);

SCM 
plain_border(SCM win)
{
  ScwmWindow *psw;
  int i;

  SCM_REDEFER_INTS;

  VALIDATE(win, "plain-border");
  psw = SCWMWINDOW(win);
  psw->fBorder = False;
  BroadcastConfig(M_CONFIGURE_WINDOW, psw);

  for (i = 0; i < 4; i++) {
    XUnmapWindow(dpy, psw->corners[i]);
    XUnmapWindow(dpy, psw->sides[i]);
  }

  SetBorderX(psw, (Scr.Hilite == psw), True, True, None, True);

  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_border_normal_p, "border-normal?", 0, 1, 0,  border_normal_p);

SCM 
border_normal_p(SCM win)
{
  VALIDATE(win, "border-normal?");
  return SCM_BOOL_FromBool(SCWMWINDOW(win)->fBorder);
}


SCM_PROC(s_set_border_width_x, "set-border-width!", 1, 1, 0,  set_border_width_x);

SCM 
set_border_width_x(SCM width, SCM win)
{
  ScwmWindow *psw;
  ScwmDecor *fl;
  int w, oldw;

  SCM_REDEFER_INTS;
  fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (!gh_number_p(width)) {
    scm_wrong_type_arg("set-border-width!", 1, width);
  }
  w = gh_scm2int(width);

  VALIDATEN(win, 2, "set-border-width!");
  psw = SCWMWINDOW(win);
  oldw = psw->boundary_width;
  psw->boundary_width = w;

  SetupFrame(psw, psw->frame_x, psw->frame_y,
	     psw->frame_width + 2 * (w - oldw),
	     psw->frame_height + 2 * (w - oldw),
	     True);


  BroadcastConfig(M_CONFIGURE_WINDOW, psw);
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_stick_icon, "stick-icon", 0, 1, 0,  stick_icon);

SCM 
stick_icon(SCM win)
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, "stick-icon");
  psw = SCWMWINDOW(win);
  psw->fStickyIcon = True;
  BroadcastConfig(M_CONFIGURE_WINDOW, psw);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_unstick_icon, "unstick-icon", 0, 1, 0,  unstick_icon);

SCM 
unstick_icon(SCM win)
{
  ScwmWindow *psw;

  SCM_REDEFER_INTS;
  VALIDATE(win, "unstick-icon");
  psw = SCWMWINDOW(win);
  psw->fStickyIcon = False;
  BroadcastConfig(M_CONFIGURE_WINDOW, psw);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_icon_sticky_p, "icon-sticky?", 0, 1, 0,  icon_sticky_p);

SCM 
icon_sticky_p(SCM win)
{
  VALIDATE(win, "icon-sticky?");
  return SCM_BOOL_FromBool(SCWMWINDOW(win)->fStickyIcon);
}


SCM_PROC(s_set_icon_box_x, "set-icon-box!", 4, 1, 0,  set_icon_box_x);

SCM 
set_icon_box_x(SCM sx, SCM sy, SCM sw, SCM sh, SCM win)
{
  /* XXX - should probably move existing window icons */
  int x, y, w, h;
  ScwmWindow *psw;

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
  psw = SCWMWINDOW(win);
  x = gh_scm2int(sx);
  y = gh_scm2int(sy);
  w = gh_scm2int(sw);
  h = gh_scm2int(sh);
  psw->IconBox[0] = x;
  psw->IconBox[1] = y;
  psw->IconBox[2] = x + w;
  psw->IconBox[3] = y + h;
  return (SCM_BOOL_T);
}


SCM_PROC(s_set_window_focus_x, "set-window-focus!", 1, 1, 0,  set_window_focus_x);

SCM 
set_window_focus_x(SCM sym, SCM win)
{
  ScwmWindow *psw;

  if (!gh_symbol_p(sym)) {
    scm_wrong_type_arg("set-window-focus!", 1, sym);
  }
  VALIDATEN(win, 2, "set-window-focus!");
  psw = SCWMWINDOW(win);

  if (gh_eq_p(sym, sym_mouse)) {
    psw->fClickToFocus = False;
    psw->fSloppyFocus = False;
  } else if (gh_eq_p(sym, sym_click)) {
    psw->fClickToFocus = True;
    psw->fSloppyFocus = False;
  } else if (gh_eq_p(sym, sym_sloppy)) {
    psw->fClickToFocus = False;
    psw->fSloppyFocus = True;
  } else if (gh_eq_p(sym, sym_none)) {
    psw->fClickToFocus = True;
    psw->fSloppyFocus = True;
  } else {
    scwm_error("set-window-focus!", 13);
  }
  return sym;
}


SCM_PROC(s_set_window_foreground_x, "set-window-foreground!", 1, 1, 0,  set_window_foreground_x);

SCM
set_window_foreground_x(SCM fg, SCM win)
{
  ScwmWindow *psw;

  VALIDATE_COLOR(fg, "set-window-foreground!", 1);

  VALIDATEN(win, 2, "set-window-foreground!");
  psw = SCWMWINDOW(win);

  psw->TextColor = fg;
  SetBorderX(psw, (Scr.Hilite == psw), True, True, None, True);

  return SCM_UNSPECIFIED;  
}


SCM_PROC(s_set_window_background_x, "set-window-background!", 1, 1, 0,  set_window_background_x);

SCM
set_window_background_x(SCM bg, SCM win)
{
  ScwmDecor * fl;
  ScwmWindow *psw;

  VALIDATE_COLOR(bg, "set-window-background!", 1);

  VALIDATEN(win, 2, "set-window-background!");
  psw = SCWMWINDOW(win);
  fl = psw->fl ? psw->fl : &Scr.DefaultDecor;


  psw->BackColor = bg;
  psw->ShadowColor = adjust_brightness(psw->BackColor, fl->shadow_factor);
  psw->ReliefColor = adjust_brightness(psw->BackColor, fl->hilight_factor);

  SetBorderX(psw, (Scr.Hilite == psw), True, True, None, True);

  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_random_placement_x, "set-random-placement!", 1, 1, 0,  set_random_placement_x);

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
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_smart_placement_x, "set-smart-placement!", 1, 1, 0,  set_smart_placement_x);

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
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_window_button_x, "set-window-button!", 2, 1, 0,  set_window_button_x);

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

  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_mwm_buttons_x, "set-mwm-buttons!", 1, 1, 0,  set_mwm_buttons_x);

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
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_mwm_border_x, "set-mwm-border!", 1, 1, 0,  set_mwm_border_x);

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

  return SCM_UNSPECIFIED;
}


void
force_icon_redraw (ScwmWindow *psw)
{
  XDestroyWindow(dpy, psw->icon_w);
  psw->icon_w = None;

  if (psw->fIconified) {
    Iconify(psw, 0, 0);
  }  
}


SCM_PROC(s_set_icon_title_x, "set-icon-title!", 1, 1, 0,  set_icon_title_x);

SCM 
set_icon_title_x(SCM title, SCM win)
{
  ScwmWindow *psw;

  /* Should changing the icon title string be allowed? */

  VALIDATEN(win, 2, "set-icon-title!");
  psw = SCWMWINDOW(win);

  if (title == SCM_BOOL_F) {
    psw->fNoIconTitle = True;
  } else if (title == SCM_BOOL_T) {
    psw->fNoIconTitle = False;
  } else {
    scm_wrong_type_arg("set-icon-title!", 1, title);
  }

  force_icon_redraw (psw);

  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_force_icon_x, "set-force-icon!", 1, 1, 0,  set_force_icon_x);

SCM
set_force_icon_x (SCM flag, SCM win)
{
  ScwmWindow *psw;

  VALIDATEN(win, 2, "set-force-icon!");
  psw = SCWMWINDOW(win);

  if (flag== SCM_BOOL_F) {
    psw->fForceIcon=False; 
  } else if (flag== SCM_BOOL_T) {
    psw->fForceIcon=True; 
  } else {
    scm_wrong_type_arg("set-force-icon!", 1, flag);
  }

  force_icon_redraw (psw);
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_show_icon_x, "set-show-icon!", 1, 1, 0, set_show_icon_x); 

SCM 
set_show_icon_x (SCM flag, SCM win)
{
  ScwmWindow *psw;

  VALIDATEN(win, 2, "set-show-icon!");
  psw = SCWMWINDOW(win);

  if (flag== SCM_BOOL_F) {
    psw->fSuppressIcon = True;
  } else if (flag == SCM_BOOL_T) {
    psw->fSuppressIcon = False;
  } else {
    scm_wrong_type_arg("set-show-icon!", 1, flag);
  }

  force_icon_redraw (psw);

  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_icon_x, "set-icon!", 1, 1, 0,  set_icon_x);

SCM 
set_icon_x(SCM picture, SCM win)
{
  ScwmWindow *psw;

  VALIDATEN(win, 2, "set-icon!");
  psw = SCWMWINDOW(win);
  if (gh_string_p(picture)) {
    psw->icon_req_image = make_image(picture);
  } else if (IMAGE_P(picture) || picture == SCM_BOOL_F) {
    psw->icon_req_image = picture;
  } else {
    scm_wrong_type_arg("set-icon!", 1, picture);
  }

  force_icon_redraw (psw);
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_mini_icon_x, "set-mini-icon!", 1, 1, 0,  set_mini_icon_x);

SCM 
set_mini_icon_x(SCM image, SCM win)
{
  ScwmWindow *psw;

  VALIDATEN(win, 2, "set-mini-icon!");
  psw = SCWMWINDOW(win);
  if (image == SCM_BOOL_F) {
    psw->mini_icon_image = SCM_BOOL_F;
  } else if (gh_string_p(image)) {
    psw->mini_icon_image = make_image(image);
  } else if (IMAGE_P(image)) {
    psw->mini_icon_image = image;
  } else {
    scm_wrong_type_arg("set-mini-icon!", 1, image);
  }

  /* Broadcast the new mini-icon or something? */
  if (psw->mini_icon_image != SCM_BOOL_F) {
    Broadcast(M_MINI_ICON, 6,
	      psw->w,	/* Watch Out ! : I reduced the set of infos... */
	      IMAGE(psw->mini_icon_image)->image,
	      IMAGE(psw->mini_icon_image)->mask,
	      IMAGE(psw->mini_icon_image)->width,
	      IMAGE(psw->mini_icon_image)->height,
	      IMAGE(psw->mini_icon_image)->depth, 0);
  }

  SetBorderX(psw, Scr.Hilite == psw, True, psw->fMapped, None, True);

  return SCM_UNSPECIFIED;

}


SCM_PROC(s_set_hint_override_x, "set-hint-override!", 1, 1, 0,  set_hint_override_x);

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
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_decorate_transient_x, "set-decorate-transient!", 1, 1, 0,  set_decorate_transient_x);

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
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_mwm_decor_hint_x, "set-mwm-decor-hint!", 1, 1, 0,  set_mwm_decor_hint_x);

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
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_mwm_func_hint_x, "set-mwm-func-hint!", 1, 1, 0,  set_mwm_func_hint_x);

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
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_PPosition_hint_x, "set-PPosition-hint!", 1, 1, 0,  set_PPosition_hint_x);

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
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_OL_decor_hint_x, "set-OL-decor-hint!", 1, 1, 0,  set_OL_decor_hint_x);

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
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_start_on_desk_x, "set-start-on-desk!", 1, 1, 0,  set_start_on_desk_x);

SCM 
set_start_on_desk_x(SCM desk, SCM win)
{
  ScwmWindow *psw;

  VALIDATEN(win, 2, "set-start-on-desk!");
  psw = SCWMWINDOW(win);
  if (desk == SCM_BOOL_F) {
    psw->fStartsOnDesk = False;
  } else if (gh_number_p(desk)) {
    DBUG(__FUNCTION__,"setting fStartsOnDesk");
    psw->fStartsOnDesk = True;
    psw->StartDesk = gh_scm2int(desk);
  } else {
    scm_wrong_type_arg("set-start-on-desk!", 1, desk);
  }
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_skip_mapping_x, "set-skip-mapping!", 1, 1, 0,  set_skip_mapping_x);

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
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_set_lenience_x, "set-lenience!", 1, 1, 0,  set_lenience_x);

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
  return SCM_UNSPECIFIED;
}


void 
init_window()
{
  sym_mouse = gh_symbol2scm("mouse");
  scm_protect_object(sym_mouse);
  sym_sloppy = gh_symbol2scm("sloppy");
  scm_protect_object(sym_sloppy);
  sym_none = gh_symbol2scm("none");
  scm_protect_object(sym_none);
#ifndef SCM_MAGIC_SNARFER
#include "window.x"
#endif
}



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
