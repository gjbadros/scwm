/* $Id$
 * focus.c
 * Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
 */

/****************************************************************************
 * This module is derived from all original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/

/***********************************************************************
 *
 * scwm focus-setting code
 *
 ***********************************************************************/

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <time.h>

#include "scwm.h"
#include "ICCCM.h"
#include "screen.h"
#include "focus.h"
#include "cursor.h"
#include "xmisc.h"
#include "callbacks.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif


/*
 * Records the time of the last processed event. Used in XSetInputFocus
 */
Time lastTimestamp = CurrentTime;	/* until Xlib does this for us */

SCWM_HOOK(window_focus_lost_hook,"window-focus-lost-hook", 1,
"This hook is invoked whenever the focus is lost on a window.\n\
It is called with one argument, the window object of the window\n\
that just lost the keyboard focus. See also `window-focus-change-hook'.");


/* Invoke the window_focus_lost_hook iff Scr.Focus is a SchemeWindow 
   and it is not already w; also require Scr.PreviousFocus to be null
   (since otherwise we are doing a GrabEm)*/
static SCWM_INLINE void
call_lost_focus_hook(ScwmWindow *psw)
{
  if (Scr.Focus && Scr.Focus != psw && NULL == Scr.PreviousFocus) {
    scwm_run_hook1(window_focus_lost_hook, SCM_FROM_PSW(Scr.Focus));
  }
}
  

Bool
FFocussableWin(ScwmWindow *psw)
{
  if (psw && psw->fClickToFocus && psw->fSloppyFocus)
    return False;
  if ((psw && psw->fLenience) || 
      (!(psw && 
         (psw->wmhints) && (psw->wmhints->flags & InputHint) &&
         (psw->wmhints->input == False)))) {
    return True;
  }
  return False;
}

SCM_DEFINE(focussable_window_p,"focussable-window?",0,1,0,
          (SCM win),
"Return #t iff WIN may receive the keyboard focus.\n\
This will return #f, e.g., if WIN's focus style is 'none, or\n\
its X11 hints do not permit it to receive the focus.")
#define FUNC_NAME s_focussable_window_p
{
  ScwmWindow *psw;
  VALIDATE_ARG_WIN_COPY_USE_CONTEXT(1,win,psw);
  return gh_bool2scm(FFocussableWin(psw));
}
#undef FUNC_NAME

/*
 * Sets the input focus to the indicated window.
 */
void 
SetFocus(Window w, ScwmWindow * psw, Bool ARG_UNUSED(FocusByMouse))
{
  int i = 0;
  DBUG_EVENT((DBG,"SetFocus", "Entered (give `%s' the focus)",psw?psw->name:"<none>"));
  if (Scr.Focus == psw) {
    DBUG_EVENT((DBG,"SetFocus", "return--%s already had focus",psw?psw->name:"win"));
    return;
  }
  if (psw) {
    psw->ttLastFocussed = time(NULL);
    psw->timeLastFocussed = lastTimestamp;
  }

  if (Scr.NumberOfScreens > 1) {
    Window wRoot = WXGetPointerChild(Scr.Root);
    if (wRoot != Scr.Root) {
      if ((Scr.Ungrabbed != NULL) && Scr.Ungrabbed->fClickToFocus) {
	/* Need to grab buttons for focus window */
	XSync(dpy, False);
	for (i = 0; i < XSERVER_MAX_BUTTONS; i++)
	  if (Scr.buttons2grab & (1 << i)) {
            GrabButtonWithModifiersMaskXcPm(i+1,0,Scr.Ungrabbed->frame,
                                            ButtonPressMask,
                                            XCURSOR_SET_FOCUS,
                                            GrabModeSync);
	  }
        call_lost_focus_hook(NULL);
	Scr.Focus = NULL;
	Scr.Ungrabbed = NULL;
	XSetInputFocus(dpy, Scr.NoFocusWin, RevertToPointerRoot, lastTimestamp);
      }
      return;
    }
  }
  if ((psw != NULL) && (psw->Desk != Scr.CurrentDesk)) {
    psw = NULL;
    w = Scr.NoFocusWin;
  }
  if ((Scr.Ungrabbed != NULL) &&
      Scr.Ungrabbed->fClickToFocus && (Scr.Ungrabbed != psw)) {
    /* need to grab all buttons for window that we are about to
     * unfocus */
    XSync(dpy, False);
    for (i = 0; i < XSERVER_MAX_BUTTONS; i++) {
      if (Scr.buttons2grab & (1 << i)) {
        /* GJB:FIXME:: segfaulted on a NULL Scr.Ungrabbed 
           once below, even with above guard so duplicate guard
           here in case something is running at the XSync call
           that changes Scr.Ungrabbed... */
        if (Scr.Ungrabbed && Scr.Ungrabbed != psw)
          GrabButtonWithModifiersMaskXcPm(i+1,0,Scr.Ungrabbed->frame,
                                          ButtonPressMask,
                                          XCURSOR_SET_FOCUS,
                                          GrabModeSync);
      }
    }
    Scr.Ungrabbed = NULL;
  }
  /* if we do click to focus, remove the grab on mouse events that
   * was made to detect the focus change */
  if (psw && psw->fClickToFocus && !psw->fSloppyFocus) {
    for (i = 0; i < XSERVER_MAX_BUTTONS; i++) {
      if (Scr.buttons2grab & (1 << i)) {
        UngrabButtonWithModifiersWin(i+1,0,psw->frame);
      }
    }
    Scr.Ungrabbed = psw;
  }
  if (psw && psw->fIconified && psw->icon_w)
    w = psw->icon_w;

  if (psw && !FFocussableWin(psw)) {
    if ( Scr.Focus != NULL && Scr.Focus->fSloppyFocus ) {
      /* don't unfocus a sloppily focused window if an unfocusable
	 window is trying to get the focus */
    } else {
      call_lost_focus_hook(NULL);
      XSetInputFocus(dpy, Scr.NoFocusWin, RevertToPointerRoot, lastTimestamp);
      Scr.Focus = NULL;
    }
  } else {
    call_lost_focus_hook(psw);
    XSetInputFocus(dpy, w, RevertToPointerRoot, lastTimestamp);
    Scr.Focus = psw;
#if 0  /* GJB:FIXME:: what are all these cases? --09/17/99 gjb */
  } else if (Scr.Focus && (Scr.Focus->Desk == Scr.CurrentDesk)) {
    /* Window doesn't want focus. Leave focus alone */
    /* XSetInputFocus (dpy,Scr.Hilite->w , RevertToPointerRoot, lastTimestamp); */
  } else {
    call_lost_focus_hook(NULL);
    XSetInputFocus(dpy, Scr.NoFocusWin, RevertToPointerRoot, lastTimestamp);
    Scr.Focus = NULL;
#endif
  }

  if (psw && psw->fDoesWmTakeFocus) {
    DBUG_EVENT((DBG,"SetFocus","send_clientmessage XA_WM_TAKE_FOCUS"));
    send_clientmessage(dpy, w, XA_WM_TAKE_FOCUS, lastTimestamp);
  }
  XSync(dpy, False);
}


void
Unfocus()
{
  SetFocus(Scr.NoFocusWin,NULL,False);
}

Bool 
StashEventTime(XEvent * ev)
{
  Time NewTimestamp = CurrentTime;

  switch (ev->type) {
  case KeyPress:
  case KeyRelease:
    NewTimestamp = ev->xkey.time;
    break;
  case ButtonPress:
  case ButtonRelease:
    NewTimestamp = ev->xbutton.time;
    break;
  case MotionNotify:
    NewTimestamp = ev->xmotion.time;
    break;
  case EnterNotify:
  case LeaveNotify:
    NewTimestamp = ev->xcrossing.time;
    break;
  case PropertyNotify:
    NewTimestamp = ev->xproperty.time;
    break;
  case SelectionClear:
    NewTimestamp = ev->xselectionclear.time;
    break;
  case SelectionRequest:
    NewTimestamp = ev->xselectionrequest.time;
    break;
  case SelectionNotify:
    NewTimestamp = ev->xselection.time;
    break;
  default:
    return False;
  }
  /* Only update is the new timestamp is later than the old one, or
   * if the new one is from a time at least 30 seconds earlier than the
   * old one (in which case the system clock may have changed) */
  if ((NewTimestamp > lastTimestamp) || ((lastTimestamp - NewTimestamp) > 30000))
    lastTimestamp = NewTimestamp;
  if (FocusOnNextTimeStamp) {
    SetFocus(FocusOnNextTimeStamp->w, FocusOnNextTimeStamp, True);
    FocusOnNextTimeStamp = NULL;
  }
  return True;
}


void 
init_focus()
{
#ifndef SCM_MAGIC_SNARFER
#include "focus.x"
#endif
}



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

