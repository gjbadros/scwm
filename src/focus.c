/* $Id$
 * focus.c
 * Copyright (C) 1997-1999 Maciej Stachowiak and Greg J. Badros
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
#include <config.h>
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

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif


/*
 * Records the time of the last processed event. Used in XSetInputFocus
 */
Time lastTimestamp = CurrentTime;	/* until Xlib does this for us */


/*
 * Sets the input focus to the indicated window.
 */

void 
SetFocus(Window w, ScwmWindow * Fw, Bool FocusByMouse)
{
  int i = 0;
  if (Fw)
    Fw->ttLastFocussed = time(NULL);

  /* ClickToFocus focus queue manipulation - only performed for
   * Focus-by-mouse type focus events */
  if (FocusByMouse && (Fw && Fw != Scr.Focus && Fw != &Scr.ScwmRoot)) {
    ScwmWindow *pswPrev, *pswNext;

    pswPrev = Fw->prev;
    pswNext = Fw->next;

    if (pswPrev)
      pswPrev->next = pswNext;
    if (pswNext)
      pswNext->prev = pswPrev;

    Fw->next = Scr.ScwmRoot.next;
    if (Scr.ScwmRoot.next)
      Scr.ScwmRoot.next->prev = Fw;
    Scr.ScwmRoot.next = Fw;
    Fw->prev = &Scr.ScwmRoot;
  }
  if (Scr.NumberOfScreens > 1) {
    Window wRoot;
    XQueryPointer(dpy, Scr.Root, &wRoot, &JunkChild,
		  &JunkX, &JunkY, &JunkX, &JunkY, &JunkMask);
    if (wRoot != Scr.Root) {
      if ((Scr.Ungrabbed != NULL) && Scr.Ungrabbed->fClickToFocus) {
	/* Need to grab buttons for focus window */
	XSync(dpy, 0);
	for (i = 0; i < XSERVER_MAX_BUTTONS; i++)
	  if (Scr.buttons2grab & (1 << i)) {
	    XGrabButton(dpy, (i + 1), 0, Scr.Ungrabbed->frame, True,
			ButtonPressMask, GrabModeSync, GrabModeAsync,
			None,
		       XCursorByNumber(XC_hand2));
	    XGrabButton(dpy, (i + 1), LockMask, Scr.Ungrabbed->frame, True,
			ButtonPressMask, GrabModeSync, GrabModeAsync,
			None,
			XCursorByNumber(XC_hand2));
	  }
	Scr.Focus = NULL;
	Scr.Ungrabbed = NULL;
	XSetInputFocus(dpy, Scr.NoFocusWin, RevertToParent, lastTimestamp);
      }
      return;
    }
  }
  if ((Fw != NULL) && (Fw->Desk != Scr.CurrentDesk)) {
    Fw = NULL;
    w = Scr.NoFocusWin;
  }
  if ((Scr.Ungrabbed != NULL) &&
      Scr.Ungrabbed->fClickToFocus && (Scr.Ungrabbed != Fw)) {
    /* need to grab all buttons for window that we are about to
     * unfocus */
    XSync(dpy, 0);
    for (i = 0; i < XSERVER_MAX_BUTTONS; i++)
      if (Scr.buttons2grab & (1 << i))
	XGrabButton(dpy, (i + 1), 0, Scr.Ungrabbed->frame, True,
		    ButtonPressMask, GrabModeSync, GrabModeAsync, None,
		    XCursorByNumber(XC_hand2));
    Scr.Ungrabbed = NULL;
  }
  /* if we do click to focus, remove the grab on mouse events that
   * was made to detect the focus change */
  if (Fw && Fw->fClickToFocus && !Fw->fSloppyFocus) {
    for (i = 0; i < XSERVER_MAX_BUTTONS; i++)
      if (Scr.buttons2grab & (1 << i)) {
	XUngrabButton(dpy, (i + 1), 0, Fw->frame);
	XUngrabButton(dpy, (i + 1), LockMask, Fw->frame);
      }
    Scr.Ungrabbed = Fw;
  }
  if (Fw && Fw->fIconified && Fw->icon_w)
    w = Fw->icon_w;

  if (Fw && Fw->fClickToFocus && Fw->fSloppyFocus) {
    XSetInputFocus(dpy, Scr.NoFocusWin, RevertToParent, lastTimestamp);
    Scr.Focus = NULL;
    Scr.UnknownWinFocused = None;
  } else if ((Fw && Fw->fLenience) ||  /* GJB:FIXME:: split this conditional up */
	     (!(Fw && 
		(Fw->wmhints) && (Fw->wmhints->flags & InputHint) &&
		(Fw->wmhints->input == False)))) {
    /* Window will accept input focus */

    XSetInputFocus(dpy, w, RevertToParent, lastTimestamp);
    Scr.Focus = Fw;
    Scr.UnknownWinFocused = None;
  } else if (Scr.Focus && (Scr.Focus->Desk == Scr.CurrentDesk)) {

    /* Window doesn't want focus. Leave focus alone */
    /* XSetInputFocus (dpy,Scr.Hilite->w , RevertToParent, lastTimestamp); */

  } else {
    XSetInputFocus(dpy, Scr.NoFocusWin, RevertToParent, lastTimestamp);
    Scr.Focus = NULL;
  }


  if (Fw && Fw->fDoesWmTakeFocus) {
    send_clientmessage(dpy, w, XA_WM_TAKE_FOCUS, lastTimestamp);
  }
  XSync(dpy, 0);

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
    SetFocus(FocusOnNextTimeStamp->w, FocusOnNextTimeStamp, 1);
    FocusOnNextTimeStamp = NULL;
  }
  return True;
}



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta */

