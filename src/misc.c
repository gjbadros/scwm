/****************************************************************************
 * This module is all original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/

/****************************************************************************
 *
 * Assorted odds and ends
 *
 **************************************************************************/


#include <config.h>

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <signal.h>

#include "scwm.h"
#include <X11/Xatom.h>
#include "misc.h"
#include "screen.h"
#include "window.h"
#include "events.h"
#include "module-interface.h"
#include "image.h"
#include "focus.h"
#include "syscompat.c"

ScwmWindow *FocusOnNextTimeStamp = NULL;

char NoName[] = "Untitled";	/* name if no name in XA_WM_NAME */
char NoClass[] = "NoClass";	/* Class if no res_class in class hints */
char NoResource[] = "NoResource";	/* Class if no res_name in class hints */


/**************************************************************************
 * 
 * Releases dynamically allocated space used to store window/icon names
 *
 **************************************************************************/
void 
free_window_names(ScwmWindow * tmp, Bool nukename, Bool nukeicon)
{
  if (!tmp)
    return;

  if (nukename && nukeicon) {
    if (tmp->name == tmp->icon_name) {
      if (tmp->name != NoName && tmp->name != NULL)
	XFree(tmp->name);
      tmp->name = NULL;
      tmp->icon_name = NULL;
    } else {
      if (tmp->name != NoName && tmp->name != NULL)
	XFree(tmp->name);
      tmp->name = NULL;
      if (tmp->icon_name != NoName && tmp->icon_name != NULL)
	XFree(tmp->icon_name);
      tmp->icon_name = NULL;
    }
  } else if (nukename) {
    if (tmp->name != tmp->icon_name
	&& tmp->name != NoName
	&& tmp->name != NULL)
      XFree(tmp->name);
    tmp->name = NULL;
  } else {			/* if (nukeicon) */
    if (tmp->icon_name != tmp->name
	&& tmp->icon_name != NoName
	&& tmp->icon_name != NULL)
      XFree(tmp->icon_name);
    tmp->icon_name = NULL;
  }

  return;
}


/**************************************************************************
 *
 * Removes expose events for a specific window from the queue 
 *
 *************************************************************************/
int 
flush_expose(Window w)
{
  XEvent dummy;
  int i = 0;

  while (XCheckTypedWindowEvent(dpy, w, Expose, &dummy))
    i++;
  return i;
}

/* CoerceEnterNotifyOnCurrentWindow()
 * Pretends to get a HandleEnterNotify on the
 * window that the pointer currently is in so that
 * the focus gets set correctly from the beginning
 * Note that this presently only works if the current
 * window is not click_to_focus;  I think that
 * that behaviour is correct and desirable. --11/08/97 gjb */
void
CoerceEnterNotifyOnCurrentWindow()
{
  extern ScwmWindow *swCurrent; /* from events.c */
  Window child, root;
  int root_x, root_y;
  int win_x, win_y;
  Bool f = XQueryPointer(dpy, Scr.Root, &root,
			 &child, &root_x, &root_y, &win_x, &win_y, &JunkMask);
  if (f && child != None) {
    Event.xany.window = child;
    swCurrent = SwFromWindow(dpy,child);
    HandleEnterNotify();
    swCurrent = None;
  }
}


Bool
FXWindowAccessible(Display *dpy, Window w)
{
  /* XGetGeometry returns true if the call was successful */
  return XGetGeometry(dpy, w, &JunkRoot, &JunkX, &JunkY,
		      &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth);
}

Bool
XGetGeometryPositionOnly(Display *dpy, Window w, int *x_ret, int *y_ret)
{
  /* XGetGeometry returns true if the call was successful */
  return XGetGeometry(dpy, w, &JunkRoot, x_ret, y_ret,
		      &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth);
}

/* For some reason, when adding a window, we need to ask the X server
   for a window geometry w/o caring about the results
   Note that this happens to be identical to FXWindowAccessible,
   but their names encode their separate uses (and making these
   inline functions would avoid the extra space overhead)
   --03/29/98 gjb */
Bool
XGetGeometryCacheIt(Display *dpy, Window w)
{
  return XGetGeometry(dpy, w, &JunkRoot, &JunkX, &JunkY,
                      &JunkWidth, &JunkHeight, &JunkBW, &JunkDepth);
}


/***********************************************************************
 *
 *  Procedure:
 *	RestoreWithdrawnLocation
 * 
 *  Puts windows back where they were before Scwm took over 
 *
 ************************************************************************/
void 
RestoreWithdrawnLocation(ScwmWindow * tmp, Bool fRestart)
{
  int a, b, w2, h2;
  unsigned int bw, mask;
  XWindowChanges xwc;

  if (!tmp)
    return;

  if (XGetGeometryPositionOnly(dpy, tmp->w, &xwc.x, &xwc.y )) {
    XTranslateCoordinates(dpy, tmp->frame, Scr.Root, xwc.x, xwc.y,
			  &a, &b, &JunkChild);
    xwc.x = a + tmp->xdiff;
    xwc.y = b + tmp->ydiff;
    xwc.border_width = tmp->old_bw;
    mask = (CWX | CWY | CWBorderWidth);

    /* We can not assume that the window is currently on the screen.
     * Although this is normally the case, it is not always true.  The
     * most common example is when the user does something in an
     * application which will, after some amount of computational delay,
     * cause the window to be unmapped, but then switches screens before
     * this happens.  The XTranslateCoordinates call above will set the
     * window coordinates to either be larger than the screen, or negative.
     * This will result in the window being placed in odd, or even
     * unviewable locations when the window is remapped.  The followin code
     * forces the "relative" location to be within the bounds of the display.
     *
     * gpw -- 11/11/93
     *
     * Unfortunately, this does horrendous things during re-starts, 
     * hence the "if(!fRestart)" clause (RN) 
     *
     * Also, fixed so that it only does this stuff if a window is more than
     * half off the screen. (RN)
     */

    if (!fRestart) {
      /* Don't mess with it if its partially on the screen now */
      if ((tmp->frame_x < 0) || (tmp->frame_y < 0) ||
	  (tmp->frame_x >= Scr.MyDisplayWidth) ||
	  (tmp->frame_y >= Scr.MyDisplayHeight)) {
	w2 = (tmp->frame_width >> 1);
	h2 = (tmp->frame_height >> 1);
	if ((xwc.x < -w2) || (xwc.x > (Scr.MyDisplayWidth - w2))) {
	  xwc.x = xwc.x % Scr.MyDisplayWidth;
	  if (xwc.x < -w2)
	    xwc.x += Scr.MyDisplayWidth;
	}
	if ((xwc.y < -h2) || (xwc.y > (Scr.MyDisplayHeight - h2))) {
	  xwc.y = xwc.y % Scr.MyDisplayHeight;
	  if (xwc.y < -h2)
	    xwc.y += Scr.MyDisplayHeight;
	}
      }
    }
    XReparentWindow(dpy, tmp->w, Scr.Root, xwc.x, xwc.y);

    if (tmp->fIconified && !(tmp->fSuppressIcon)) {
      if (tmp->icon_w)
	XUnmapWindow(dpy, tmp->icon_w);
      if (tmp->icon_pixmap_w)
	XUnmapWindow(dpy, tmp->icon_pixmap_w);
    }
    XConfigureWindow(dpy, tmp->w, mask, &xwc);
    if (!fRestart)
      XSync(dpy, 0);
  }
}


/****************************************************************************
 *
 * Records the time of the last processed event. Used in XSetInputFocus
 *
 ****************************************************************************/
Time lastTimestamp = CurrentTime;	/* until Xlib does this for us */

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

/*****************************************************************************
 *
 * Grab the pointer and keyboard
 *
 ****************************************************************************/
Bool 
GrabEm(enum cursor cursor)
{
  int i = 0, val = 0;
  unsigned int mask;

  XSync(dpy, 0);
  /* move the keyboard focus prior to grabbing the pointer to
   * eliminate the enterNotify and exitNotify events that go
   * to the windows */
  if (Scr.PreviousFocus == NULL)
    Scr.PreviousFocus = Scr.Focus;
  SetFocus(Scr.NoFocusWin, NULL, 0);
  mask = ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask
    | EnterWindowMask | LeaveWindowMask;
  while ((i < 1000) && (val = XGrabPointer(dpy, Scr.Root, True, mask,
				     GrabModeAsync, GrabModeAsync, Scr.Root,
				    Scr.ScwmCursors[cursor], CurrentTime) !=
			GrabSuccess)) {
    i++;
    /* If you go too fast, other windows may not get a change to release
     * any grab that they have. */
    usleep(1);
  }

  /* If we fall out of the loop without grabbing the pointer, its
     time to give up */
  XSync(dpy, 0);

  return (val == GrabSuccess);
}


/*****************************************************************************
 *
 * UnGrab the pointer and keyboard
 *
 ****************************************************************************/
void 
UngrabEm()
{
  Window w;

  XSync(dpy, 0);
  XUngrabPointer(dpy, CurrentTime);

  if (Scr.PreviousFocus != NULL) {
    w = Scr.PreviousFocus->w;

    /* if the window still exists, focus on it */
    if (w) {
      SetFocus(w, Scr.PreviousFocus, 0);
    }
    Scr.PreviousFocus = NULL;
  }
  XSync(dpy, 0);
}



/****************************************************************************
 *
 * Keeps the "StaysOnTop" windows on the top of the pile.
 * This is achieved by clearing a flag for OnTop windows here, and waiting
 * for a visibility notify on the windows. Exeption: OnTop windows which are
 * obscured by other OnTop windows, which need to be raised here.
 *
 ****************************************************************************/
void 
KeepOnTop()
{
  ScwmWindow *t;

  /* flag that on-top windows should be re-raised */
  for (t = Scr.ScwmRoot.next; t != NULL; t = t->next) {
    if (t->fOnTop && !t->fVisible) {
      RaiseWindow(t);
      t->fRaised = False;
    } else
      t->fRaised = True;
  }
}



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
