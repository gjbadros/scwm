/* $Id$
 * complex.c
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#include "complex.h"

#include "scwm.h"
#include "util.h"
#include "screen.h"
#include "xmisc.h"
#include "syscompat.h"
#include "focus.h"

/*
 * IsClick(...)
 * Waits Scr.ClickTime, or until it is evident that the user is not
 * clicking, but is moving the cursor
 * This function is derived from code by Robert Nation
 */

/* GJB:FIXME:: a single, slow click with no movement should
   still count as a single click */
Bool 
IsClick(int x, int y, unsigned EndMask, XEvent * d)
{
  int xcurrent, ycurrent, total = 0;
  Time t0;

  xcurrent = x;
  ycurrent = y;
  t0 = lastTimestamp;

  while ((total < Scr.ClickTime) &&
	 (x - xcurrent < 3) && (x - xcurrent > -3) &&
	 (y - ycurrent < 3) && (y - ycurrent > -3) &&
	 ((lastTimestamp - t0) < Scr.ClickTime)) {
    ms_sleep(20);
    total += 20;
    if (XCheckMaskEvent(dpy, EndMask, d)) {
      StashEventTime(d);
      return True;
    }
    if (XCheckMaskEvent(dpy, ButtonMotionMask | PointerMotionMask, d)) {
      xcurrent = d->xmotion.x_root;
      ycurrent = d->xmotion.y_root;
      StashEventTime(d);
    }
  }
  return False;
}



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
