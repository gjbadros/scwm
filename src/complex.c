
/****************************************************************************
 * This module is all new
 * by Rob Nation 
 * Copyright 1993 Robert Nation. No restrictions are placed on this code,
 * as long as the copyright notice is preserved
 ****************************************************************************/

#include <config.h>

#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#include "scwm.h"
#include "menus.h"
#include "misc.h"
#include "screen.h"

/*****************************************************************************
 *
 * Waits Scr.ClickTime, or until it is evident that the user is not
 * clicking, but is moving the cursor
 *
 ****************************************************************************/
/* FIXGJB: a single, slow click with no movement should
   still cound as a single click */
Bool 
IsClick(int x, int y, unsigned EndMask, XEvent * d)
{
  int xcurrent, ycurrent, total = 0;
  Time t0;
  extern Time lastTimestamp;

  xcurrent = x;
  ycurrent = y;
  t0 = lastTimestamp;

  while ((total < Scr.ClickTime) &&
	 (x - xcurrent < 3) && (x - xcurrent > -3) &&
	 (y - ycurrent < 3) && (y - ycurrent > -3) &&
	 ((lastTimestamp - t0) < Scr.ClickTime)) {
    sleep_ms(20);
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


