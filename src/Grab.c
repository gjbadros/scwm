/* $Id$ */
/*
 *      Copyright (C) 1997, Maciej Stachowiak and Greg J. Badros
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.GPL.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <X11/Xlib.h>

#include "Grab.h"

#include "scwm.h"

static int xgrabcount = 0;

void 
XGrabServer_withSemaphore(Display * disp)
{
  if (xgrabcount == 0) {
    XGrabServer(disp);
  }
  ++xgrabcount;
}

void 
XUngrabServer_withSemaphore(Display * disp)
#define FUNC_NAME "XUngrabServer_withSemaphore"
{
  if (--xgrabcount < 0) {	/* should never happen */
    fprintf(stderr,"%s: too many ungrabs!\n",FUNC_NAME);
    xgrabcount = 0;
  }
  if (xgrabcount == 0) {
    XUngrabServer(disp);
  }
}
#undef FUNC_NAME

SCWM_PROC(X_grab_server, "X-grab-server", 0, 0, 0,
	  ())
/** Grab the X server.
This is very risky; you should almost definitely use
`with-grabbed-server' instead.  This must be paired with
X-ungrab-server.  This primitive is undefined at startup
to make it hard to access directly. */
#define FUNC_NAME s_X_grab_server
{
  XGrabServer_withSemaphore(dpy);
  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCWM_PROC(X_ungrab_server, "X-ungrab-server", 0, 0, 0,
	  ())
/** Ungrab the X server.
Using these functions directly is risky; you should almost definitely use
`with-grabbed-server' instead. This primitive is undefined at startup
to make it hard to access directly. */
#define FUNC_NAME s_X_ungrab_server
{
  XUngrabServer_withSemaphore(dpy);
  return SCM_UNDEFINED;
}
#undef FUNC_NAME


void init_Grab()
{
#ifndef SCM_MAGIC_SNARFER
#include "Grab.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
