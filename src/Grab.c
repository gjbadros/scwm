/* $Id$ */
/*
 * Copyright (C) 1997, 1998, 1999, 2000 Maciej Stachowiak and Greg J. Badros
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
#include "scwmconfig.h"
#endif

#include <stdio.h>
#include <X11/Xlib.h>

#include "Grab.h"

#include "scwm.h"

static int xgrabcount = 0;

int CServerGrabs() { return xgrabcount; }

void 
XGrabServer_withSemaphore(Display * disp)
{
#define FUNC_NAME "XGrabServer_withSemaphore"
  /* should happen before we grab the server so that X-server-grabs
     conservatively reports that the Server is grabbed (not really an
     issue now, but could be with more asynch events --04/11/99 gjb) */
  ++xgrabcount; 

  if (xgrabcount == 1) {
    /* we just incremented from 0, so do the actual grab */
    XGrabServer(disp);
    DBUG((DBG,"XGrabServer_withSemaphore","Grabbed!"));
  }
}
#undef FUNC_NAME

void 
XUngrabServer_withSemaphore(Display * disp)
{
#define FUNC_NAME "XUngrabServer_withSemaphore"
  if (xgrabcount == 1) {
    DBUG((DBG,"XGrabServer_withSemaphore","ungrabbed!"));
    XUngrabServer(disp);
  }
  --xgrabcount;
  if (xgrabcount < 0) {	/* should never happen */
    scwm_msg(ERR,FUNC_NAME,"More ungrabs than grabs!");
    xgrabcount = 0;
  }
}
#undef FUNC_NAME

SCWM_PROC(X_grab_server, "X-grab-server", 0, 0, 0,
	  (),
"Grab the X server.\n\
This is very risky; you should almost definitely use\n\
`with-grabbed-server' instead.  This must be paired with\n\
X-ungrab-server.  This primitive is undefined at startup\n\
to make it hard to access directly. See also `X-server-grabs'")
#define FUNC_NAME s_X_grab_server
{
  XGrabServer_withSemaphore(dpy);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(X_ungrab_server, "X-ungrab-server", 0, 0, 0,
	  (),
"Ungrab the X server.\n\
Using `X-grab-server' and `X-ungrab-server' directly is risky; you \n\
should almost definitely use `with-grabbed-server' instead. This \n\
primitive is undefined at startup to make it hard to access directly. \n\
See also `X-server-grabs'")
#define FUNC_NAME s_X_ungrab_server
{
  XUngrabServer_withSemaphore(dpy);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(X_server_grabs, "X-server-grabs", 0, 0, 0,
	  (),
"Return the number of nested server grabs.\n\
Nonzero means the server is currently grabbed. \n\
See also `with-grabbed-server', `X-grab-server' and `X-ungrab-server'")
#define FUNC_NAME s_X_server_grabs
{
  return gh_int2scm(xgrabcount);
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
/* vim:ts=8:sw=2:sta 
 */

