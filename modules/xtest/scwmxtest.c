/* $Id$
 * Copyright (C) 1999, 2000 Greg J. Badros
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

#include <X11/Xlib.h>
#include <X11/extensions/XTest.h>

#include <guile/gh.h>
#include "guile-compat.h"

#include "scwm.h"
#include "screen.h"

extern int XTestEventBase, XTestErrorBase, XTestMajorP, XTestMinorP;
extern Bool XTestSupported;

SCWM_PROC(xtest_supported_p,"xtest-supported?",0,0,0,
	  (),
"Returns #t iff the XTest extension is supported on this server.")
#define FUNC_NAME s_xtest_supported_p
{
  return gh_bool2scm(XTestSupported);
}
#undef FUNC_NAME 


SCWM_PROC(xtest_fake_button_event,"xtest-fake-button-event",2,1,0,
	  (SCM button, SCM is_press_p, SCM ms_delay),
"Fake an X event of button number BUTTON after a delay of MS-DELAY.\n\
The event is a mouse press if IS-PRESS? is #t, or a release otherwise. If\n\
MS-DELAY is ommitted or is not a number, no delay is used")
#define FUNC_NAME s_xtest_fake_button_event
{
  int but;
  Bool fPress;
  long delay;

  /* we do this to permit an extra value of #t/#f from get-key-event/get-mouse-event
     to not change the semantics here */
  if (!gh_number_p(ms_delay)) ms_delay = SCM_BOOL_F;

  VALIDATE_ARG_INT_COPY(1,button,but);
  VALIDATE_ARG_BOOL_COPY(2,is_press_p,fPress);
  VALIDATE_ARG_INT_COPY_USE_DEF(3,ms_delay,delay,0);

  XTestFakeButtonEvent(dpy, but, fPress, delay? delay: CurrentTime);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME 

SCWM_PROC(xtest_fake_key_event,"xtest-fake-key-event",2,1,0,
	  (SCM keycode, SCM is_press_p, SCM ms_delay),
"Fake an X event of key KEYCODE after a delay of MS-DELAY.\n\
The event is a key press if IS-PRESS? is #t, or a release otherwise. If\n\
MS-DELAY is ommitted or is not a number, no delay is used.")
#define FUNC_NAME s_xtest_fake_key_event
{
  int key;
  Bool fPress;
  long delay;
  /* we do this to permit an extra value of #t/#f from get-key-event/get-mouse-event
     to not change the semantics here */
  if (!gh_number_p(ms_delay)) ms_delay = SCM_BOOL_F;

  VALIDATE_ARG_INT_COPY(1,keycode,key);
  VALIDATE_ARG_BOOL_COPY(2,is_press_p,fPress);
  VALIDATE_ARG_INT_COPY_USE_DEF(3,ms_delay,delay,0);

  XTestFakeKeyEvent(dpy, key, fPress, delay? delay: CurrentTime);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME 


SCWM_PROC(xtest_fake_motion_event,"xtest-fake-motion-event",2,2,0,
	  (SCM x, SCM y, SCM screen, SCM ms_delay),
"Fake an X motion event to X,Y after a delay of MS-DELAY on SCREEN.\n\
If SCREEN is ommitted, 0 is used.\n\
If MS-DELAY is ommitted or is not a number, no delay is used.")
#define FUNC_NAME s_xtest_fake_motion_event
{
  int xpos, ypos;
  int scr;
  long delay;

  /* we do this to permit an extra value of #t/#f from get-key-event/get-mouse-event
     to not change the semantics here */
  if (!gh_number_p(ms_delay)) ms_delay = SCM_BOOL_F;

  VALIDATE_ARG_INT_COPY(1,x,xpos);
  VALIDATE_ARG_INT_COPY(2,y,ypos);
  VALIDATE_ARG_INT_COPY_USE_DEF(3,screen,scr,0);
  VALIDATE_ARG_INT_COPY_USE_DEF(4,ms_delay,delay,0);

  XTestFakeMotionEvent(dpy, scr, xpos, ypos, delay? delay: CurrentTime);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME 


SCWM_PROC(xtest_fake_relative_motion_event,"xtest-fake-relative-motion-event",2,1,0,
	  (SCM dx, SCM dy, SCM ms_delay),
"Fake an X motion relative event of a move DX, DY after a delay of MS-DELAY.\n\
If MS-DELAY is ommitted or is #f or 0, no delay is used")
#define FUNC_NAME s_xtest_fake_relative_motion_event
{
  int dxpos, dypos;
  long delay;

  /* we do this to permit an extra value of #t/#f from get-key-event/get-mouse-event
     to not change the semantics here */
  if (!gh_number_p(ms_delay)) ms_delay = SCM_BOOL_F;

  VALIDATE_ARG_INT_COPY(1,dx,dxpos);
  VALIDATE_ARG_INT_COPY(2,dy,dypos);
  VALIDATE_ARG_INT_COPY_USE_DEF(3,ms_delay,delay,0);

  XTestFakeRelativeMotionEvent(dpy, dxpos, dypos, delay? delay: CurrentTime);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME 


static void
init_scwmxtest()
{
#ifndef SCM_MAGIC_SNARFER
 #include "scwmxtest.x"
#endif
}

void scm_init_app_scwm_scwmxtest_module()
{
  scm_register_module_xxx("app scwm scwmxtest", init_scwmxtest);
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
