/* $Id$
 * Copyright (C) 1998, 1999, 2000 Maciej Stachowiak and Greg J. Badros
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

#include <guile/gh.h>
#include "guile-compat.h"

#include <gdk/gdkprivate.h>
#include <signal.h>

#include "scwm-snarf.h"
#include "scwm_msg.h"

SCWM_PROC(scwm_gdk_X_fdes, "scwm-gdk-X-fdes", 0, 0, 0,
	  (),
"Return the integer connection number of the gdk_display.\n\
Returns -1 if gdk_display is not initialized.")
#define FUNC_NAME s_scwm_gdk_X_fdes
{
  return gh_int2scm(gdk_display?
                    XConnectionNumber(gdk_display):
                    -1);
}
#undef FUNC_NAME 

extern void newhandler(int sig);
extern void newhandler_doreset(int sig);
extern void newsegvhandler(int sig);
extern void Restart(int nonsense);
extern XErrorHandler ScwmErrorHandler(Display *, XErrorEvent *);
extern gint gdk_error_warnings;

/* Must chain some gdk specific error handling with
   the basic ScwmErrorHandler;  discovered this was
   necessary because guile-gtk images were incorrectly
   trying to use the X shared memory extension when
   displaying on a remote host because the gdk_error_code
   was not getting set to indicate an error in XShmAttach() */
static XErrorHandler 
ScwmGdkErrorHandler(Display * dpy, XErrorEvent *error)
{
  /* from gdk_x_error function in gdk.c of gtk+-1.2.x */
  if (error->error_code)
    gdk_error_code = error->error_code;
  if (gdk_error_warnings)
    return ScwmErrorHandler(dpy,error);
  else
    return 0;
}

/* extern void DeadPipe(int nonsense); */

SCWM_PROC(restore_scwm_handlers, "restore-scwm-handlers", 0, 0, 0,
	  (),
"Restore the scwm behavior for signals and for X errors.\n\
This is automatically called when (app scwm gtk) is loaded, to clean up\n\
after the (gtk gtk) module.")
#define FUNC_NAME s_restore_scwm_handlers
{
  extern Bool fHandleSegv;
  /* BEWARE:
     this code is coupled with the code that does the same 
     thing in scwm.c */
#ifdef SCWM_RESET_ON_SIGINT
  newhandler_doreset(SIGINT);
#else
  newhandler(SIGINT);
#endif
  newhandler_doreset(SIGHUP);
  newhandler_doreset(SIGFPE);
/*  signal(SIGPIPE,DeadPipe); */
  newhandler(SIGQUIT);
  newhandler(SIGTERM);
  if (fHandleSegv)
    newsegvhandler(SIGSEGV);

  signal(SIGUSR1, Restart);

  XSetErrorHandler((XErrorHandler) ScwmGdkErrorHandler);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME 



static
void
init_scwmgtkhelper()
{
#ifndef SCM_MAGIC_SNARFER
 #include "scwmgtkhelper.x"
#endif
}

void scm_init_app_scwm_scwmgtkhelper_module()
{
  scm_register_module_xxx("app scwm scwmgtkhelper", init_scwmgtkhelper);
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
