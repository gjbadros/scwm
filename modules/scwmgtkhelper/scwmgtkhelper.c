/* $Id$
 * Copyright (C) 1998, Maciej Stachowiak
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
#include <X11/Xlib.h>

#include <guile/gh.h>
/* FIXGJB13: guile-1.3 will have this already included */
#include <libguile/dynl.h>

#include <gdk/gdkprivate.h>
#include <signal.h>

#include "scwm-snarf.h"


SCWM_PROC(scwm_gdk_X_fdes, "scwm-gdk-X-fdes", 0, 0, 0,
	  ())
#define FUNC_NAME s_scwm_gdk_X_fdes
{
  return gh_int2scm(XConnectionNumber(gdk_display));
}
#undef FUNC_NAME 

extern void newhandler(int sig);
extern void newsegvhandler(int sig);
extern void Restart(int nonsense);
extern XErrorHandler ScwmErrorHandler(Display *, XErrorEvent *);


SCWM_PROC(restore_scwm_handlers, "restore-scwm-handlers", 0, 0, 0,
	  ())
     /** Restore the scwm behavior for signals and for X errors.
This is automatically called when (app scwm gtk) is loaded, to clean up
after the (gtk gtk) module. */
#define FUNC_NAME s_restore_scwm_handlers
{
  newhandler(SIGINT);
  newhandler(SIGHUP);
  newhandler(SIGQUIT);
  newhandler(SIGTERM);
  /* FIXGJB: I seem to lose the last stack frame in my backtrace if this is
     set... do others not see this? --07/24/98 gjb */
  newsegvhandler(SIGSEGV);

  signal(SIGUSR1, Restart);

  XSetErrorHandler((XErrorHandler) ScwmErrorHandler);
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
