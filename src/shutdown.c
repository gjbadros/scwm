/* $Id$ 
 * Copyright (C) 1998-1999 Maciej Stachowiak and Greg J. Badros
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <unistd.h>
#include <signal.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>

#include <guile/gh.h>
#include "guile-compat.h"

#include "shutdown.h"

#include "scwm.h"
#include "screen.h"
#include "virtual.h"
#include "callbacks.h"
#include "syscompat.h"

#ifdef HAVE_LIBSM_LIBICE
#include "session-manager.h"
#endif

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

SCWM_HOOK(shutdown_hook, "shutdown-hook",1,
"The procedures in shutdown-hook are before scwm quits or restarts.
A single boolean argument is passed that is #t iff scwm is restarting.");

SCWM_HOOK(startup_hook, "startup-hook",0,
"The procedures in startup-hook are called with no arguments after scwm
has processed the scwmrc and captured all application windows, and
right before it enters the main event loop.  Note that during
processing of the .scwmrc startup file, windows have not already been
captured, so the window-list (as reported by `list-all-windows') is
empty.  To provide behviour conditioned on a property of an existing
window, this hook should be used instead.");


static SCWM_INLINE void
run_restart_command(char *command) {
  if (STREQ(command,"scwm")) {
      char *my_argv[20];
      int i, done, j;

      i = 0;
      j = 0;
      done = 0;
      while ((g_argv[j] != NULL) && (i < 18)) {
	if (!STREQ(g_argv[j], "-s")) {
	  my_argv[i] = g_argv[j];
	  i++;
	  j++;
	} else
	  j++;
      }
      my_argv[i++] = "-s";
      
      while (i < 20)
	my_argv[i++] = NULL;
      execvp(command, my_argv);
  } else {
    execl("/bin/sh", "/bin/sh", "-c", command, NULL);
  }
  scwm_msg(ERR, "Done", "Call of '%s' failed!!!!", command);
  execvp(g_argv[0], g_argv);	/* that _should_ work */
  scwm_msg(ERR, "Done", "Call of '%s' failed!!!!", g_argv[0]);
}


extern Window w_for_scwmexec_response;

/* restart_or_dump == 0 to just close
   > 0 for restart
   < 0 for dump core */
void 
Done(int restart_or_dump, char *command)
{
  /* need to be sure we've opened the display -- could
     seg fault during startup */
  if (dpy) {
    call1_hooks(shutdown_hook,SCM_BOOL_FromBool(restart_or_dump));

    Reborder((restart_or_dump > 0));
    XSetInputFocus(dpy, PointerRoot, RevertToPointerRoot, CurrentTime);

    XDeleteProperty(dpy, Scr.Root, XA_SCWMEXEC_LISTENER);

    /* Pretty sure this should be done... */
    XDeleteProperty(dpy, Scr.Root, XA_MOTIF_WM);

    /* GJB:FIXME:: this used to be done only on restart -- why? --07/31/98 gjb */
    SaveDesktopState();

    if (None != w_for_scwmexec_response) {
      /* give a response to libscwmexec in case we were in the middle of
         a scwmexec when we quit or segfaulted */
      XChangeProperty(dpy, w_for_scwmexec_response,
                      XA_SCWMEXEC_OUTPUT, XA_STRING,
                      8, PropModeReplace, "", 0);
      XChangeProperty(dpy, w_for_scwmexec_response,
                      XA_SCWMEXEC_ERROR, XA_STRING,
                      8, PropModeReplace, "", 0);
      XChangeProperty(dpy, w_for_scwmexec_response,
                      XA_SCWMEXEC_REPLY, XA_STRING,
                      8, PropModeReplace, "", 0);
    }

#ifdef HAVE_LIBSM_LIBICE
    doneSM(restart_or_dump < 0);
#endif

    /* Really make sure that the connection is closed and cleared! */
    XUngrabServer(dpy);
    XDefineCursor(dpy, Scr.Root, None);
    XSelectInput(dpy, Scr.Root, 0);
    XSync(dpy, False);

    if (restart_or_dump > 0 && STREQ(command,"scwm")) {
      /* we're restarting Scwm -- must do this before
         we close the display */
      XChangeProperty(dpy, Scr.Root, 
                      XA_SCWM_RESTARTING, XA_STRING,
                      8, PropModeReplace, (unsigned char *) "scwm-restart", 13);
    }

    XCloseDisplay(dpy);
  }

  if (restart_or_dump < 0) {
    reset_signal_handler(SIGSEGV);
    /* force seg fault -- need to use as an argument to a function
       to be sure it doesn't get optimized away, so invoke a function
       we're sure exists -- this same function! --07/23/98 gjb */
    Done(*((int *)0),NULL); /* Force seg fault */
    return; /* Never executed */
  } else if (restart_or_dump > 0) {
    sleep(1);
    ReapChildren();
    run_restart_command(command);
  } else {
    exit(0);
  }
}


/****************************************************************************
 *
 * Save Desktop State
 *
 ****************************************************************************/
void 
SaveDesktopState()
{
  ScwmWindow *t;
  unsigned long data[1];

  for (t = Scr.ScwmRoot.next; t != NULL; t = t->next) {
    data[0] = (unsigned long) t->Desk;
    XChangeProperty(dpy, t->w, XA_WM_DESKTOP, XA_WM_DESKTOP, 32,
		    PropModeReplace, (unsigned char *) data, 1);
  }

  data[0] = (unsigned long) Scr.CurrentDesk;
  XChangeProperty(dpy, Scr.Root, XA_WM_DESKTOP, XA_WM_DESKTOP, 32,
		  PropModeReplace, (unsigned char *) data, 1);

  XSync(dpy, False);
}

SCWM_PROC(restart, "restart", 0, 1, 0,
          (SCM command),
"Restart the window manager. 
If COMMAND is specified, use that, as the new window manager to
run. If COMMAND is not specified or is exactly equal to \"scwm\",
restart scwm with the same command-line arguments as given previously.")
#define FUNC_NAME s_restart
{
  char *sz;

  VALIDATE_ARG_STR_NEWCOPY_USE_NULL(1,command,sz);
  if (!sz) sz = strdup("scwm");

  Done(1, sz);  /* 1 == restart */

  /* should not ever get here */
  FREE(sz);
  return SCM_UNSPECIFIED;	
}
#undef FUNC_NAME

/* GJB:FIXME:MS: what is args for, and why does this have it */
SCWM_PROC(scwm_quit, "scwm-quit", 0, 0, 1,
          (SCM ARG_IGNORE(args)),
"Exit scwm cleanly. `quit' is redefined as this within scwm.
ARGS is ignored")
#define FUNC_NAME s_scwm_quit
{
  if (master_pid != getpid())
    kill(master_pid, SIGTERM);
  Done(0, NULL); /* Done shouldn't return, but you
		    never know... */
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void run_startup_hook()
{
  call0_hooks(startup_hook);
}

void init_shutdown()
{
#ifndef SCM_MAGIC_SNARFER
#include "shutdown.x"
#endif
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

