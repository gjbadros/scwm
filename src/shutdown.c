/* $Id$ 
 * (C) 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <unistd.h>
#include <signal.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>
#include <guile/gh.h>
#include "scwm.h"
#include "screen.h"
#include "virtual.h"
#include "callbacks.h"
#include "shutdown.h"
#include "syscompat.h"
#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif


SCM shutdown_hook;

static inline void
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


/* restart_or_dump == 0 to just close
   > 0 for restart
   < 0 for dump core */
void 
Done(int restart_or_dump, char *command)
{
  call0_hooks(shutdown_hook);

  MoveViewport(0, 0, False);

  /* Close all my pipes */

  Reborder();

  XDeleteProperty(dpy, Scr.Root, XA_SCWMEXEC_LISTENER);


  /* Pretty sure this should be done... */
  XDeleteProperty(dpy, Scr.Root, _XA_MOTIF_WM);
  
  if (restart_or_dump > 0) {
    SaveDesktopState();		/* I wonder why ... */

    /* Really make sure that the connection is closed and cleared! */
    XSelectInput(dpy, Scr.Root, 0);
    XSync(dpy, 0);
    XCloseDisplay(dpy);

    sleep(1);
    ReapChildren();
    run_restart_command(command);
  } else if (restart_or_dump < 0) {
    int i = *((int *)0); /* Force seg fault */
  } else {
    XCloseDisplay(dpy);
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
    XChangeProperty(dpy, t->w, _XA_WM_DESKTOP, _XA_WM_DESKTOP, 32,
		    PropModeReplace, (unsigned char *) data, 1);
  }

  data[0] = (unsigned long) Scr.CurrentDesk;
  XChangeProperty(dpy, Scr.Root, _XA_WM_DESKTOP, _XA_WM_DESKTOP, 32,
		  PropModeReplace, (unsigned char *) data, 1);

  XSync(dpy, 0);
}

SCWM_PROC(restart, "restart", 0, 1, 0,
          (SCM command))
{
  int dummy;
  char *sz;

  if (gh_string_p(command)) {
    sz = gh_scm2newstr(command, &dummy);
  } else if (command == SCM_UNDEFINED) {
    sz = "scwm";
  } else {
    scm_wrong_type_arg(s_restart, 1, command);
  }

  Done(1, sz);  /* 1 == restart */
  FREE(sz); /* Done shouldn't return, but you never know... */
  return SCM_UNSPECIFIED;	
}

SCWM_PROC(scwm_quit, "scwm-quit", 0, 0, 1,
          (SCM args))
{
  if (master_pid != getpid())
    kill(master_pid, SIGTERM);
  Done(0, NULL); /* Done shouldn't return, but you
		    never know... */
  return SCM_UNSPECIFIED;
}


void init_shutdown()
{
  SCWM_DEFINE_HOOK(shutdown_hook, "shutdown-hook");

#ifndef SCM_MAGIC_SNARFER
#include "shutdown.x"
#endif
}
