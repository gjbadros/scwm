/* $Id$
 * module-interface.c
 * (C) 1998 Maciej Stachowiak and Greg J. Badros
 * 
 * Replaces the module stuff from fvwm2 with hooks to call
 * scheme code which can then talk to a module via the old mechanisms
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/X.h>
#include <guile/gh.h>
#include <stdarg.h>
#include "scwm.h"
#include "window.h"
#include "callbacks.h"
#include "scwm-constraints.h"
#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif


SCM broadcast_hook;
SCM broadcast_config_hook;
SCM broadcast_name_hook;

void
Broadcast(unsigned long event_type, unsigned long num_datum,
	  unsigned long data1, unsigned long data2, unsigned long data3, 
	  unsigned long data4, unsigned long data5, unsigned long data6,
	  unsigned long data7)
{
  apply_hooks (broadcast_hook, 
	       gh_list(gh_ulong2scm(event_type), 
		       gh_ulong2scm(num_datum),
		       gh_ulong2scm(data1),
		       gh_ulong2scm(data2),
		       gh_ulong2scm(data3),
		       gh_ulong2scm(data4),
		       gh_ulong2scm(data5),
		       gh_ulong2scm(data6),
		       gh_ulong2scm(data7),
		       SCM_UNDEFINED));
}


void
BroadcastConfig(unsigned long event_type, ScwmWindow *psw)
{
  apply_hooks (broadcast_config_hook, 
	       gh_list(gh_ulong2scm(event_type), 
		       psw->schwin,
		       SCM_UNDEFINED));
}

void BroadcastName(unsigned long event_type, unsigned long data1,
		   unsigned long data2, unsigned long data3, char *szName)
{
  SCM name = gh_str02scm(szName);
  apply_hooks (broadcast_name_hook, 
	       gh_list(gh_ulong2scm(event_type), 
		       gh_ulong2scm(data1),
		       gh_ulong2scm(data2),
		       gh_ulong2scm(data3),
		       name,
		       SCM_UNDEFINED));
}


/* This procedure constructs the contents of a BroadcastInfo fvwm
   module packet and returns it as a Scheme string. This and other
   fvwm-module-related stuff should go in a dynamically loadable
   module once I figure that stuff out. */

SCWM_PROC(marshal_fvwm2_config_info, "marshal-fvwm2-config-info", 1, 0, 0,
          (SCM win))
{
  ScwmWindow *psw;
  unsigned long info[24];
  int i;

  VALIDATE(win, s_marshal_fvwm2_config_info);
  psw = PSWFROMSCMWIN(win);

  info[i=0] = psw->w;
  info[++i] = psw->frame;
  info[++i] = 0; /* modules shouldn't need to know this! was (unsigned long)t; */
  info[++i] = FRAME_X(psw);
  info[++i] = FRAME_Y(psw);
  info[++i] = FRAME_WIDTH(psw);
  info[++i] = FRAME_HEIGHT(psw);
  info[++i] = psw->Desk;
  info[++i] = FlagsBitsFromSw(psw);
  info[++i] = psw->title_height;
  info[++i] = psw->boundary_width;
  info[++i] = (psw->hints.flags & PBaseSize)?psw->hints.base_width:0;
  info[++i] = (psw->hints.flags & PBaseSize)?psw->hints.base_height:0;
  info[++i] = (psw->hints.flags & PResizeInc)?psw->hints.width_inc:1;
  info[++i] = (psw->hints.flags & PResizeInc)?psw->hints.height_inc:1;
  info[++i] = psw->hints.min_width;
  info[++i] = psw->hints.min_height;
  info[++i] = psw->hints.max_width;
  info[++i] = psw->hints.max_height;
  info[++i] = psw->icon_w;
  info[++i] = psw->icon_pixmap_w;
  info[++i] = psw->hints.win_gravity;
  info[++i] = XCOLOR(psw->TextColor);
  info[++i] = XCOLOR(psw->BackColor);

  return gh_str2scm((char *)info,sizeof(info));
}

void init_module_interface()
{
#ifndef SCM_MAGIC_SNARFER
#include "module-interface.x"
#endif
  /* This will ensure that these are defined in the root module. */
  SCWM_DEFINE_HOOK(broadcast_hook, "broadcast-hook");
  SCWM_DEFINE_HOOK(broadcast_config_hook, "broadcast-config-hook");
  SCWM_DEFINE_HOOK(broadcast_name_hook, "broadcast-name-hook");
}


