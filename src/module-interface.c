/* $Id$
 * module-interface.c
 * Copyright (C) 1998-1999 Maciej Stachowiak and Greg J. Badros
 * 
 * Replaces the module stuff from fvwm2 with hooks to call
 * scheme code which can then talk to a module via the old mechanisms
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdarg.h>
#include <assert.h>
#include <X11/X.h>

#include <guile/gh.h>

#include "module-interface.h"

#include "scwm.h"
#include "screen.h"
#include "window.h"
#include "callbacks.h"
#include "scwm-constraints.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

SCWM_HOOK(broadcast_hook, "broadcast-hook", 9,
"This hook is invoked whenever fvwm2 would call Broadcast.
This hook is principally of use in implementing the fvwm2
module interface and for stuff that needs to be notified in ways that
can't be done with the proper hooks that have been included so
far. The procedures in this hook are passed a numerical code
representing the event type, a number that indicates how many of the
following data arguments are meaningful, and 7 numeric data arguments.");

SCWM_HOOK(broadcast_config_hook, "broadcast-config-hook", 2,
"This hook is invoked whenever fvwm2 would call BroadcastConfig.
This hook is principally of use in implementing the fvwm2
module interface and for stuff that needs to be notified in ways that
can't be done with the proper hooks that have been included so
far. The procedures in this hook are passed two arguments: the event-type
and the window object.");

SCWM_HOOK(broadcast_name_hook, "broadcast-name-hook", 5,
"This hook is invoked whenever fvwm2 would call BroadcastName.
This hook is principally of use in implementing the fvwm2
module interface and for stuff that needs to be notified in ways
that can't be done with the proper hooks that have been included so
far. The procedures in this hook are passed an event type, three
numeric data arguments, and a string.");

SCWM_HOOK(broadcast_mini_icon_hook, "broadcast-mini-icon-hook", 2,
"This hook is invoked whenever fvwm2 would call BroadcastMiniIcon.
This hook is principally of use in implementing the fvwm2
module interface and for stuff that needs to be notified in ways that
can't be done with the proper hooks that have been included so
far. The procedures The procedures in this hook are passed two arguments:
the event-type and the window object.");


void
Broadcast(unsigned long event_type, unsigned long num_datum,
	  unsigned long data1, unsigned long data2, unsigned long data3, 
	  unsigned long data4, unsigned long data5, unsigned long data6,
	  unsigned long data7)
{
  scwm_run_hook(broadcast_hook, 
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
BroadcastIconInfo(unsigned long event_type, const ScwmWindow *psw)
{
  assert(event_type == M_ICONIFY || event_type == M_ICON_LOCATION);
  Broadcast(event_type, 7, psw->w, psw->frame,
            (unsigned long) psw,
	    ICON_X_VP(psw), ICON_Y_VP(psw),
            psw->icon_p_width,
            psw->icon_w_height + psw->icon_p_height);
}



void
BroadcastConfig(unsigned long event_type, const ScwmWindow *psw)
{
  if (Scr.fWindowsCaptured) {
    scwm_run_hook(broadcast_config_hook, 
                  gh_list(gh_ulong2scm(event_type), 
                          SCM_FROM_PSW(psw),
                          SCM_UNDEFINED));
  }
}

void BroadcastName(unsigned long event_type, unsigned long data1,
		   unsigned long data2, unsigned long data3, char *szName)
{
  SCM name = gh_str02scm(szName);
  if (Scr.fWindowsCaptured) {
    scwm_run_hook(broadcast_name_hook, 
                  gh_list(gh_ulong2scm(event_type), 
                          gh_ulong2scm(data1),
                          gh_ulong2scm(data2),
                          gh_ulong2scm(data3),
                          name,
                          SCM_UNDEFINED));
  }
}

void BroadcastMiniIcon(unsigned long event_type, ScwmWindow *psw)
{
  if (Scr.fWindowsCaptured) {
    scwm_run_hook(broadcast_mini_icon_hook, 
                  gh_list(gh_ulong2scm(event_type), 
                          SCM_FROM_PSW(psw),
                          SCM_UNDEFINED));
  }
}

/* This and other fvwm-module-related stuff should go in a dynamically
   loadable module once I figure that stuff out. */

SCWM_PROC(marshal_fvwm2_config_info, "marshal-fvwm2-config-info", 1, 0, 0,
          (SCM win),
"Constructs a fvwm2 BroadcastInfo module packet.\n\
The return value is the contents of a BroadcastInfo fvwm2\n\
module packet for WIN as a Scheme string.")
#define FUNC_NAME s_marshal_fvwm2_config_info
{
  ScwmWindow *psw;
  unsigned long info[24];
  int i;

  VALIDATE_ARG_WIN_COPY(1,win,psw);

  info[i=0] = psw->w;
  info[++i] = psw->frame;
  info[++i] = 0; /* was psw; modules cannot do anything with that value, though */
  info[++i] = FRAME_X_VP(psw);
  info[++i] = FRAME_Y_VP(psw);
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
#undef FUNC_NAME


SCWM_PROC(marshal_fvwm2_iconify_info, "marshal-fvwm2-iconify-info", 1, 0, 0,
          (SCM win),
"Constructs a fvwm2 \"M_ICONIFY\" module packet.\n\
The return value is the contents of an \"M_ICONIFY\" fvwm\n\
module packet for WIN as a Scheme string.")
#define FUNC_NAME s_marshal_fvwm2_iconify_info
{
  ScwmWindow *psw;
  unsigned long info[8];

  VALIDATE_ARG_WIN_COPY(1,win,psw);

  info[0] = 7;
  info[1] = psw->w;
  info[2] = psw->frame;
  info[3] = 0; /* Was psw itself - BROKEN! */
  if((psw->fIconUnmapped)) {
    info[4] = 0;
    info[5] = 0;
    info[6] = 0;
    info[7] = 0;
  } else {
    info[4] = ICON_X_VP(psw);
    info[5] = ICON_Y_VP(psw);
    info[6] = psw->icon_w_width;
    info[7] = psw->icon_w_height+psw->icon_p_height;
  }

  return gh_str2scm((char *)info,sizeof(info));
}
#undef FUNC_NAME

void init_module_interface()
{
#ifndef SCM_MAGIC_SNARFER
#include "module-interface.x"
#endif
  /* This will ensure that these are defined in the root module. */

}



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

