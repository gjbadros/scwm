/*
 * $Id$
 * module-interface.c
 * Replaces the module stuff from fvwm2 with hooks to call
 * scheme code which can then talk to a module via the old mechanisms
 */

#include <X11/X.h>
#include <guile/gh.h>
#include <stdarg.h>
#include "scwm.h"
#include "misc.h"

/* FIXGJB: use call_thunk_with_message_handler, or some 
   variety thereof to keep bad broadcast handlers
   from crashing scwm.  MSFIX: do you know how to do this?
   --gjb 11/27/97 */

/* FIXMS: these should really be lists of procedures, or something
   higher level should provide proper hook add/remove functionality,
   otherwise it is kind of a pain to use them. */

SCM *loc_broadcast_hook;
SCM *loc_broadcast_config_hook;
SCM *loc_broadcast_name_hook;

void
Broadcast(unsigned long event_type, unsigned long num_datum,
	  unsigned long data1, unsigned long data2, unsigned long data3, 
	  unsigned long data4, unsigned long data5, unsigned long data6,
	  unsigned long data7)
{
  SCM proc = *loc_broadcast_hook;
  if (proc != SCM_BOOL_F) {
    if (gh_procedure_p(proc)) {
      gh_apply(proc, gh_list(
	gh_ulong2scm(event_type), 
	gh_ulong2scm(num_datum),
	gh_ulong2scm(data1),
	gh_ulong2scm(data2),
	gh_ulong2scm(data3),
	gh_ulong2scm(data4),
	gh_ulong2scm(data5),
	gh_ulong2scm(data6),
	gh_ulong2scm(data7),
	SCM_UNDEFINED
	));
    } else {
      scwm_msg(ERR,__FUNCTION__,"broadcast-hook is not a procedure -- unsetting it");
      *loc_broadcast_hook = SCM_BOOL_F;
    }
  }
}


void
BroadcastConfig(unsigned long event_type, ScwmWindow *sw)
{
  SCM proc = *loc_broadcast_config_hook;
  if (proc != SCM_BOOL_F) {
    if (gh_procedure_p(proc)) {
      gh_apply(proc, gh_list(
	gh_ulong2scm(event_type), 
	sw->schwin,
	SCM_UNDEFINED
	));
    } else {
      scwm_msg(ERR,__FUNCTION__,"broadcast-config-hook is not a procedure -- unsetting it");
      *loc_broadcast_config_hook = SCM_BOOL_F;
    }
  }
}

void BroadcastName(unsigned long event_type, unsigned long data1,
		   unsigned long data2, unsigned long data3, char *szName)
{
  SCM proc = *loc_broadcast_name_hook;
  if (proc != SCM_BOOL_F) {
    if (gh_procedure_p(proc)) {
      SCM name = gh_str02scm(szName);
      gh_apply(proc, gh_list(
	gh_ulong2scm(event_type), 
	gh_ulong2scm(data1),
	gh_ulong2scm(data2),
	gh_ulong2scm(data3),
	name,
	SCM_UNDEFINED
	));
    } else {
      scwm_msg(ERR,__FUNCTION__,"broadcast-name-hook is not a procedure -- unsetting it");
      gh_define("broadcast-name-hook",SCM_BOOL_F);
    }
  }
}


/* This procedure constructs the contents of a BroadcastInfo fvwm
   module packet and returns it as a Scheme string. This and other
   fvwm-module-related stuff should go in a dynamically loadable
   module once I figure that stuff out. */

SCM_PROC(s_marshal_fvwm2_config_info, "marshal-fvwm2-config-info", 1, 0, 0, marshal_fvwm2_config_info);

SCM marshal_fvwm2_config_info (SCM win)
{
  ScwmWindow *sw;
  unsigned long info[24];

  VALIDATE(win, s_marshal_fvwm2_config_info);
  sw = SCWMWINDOW(win);

  info[0] = sw->w;
  info[1] = sw->frame;
  info[2] = 0; /* modules shouldn't need to know this! was (unsigned long)t; */
  info[3] = sw->frame_x;
  info[4] = sw->frame_y;
  info[5] = sw->frame_width;
  info[6] = sw->frame_height;
  info[7] = sw->Desk;
  info[8] = FlagsBitsFromSw(sw);
  info[9] = sw->title_height;
  info[10] = sw->boundary_width;
  info[11] = (sw->hints.flags & PBaseSize)?sw->hints.base_width:0;
  info[12] = (sw->hints.flags & PBaseSize)?sw->hints.base_height:0;
  info[13] = (sw->hints.flags & PResizeInc)?sw->hints.width_inc:1;
  info[14] = (sw->hints.flags & PResizeInc)?sw->hints.height_inc:1;
  info[15] = sw->hints.min_width;
  info[16] = sw->hints.min_height;
  info[17] = sw->hints.max_width;
  info[18] = sw->hints.max_height;
  info[19] = sw->icon_w;
  info[20] = sw->icon_pixmap_w;
  info[21] = sw->hints.win_gravity;
  info[22] = XCOLOR(sw->TextColor);
  info[23] = XCOLOR(sw->BackColor);

  return (gh_str2scm((char *)info,24*sizeof(unsigned long)));
}

void init_module_interface()
{
#ifndef SCM_MAGIC_SNARFER
#include "module-interface.x"
#endif
  /* This will ensure that these are defined in the root module. */
  loc_broadcast_hook = SCM_CDRLOC
    (scm_sysintern("broadcast-hook", SCM_BOOL_F));
  loc_broadcast_config_hook = SCM_CDRLOC
    (scm_sysintern("broadcast-config-hook", SCM_BOOL_F));
  loc_broadcast_name_hook = SCM_CDRLOC
    (scm_sysintern("broadcast-name-hook", SCM_BOOL_F));  
}


