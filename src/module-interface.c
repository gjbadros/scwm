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
   variety thereof to keep bad broadcat handlers
   from crashing scwm.  MSFIX: do you know how to do this?
   --gjb 11/27/97 */

void
Broadcast(unsigned long event_type, unsigned long num_datum,
	  unsigned long data1, unsigned long data2, unsigned long data3, 
	  unsigned long data4, unsigned long data5, unsigned long data6,
	  unsigned long data7)
{
  SCM proc = gh_lookup("broadcast-hook");
  if (proc != SCM_UNDEFINED) {
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
      scwm_msg(ERR,__FUNCTION__,"broadcast-hook is not a procedure -- undefined it");
      gh_define("broadcast-hook",SCM_UNDEFINED);
    }
  }
}


void
BroadcastConfig(unsigned long event_type, ScwmWindow *sw)
{
  SCM proc = gh_lookup("broadcast-config-hook");
  if (proc != SCM_UNDEFINED) {
    if (gh_procedure_p(proc)) {
      gh_apply(proc, gh_list(
	gh_ulong2scm(event_type), 
	sw->schwin,
	SCM_UNDEFINED
	));
    } else {
      scwm_msg(ERR,__FUNCTION__,"broadcast-config-hook is not a procedure -- undefined it");
      gh_define("broadcast-config-hook",SCM_UNDEFINED);
    }
  }
}

void BroadcastName(unsigned long event_type, unsigned long data1,
		   unsigned long data2, unsigned long data3, char *szName)
{
  SCM proc = gh_lookup("broadcast-name-hook");
  if (proc != SCM_UNDEFINED) {
    if (gh_procedure_p(proc)) {
      SCM name = gh_str02scm(szName);
      fprintf(stderr,"calling broadcast-name-hook\n");
      gh_apply(proc, gh_list(
	gh_ulong2scm(event_type), 
	gh_ulong2scm(data1),
	gh_ulong2scm(data2),
	gh_ulong2scm(data3),
	name,
	SCM_UNDEFINED
	));
    } else {
      scwm_msg(ERR,__FUNCTION__,"broadcast-name-hook is not a procedure -- undefined it");
      gh_define("broadcast-name-hook",SCM_UNDEFINED);
    }
  }
}
