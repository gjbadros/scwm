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

SCM 
scm_cons_varargs (int citems, ...)
{
  va_list val;
  SCM rgconscells[20];
  int i = 0;

  va_start(val,citems);
  while (--citems > 0) {
    SCM scm;
    SCM_NEWCELL(rgconscells[i]);
    scm = va_arg(val,SCM);
    SCM_SETCAR(rgconscells[i],scm);
    i++;
  }
  SCM_NEWCELL(rgconscells[i]);
  SCM_SETCAR(rgconscells[i],scm_listofnull);

  while (--i > 1) {
    SCM_SETCDR(rgconscells[i-1],rgconscells[i]);
  }

  return rgconscells[0];
}

void
Broadcast(unsigned long event_type, unsigned long num_datum,
	  unsigned long data1, unsigned long data2, unsigned long data3, 
	  unsigned long data4, unsigned long data5, unsigned long data6,
	  unsigned long data7)
{
  SCM proc = gh_lookup("broadcast-hook");
  if (proc != SCM_UNDEFINED && gh_procedure_p(proc)) {
    fprintf(stderr,"calling Broadcast-hook\n");
    scm_apply(proc, gh_ulong2scm(event_type), 
	      scm_cons_varargs(num_datum,
			       gh_ulong2scm(data1),
			       gh_ulong2scm(data2),
			       gh_ulong2scm(data3),
			       gh_ulong2scm(data4),
			       gh_ulong2scm(data5),
			       gh_ulong2scm(data6),
			       gh_ulong2scm(data7)));
  }
}


void
BroadcastConfig(unsigned long event_type, ScwmWindow *t)
{
  /* SendConfig */
}

void BroadcastName(unsigned long event_type, unsigned long data1,
		   unsigned long data2, unsigned long data3, char *name)
{
  /* SendName */
}
