/* $Id$
 * module-interface.h 
 * unknown 
 * <@cs.washington.edu> 
 * Seattle, WA  USA  {FIX: use env_vars}
 * http://www.cs.washington.edu/homes/gjb
 */

#ifndef _MODULE_INTERFACE_H_
#define _MODULE_INTERFACE_H_

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include "window_fwd.h"

void Broadcast(unsigned long event_type, unsigned long num_datum,
	       unsigned long data1, unsigned long data2, unsigned long data3, 
	       unsigned long data4, unsigned long data5, unsigned long data6,
	       unsigned long data7);

void BroadcastIconInfo(unsigned long event_type, const ScwmWindow *psw);

void BroadcastConfig(unsigned long event_type, const struct ScwmWindow *t);

void BroadcastName(unsigned long event_type, unsigned long data1,
		   unsigned long data2, unsigned long data3, char *name);

void BroadcastMiniIcon(unsigned long event_type, struct ScwmWindow *psw);

#endif /* _MODULE_INTERFACE_H_ */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

