/* $Id$
 * module-interface.h 
 * unknown 
 * <@cs.washington.edu> 
 * Seattle, WA  USA  {FIX: use env_vars}
 * http://www.cs.washington.edu/homes/gjb
 */

#ifndef _MODULE_INTERFACE_H_
#define _MODULE_INTERFACE_H_

void Broadcast(unsigned long event_type, unsigned long num_datum,
	       unsigned long data1, unsigned long data2, unsigned long data3, 
	       unsigned long data4, unsigned long data5, unsigned long data6,
	       unsigned long data7);

void BroadcastConfig(unsigned long event_type, ScwmWindow *t);

void BroadcastName(unsigned long event_type, unsigned long data1,
		   unsigned long data2, unsigned long data3, char *name);

SCM marshal_fvwm2_config_info (SCM win);

#endif /* _MODULE_INTERFACE_H_ */
