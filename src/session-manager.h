/* $Id$
 * session-manager.h 
 * (C) 1998 Greg J. Badros
 * Scwm Session Manager Support
 */

#ifndef SESSION_MANAGER_H__
#define SESSION_MANAGER_H__

#include <X11/SM/SMlib.h>

extern int IceSMfd;
extern IceConn IceSMconn;
extern SmcConn SMconn;
extern char *oldSessionId;
extern char *newSessionId;
extern char *sessionProg;

void initSM();

#endif /* _SESSION-MANAGER_H_ */
