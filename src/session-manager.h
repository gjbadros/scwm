/* $Id$
 * session-manager.h 
 * (C) 1998 Greg J. Badros
 * Scwm Session Manager Support
 */

#ifndef SESSION_MANAGER_H__
#define SESSION_MANAGER_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/SM/SMlib.h>
/* for NULL */
#include <stdio.h>

#undef EXTERN
#undef EXTERN_SET
#ifdef SESSION_MANAGER_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

EXTERN_SET(int IceSMfd, -1);
EXTERN_SET(IceConn IceSMconn, NULL);
EXTERN_SET(SmcConn SMconn, NULL);
EXTERN_SET(char *SmcId, NULL);

void initSM();

#endif /* SESSION_MANAGER_H__ */
