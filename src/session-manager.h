/* $Id$
 * session-manager.h 
 * Copyright (C) 1998, 1999, 2000 Greg J. Badros and Robert Bihlmeyer
 * Scwm Session Manager Support
 */

#ifndef SESSION_MANAGER_H__
#define SESSION_MANAGER_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/X.h>
#include <X11/SM/SMlib.h>
#include <stdio.h>
#include "scwm.h"

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

EXTERN Atom XA_SM_CLIENT_ID;

void initSM();
void doneSM(int);
void restoreWindowState(ScwmWindow *);

#endif /* SESSION_MANAGER_H__ */
