/* $Id$
 * session-manager.c
 * Copyright 1998, Greg J. Badros and Robert Bihlmeyer
 * Scwm Session Manager Support
 * Largely borrowed from IceWM's icewm.cc (C) 1997 by Marko Macek
 */

#define SESSION_MANAGER_IMPLEMENTATION

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "session-manager.h"
#include "scwm.h"

void iceWatchFD(IceConn conn,
                IcePointer client_data,
                Bool opening,
                IcePointer *watch_data)
{
  if (opening) {
    if (IceSMfd != -1) { /* shouldn't happen */
      scwm_msg(WARN,"iceWatchFD",
	       "TOO MANY ICE CONNECTIONS -- not supported\n");
    } else {
      IceSMfd = IceConnectionNumber(conn);
    }
  } else {
    if (IceConnectionNumber(conn) == IceSMfd)
      IceSMfd = -1;
  }
}

static
void saveYourselfPhase2Proc(SmcConn conn, SmPointer client_data) {
  SmcSaveYourselfDone(conn, True);
}

static
void saveYourselfProc(SmcConn conn,
                      SmPointer client_data,
                      int save_type,
                      Bool shutdown,
                      int interact_style,
                      Bool fast)
{
  SmcRequestSaveYourselfPhase2(conn, &saveYourselfPhase2Proc, NULL);
}

static
void shutdownCancelledProc(SmcConn conn, SmPointer client_data) {
}

static
void saveCompleteProc(SmcConn conn, SmPointer client_data) {
}

static
void dieProc(SmcConn conn, SmPointer client_data) {
  SmcCloseConnection(conn, 0, NULL);
  if (conn == SMconn) {
    SMconn = NULL;
    IceSMconn = NULL;
  }
}

static void setSMProperties() {
  CARD8 restartStyle = SmRestartImmediately;
  SmPropValue programVal = { 0, NULL };
  SmPropValue userIDVal = { 0, NULL };
  SmPropValue *restartVal = NEWC(g_argc+2, SmPropValue);
  SmPropValue restartStyleVal = { 1, &restartStyle };
  SmProp programProp = { SmProgram, SmARRAY8, 1, &programVal };
  SmProp userIDProp = { SmUserID, SmARRAY8, 1, &userIDVal };
  SmProp restartProp = { SmRestartCommand, SmLISTofARRAY8, 0, restartVal };
  SmProp cloneProp = { SmCloneCommand, SmLISTofARRAY8, 0, restartVal };
  SmProp restartStyleProp = { SmRestartStyleHint, SmCARD8,
			      1, &restartStyleVal };
  SmProp *props[] = {
    &programProp,
    &userIDProp,
    &restartProp,
    &cloneProp,
    &restartStyleProp
  };
  int i, j;

  programVal.value = strrchr(g_argv[0], '/');
  if (!programVal.value)
    programVal.value = g_argv[0];
  programVal.length = strlen(programVal.value);
  userIDVal.length = strlen(UserName);
  userIDVal.value = (SmPointer)UserName;
  for (i=0, j=0; j<g_argc; j++) {
    if (strcmp(g_argv[i], CLIENT_ID_OPT_STRING)!=0) {
      restartVal[i].length = strlen(g_argv[j]);
      restartVal[i++].value = g_argv[j];
    } else {
      j++;			/* skip old client-id option and argument */
    }
  }
  cloneProp.num_vals=i;
  restartVal[i].length = strlen(CLIENT_ID_OPT_STRING);
  restartVal[i++].value = CLIENT_ID_OPT_STRING;
  restartVal[i].length = strlen(SmcId);
  restartVal[i++].value = SmcId;
  restartProp.num_vals=i;
  SmcSetProperties(SMconn, sizeof(props)/sizeof(props[0]), props);
  FREE(restartVal);
}

void initSM() {
  char error_str[256];
  SmcCallbacks smcall;
  char *SmcNewId;

  if (IceAddConnectionWatch(&iceWatchFD, NULL) == 0) {
    scwm_msg(WARN,"initSM","IceAddConnectionWatch failed.");
    return ;
  }

  memset(&smcall, 0, sizeof(smcall));
  smcall.save_yourself.callback = &saveYourselfProc;
  smcall.save_yourself.client_data = NULL;
  smcall.die.callback = &dieProc;
  smcall.die.client_data = NULL;
  smcall.save_complete.callback = &saveCompleteProc;
  smcall.save_complete.client_data = NULL;
  smcall.shutdown_cancelled.callback = &shutdownCancelledProc;
  smcall.shutdown_cancelled.client_data = NULL;
    
  if ((SMconn = SmcOpenConnection(NULL, /* network ids */
                                  NULL, /* context */
                                  1, 0, /* protocol major, minor */
                                  SmcSaveYourselfProcMask |
                                  SmcSaveCompleteProcMask |
                                  SmcShutdownCancelledProcMask |
                                  SmcDieProcMask,
                                  &smcall,
                                  SmcId, &SmcNewId,
                                  sizeof(error_str), error_str)) == NULL)
    {
      scwm_msg(WARN,"initSM","session manager initialization failed: %s\n", error_str);
      return ;
    } 
  SmcId = SmcNewId;
  IceSMconn = SmcGetIceConnection(SMconn);
  setSMProperties();
}

/* cleanly exit the session.
   Don't let the SM restart us if automatic_restart is 0. */
void doneSM(int automatic_restart)
{
  CARD8 restartStyle = SmRestartIfRunning;
  SmPropValue restartStyleVal = { 1, &restartStyle };
  SmProp restartStyleProp = { SmRestartStyleHint, SmCARD8,
			      1, &restartStyleVal };

  if (!automatic_restart)
    SmcSetProperties(SMconn, 1, &restartStyleProp);
  SmcCloseConnection(SMconn, 0, NULL);
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
