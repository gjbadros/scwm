/* $Id$
 * session-manager.c
 * (C) 1998 Greg J. Badros
 * Scwm Session Manager Support
 * Largely borrowed from IceWM's icewm.cc (C) 1997 by Marko Macek
 */

#include "session-manager.h"

#include "scwm.h"

int IceSMfd = -1;
IceConn IceSMconn = NULL;
SmcConn SMconn = NULL;
char *oldSessionId = NULL;
char *newSessionId = NULL;
char *sessionProg;

void iceWatchFD(IceConn conn,
                IcePointer client_data,
                Bool opening,
                IcePointer *watch_data)
{
  if (opening) {
    if (IceSMfd != -1) { /* shouldn't happen */
      scwm_msg(WARN,"iceWatchFD","TOO MANY ICE CONNECTIONS -- not supported\n");
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
  SmPropValue programVal = { 0, NULL };
  SmPropValue userIDVal = { 0, NULL };
  SmPropValue restartVal[3] = { { 0, NULL }, { 0, NULL }, { 0, NULL } };
  /* broken headers ??? */
  SmProp programProp = { (char *)SmProgram, 
                         (char *)SmLISTofARRAY8, 
                         1, &programVal };
  SmProp userIDProp = { (char *)SmUserID, 
                        (char *)SmARRAY8, 
                        1, &userIDVal };
  SmProp restartProp = { (char *)SmRestartCommand,
                         (char *)SmLISTofARRAY8, 
                         3, (SmPropValue *)&restartVal };
  SmProp cloneProp = { (char *)SmCloneCommand,
                       (char *)SmLISTofARRAY8, 
                       2, (SmPropValue *)&restartVal };
  SmProp *props[] = {
    &programProp,
    &userIDProp,
    &restartProp,
    &cloneProp
  };

  char *user = getenv("USER");
  const char *clientId = "-clientId";

  programVal.length = strlen(sessionProg);
  programVal.value = sessionProg;
  userIDVal.length = strlen(user);
  userIDVal.value = (SmPointer)user;
  restartVal[0].length = strlen(sessionProg);
  restartVal[0].value = sessionProg;
  restartVal[1].length = strlen(clientId);
  restartVal[1].value = (char *)clientId;
  restartVal[2].length = strlen(newSessionId);
  restartVal[2].value = newSessionId;

  SmcSetProperties(SMconn,
                   sizeof(props)/sizeof(props[0]),
                   (SmProp **)&props);
}

void initSM() {
  char error_str[256];
  SmcCallbacks smcall;

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
                                  oldSessionId, &newSessionId,
                                  sizeof(error_str), error_str)) == NULL)
    {
      scwm_msg(WARN,"initSM","session manager initialization failed: %s\n", error_str);
      return ;
    }
  IceSMconn = SmcGetIceConnection(SMconn);

  setSMProperties();
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
