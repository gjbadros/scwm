/* $Id$
 * session-manager.c
 * Copyright (C) 1998-1999, Greg J. Badros and Robert Bihlmeyer
 *
 * Scwm Session Manager Support
 * Some code derived from IceWM (C) 1997 by Marko Macek
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.GPL.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, this file may alternatively be distributed under
 * the fvwm license (see COPYING.FVWM).
 *
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#ifdef HAVE_LIBSM_LIBICE

/* The SM standard says that SmDiscardCommand should be of type
   SmLISTofARRAY8 on POSIX systems. xsm insists that it should be a SmARRAY8.
   Sigh. */
#define SUPPORT_BROKEN_DISCARD_COMMAND 1

#define SESSION_MANAGER_IMPLEMENTATION

#include "session-manager.h"

#include "icons.h"
#include "scwm.h"
#include "screen.h"
#include "shutdown.h"
#include "xproperty.h"
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <netinet/in.h>

/* from gnome-core/gsm/session.h */
#define GsmPriority              "_GSM_Priority"

/* requested priority; before the panel (40), but after the capplets (20) */
#define ScwmPriority	30

/* a serial number uniquely identifying the state file's version */
#define STATE_FILE_SERIAL sizeof(SMWindowData)

/* encapsulates state information of a window that is saved across sessions */
typedef struct SMWindowData_ {
  struct SMWindowData_ *next;
  char *id;
  char *name;
  XClassHint class;
  INT32 x, y, w, h;
  INT32 icon_x, icon_y;
  INT32 desk;
  CARD32 flags;
} SMWindowData;

static SMWindowData *SMData;	/* the head of a list of SMWindowData el's */
static char SMerror[256];
static int SavePhase = 0;

/* write a 32-bit quantity in network byte order */
static void writeI32(FILE *fd, CARD32 x)
{
  x = htonl(x);
  fwrite(&x, sizeof(x), 1, fd);
}

/* read a 32-bit quantity in network byte order */
static CARD32 readI32(FILE *fd)
{
  CARD32 x;

  if (fread(&x, sizeof(x), 1, fd) < 1)
    return 0;
  return ntohl(x);
}

/*  write a zero-terminated string */
static void writeString(FILE *fd, char *s)
{
  CARD32 len = strlen(s);

  writeI32(fd, len);
  if (len)
    fwrite(s, len, 1, fd);
}

/* read a string, zero-terminate and return it.
   The returned string must be FREE'd by the caller. */
static char *readString(FILE *fd)
{
  char *s;
  CARD32 len = readI32(fd);

  if (!len)
    return "";
  s = NEWC(len+1, char);
  fread(s, len, 1, fd);
  s[len] = 0;
  return s;
}

/* find the SM client id to which a ScwmWindow belongs.
   Returns NULL on failure.
   The returned string must be XFree'd by the caller. */
static unsigned char *getWindowClientId(ScwmWindow *psw)
{
  Atom type;
  int fmt;
  unsigned long len;
  unsigned char *leader, *id;
  Window win;

  leader = GetXProperty(psw->w, XA_WM_CLIENT_LEADER, False, &type, &fmt, &len);
  if (leader && (type != XA_WINDOW || fmt != 32 || len != 1)) {
    XFree(leader);
    leader = NULL;
  }
  if (leader)
    win = *(Window *)leader;
  else if (psw->wmhints && (psw->wmhints->flags & WindowGroupHint))
    win = psw->wmhints->window_group;
  else if (psw->fTransient)
    win = psw->transientfor;
  else
    return NULL;

  id = GetXProperty(win, XA_SM_CLIENT_ID, False, &type, &fmt, &len);
  if (id && (type != XA_STRING || fmt != 8)) {
    XFree(id);
    id = NULL;
  }
  return id;
}

/* write out the state of a ScwmWindow */
static void writeWindow(FILE *fd, ScwmWindow *psw)
{
  unsigned char *clientId;

  clientId = getWindowClientId(psw);
  if (clientId) {
    writeString(fd, clientId);
    writeString(fd, psw->name);
    writeString(fd, psw->classhint.res_name);
    writeString(fd, psw->classhint.res_class);
    writeI32(fd, FRAME_X(psw));
    writeI32(fd, FRAME_Y(psw));
    writeI32(fd, FRAME_WIDTH(psw) - psw->xboundary_width*2);
    writeI32(fd, FRAME_HEIGHT(psw)
	     - (psw->fTitle ? psw->title_height : 0) - psw->boundary_width*2);
    writeI32(fd, ICON_X(psw));
    writeI32(fd, ICON_Y(psw));
    writeI32(fd, psw->Desk);
    writeI32(fd, FlagsBitsFromSw(psw));
    XFree(clientId);
  }
}

/* read state info about a window.
   Returns NULL on EOF.
   The returned structure must be FREE'd by the caller. */
static SMWindowData *readWindow(FILE *fd)
{
  SMWindowData *d = NEW(SMWindowData);

  d->id = readString(fd);
  d->name = readString(fd);
  d->class.res_name = readString(fd);
  d->class.res_class = readString(fd);
  d->x = readI32(fd);
  d->y = readI32(fd);
  d->w = readI32(fd);
  d->h = readI32(fd);
  d->icon_x = readI32(fd);
  d->icon_y = readI32(fd);
  d->desk = readI32(fd);
  d->flags = readI32(fd);
  if (feof(fd)) {
    FREE(d);
    d = NULL;
  }
  return d;
}

/* if the ScwmWindow matches a window from the previous session,
   initialize its state with the saved information. */
void restoreWindowState(ScwmWindow *psw)
{
  SMWindowData **p, *d;
  unsigned char *clientId;
  /* FIXME  extern long isIconicState; */
  
  if (!SmcId)
    return;
  clientId = getWindowClientId(psw);
  if (!clientId)
    return;
  for (p = &SMData; (d = *p) != NULL; p = &d->next)
    if (strcmp(clientId, d->id) == 0
	&& strcmp(psw->classhint.res_name, d->class.res_name) == 0
	&& strcmp(psw->classhint.res_class, d->class.res_class) == 0)
      break;
  if (d) {			/* found match */
    PswUpdateFlags(psw, d->flags);
    psw->attr.x = d->x;
    psw->attr.y = d->y;
    psw->attr.width = d->w;
    psw->attr.height = d->h;
    psw->StartDesk = d->desk;
    psw->fStartsOnDesk = True;
    psw->icon_x_loc = d->icon_x;
    psw->icon_y_loc = d->icon_y;
    psw->fIconMoved = True;
    if (psw->fIconified) {
      psw->fStartIconic = True;
    } 
    *p = d->next;		/* remove from list */
    FREE(d);
  }
}

/* the name of the state file used by the current scwm */
static char *statefile()
{
  char *smdir, *file;

  if (!(smdir = getenv("SM_SAVE_DIR")))
    smdir = UserHome;
  file = NEWC(strlen(smdir)+6+strlen(SmcId)+1, char);
  sprintf(file, "%s/.scwm%s", smdir, SmcId);
  return file;
}

/* save the state of all windows */
static void saveYourself2(SmcConn conn, SmPointer ARG_UNUSED(client_data))
{
#define FUNC_NAME "saveYourself2"
  ScwmWindow *psw;
  char *savename = statefile();
  FILE *save;
  int successful = True;

  if (SavePhase != 1)
    return;
  SavePhase = 2;
  if ((save = fopen(savename, "w")) != NULL) {
    writeI32(save, STATE_FILE_SERIAL);
    for (psw = Scr.ScwmRoot.next; psw != NULL; psw = psw->next) {
      writeWindow(save, psw);
    }
    if (fclose(save) < 0) { 
      scwm_msg(WARN, FUNC_NAME, "Could not finish writing `%s' (%s)",
	       savename, strerror(errno));
      successful = False;
    }
  } else {
    scwm_msg(WARN, FUNC_NAME, "Could not write `%s' (%s)",
	     savename, strerror(errno));
    successful = False;
  }
  SmcSaveYourselfDone(conn, successful);
  FREEC(savename);
  SavePhase = 0;
}
#undef FUNC_NAME

/* load a number of window states to be used for restarted clients */
static void loadMyself()
{
#define FUNC_NAME "loadMyself"
  SMWindowData *d;
  char *loadname = statefile();
  FILE *load;

  SMData = NULL;
  if ((load = fopen(loadname, "r")) != NULL) {
    if (readI32(load) == STATE_FILE_SERIAL)
      while ((d = readWindow(load)) != NULL) {
	d->next = SMData;
	SMData = d;
      }
    fclose(load);
  } else {
    scwm_msg(WARN, FUNC_NAME, "Could not read `%s' (%s)",
	     loadname, strerror(errno));
  }
  FREE(loadname);
}
#undef FUNC_NAME

static
void saveYourself(SmcConn conn, SmPointer ARG_UNUSED(client_data), int ARG_UNUSED(save_type),
		  Bool ARG_UNUSED(shutdown), int ARG_UNUSED(interact_style), 
                  Bool ARG_UNUSED(fast))
{
  SavePhase = 1;
  SmcRequestSaveYourselfPhase2(conn, &saveYourself2, NULL);
}

static
void shutdownCancelled(SmcConn conn, SmPointer ARG_UNUSED(client_data))
{
  if (SavePhase == 1)		/* still waiting for Phase2? */
    SmcSaveYourselfDone(conn, False);
}

static
void saveComplete(SmcConn ARG_UNUSED(conn), SmPointer ARG_UNUSED(client_data))
{
}

static
void die(SmcConn conn, SmPointer ARG_UNUSED(client_data))
{
  SmcCloseConnection(conn, 0, NULL);
  if (conn == SMconn) {
    SMconn = NULL;
    IceSMconn = NULL;
  }
  Done(0, NULL);		/* exit cleanly */
}

static void setSMProperties()
{
  CARD8 restartStyle = SmRestartImmediately;
  CARD8 priority = ScwmPriority;
  SmPropValue userIDVal;
  SmPropValue cwdVal;
  SmPropValue *restartVal;
  SmPropValue restartStyleVal;
#ifdef SUPPORT_BROKEN_DISCARD_COMMAND
  SmPropValue discardVal;
#else
  SmPropValue discardVal[] = { { 2, "rm" }, { 2, "-f" }, { 0, NULL } };
#endif
  SmPropValue priorityVal;
  SmProp userIDProp;
  SmProp cwdProp;
  SmProp programProp;
  SmProp restartProp;
  SmProp cloneProp;
  SmProp discardProp;
  SmProp restartStyleProp;
  SmProp priorityProp;
  SmProp *props[8];
  int i, j;

  userIDVal.length = strlen(UserName);
  userIDVal.value = (SmPointer)UserName;
  restartVal = NEWC(g_argc+2, SmPropValue);
  restartStyleVal.length = 1;
  restartStyleVal.value = (SmPointer)&restartStyle;
  priorityVal.length = 1;
  priorityVal.value = (SmPointer)&priority;
  userIDProp.name = SmUserID;
  userIDProp.type = SmARRAY8;
  userIDProp.num_vals = 1;
  userIDProp.vals = &userIDVal;
  cwdProp.name = SmCurrentDirectory;
  cwdProp.type = SmARRAY8;
  cwdProp.num_vals = 1;
  cwdProp.vals = &cwdVal;
  programProp.name = SmProgram;
  programProp.type = SmARRAY8;
  programProp.num_vals = 1;
  programProp.vals = restartVal;
  restartProp.name = SmRestartCommand;
  restartProp.type =SmLISTofARRAY8;
  restartProp.num_vals = 0;
  restartProp.vals = restartVal;
  cloneProp.name = SmCloneCommand;
  cloneProp.type = SmLISTofARRAY8;
  cloneProp.num_vals = 0;
  cloneProp.vals = restartVal;
  discardProp.name = SmDiscardCommand;
#ifdef SUPPORT_BROKEN_DISCARD_COMMAND
  discardProp.type =SmARRAY8;
  discardProp.num_vals = 1;
  discardProp.vals = &discardVal;
#else
  discardProp.type =SmLISTofARRAY8;
  discardProp.num_vals = sizeof(discardVal)/ sizeof(discardVal[0]);
  discardProp.vals = discardVal;
#endif
  restartStyleProp.name = SmRestartStyleHint;
  restartStyleProp.type = SmCARD8;
  restartStyleProp.num_vals = 1;
  restartStyleProp.vals = &restartStyleVal;
  priorityProp.name = GsmPriority;
  priorityProp.type = SmCARD8;
  priorityProp.num_vals = 1;
  priorityProp.vals = &priorityVal;
  props[0] = &cwdProp;
  props[1] = &programProp;
  props[2] = &userIDProp;
  props[3] = &restartProp;
  props[4] = &cloneProp;
  props[5] = &discardProp;
  props[6] = &restartStyleProp;
  props[7] = &priorityProp;
  
  cwdVal.value = xgetcwd(NULL, 0);
  cwdVal.length = strlen(cwdVal.value);
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
  restartProp.num_vals = i;
#ifdef SUPPORT_BROKEN_DISCARD_COMMAND
  {
    char *s = statefile();
    
    discardVal.length = 6+strlen(s)+1;
    discardVal.value = NEWC(discardVal.length, char);
    sprintf(discardVal.value, "rm -f %s", s);
    FREE(s);
  }
#else
  discardVal[2].value=statefile();
  discardVal[2].length=strlen(discardVal[2].value);
#endif
  SmcSetProperties(SMconn, sizeof(props)/sizeof(props[0]), props);
#ifdef SUPPORT_BROKEN_DISCARD_COMMAND
  FREE(discardVal.value);
#else
  FREE(discardVal[2].value);
#endif
  FREE(restartVal);
  FREE(cwdVal.value);
}

static void iceWatchFD(IceConn conn, IcePointer ARG_UNUSED(client_data),
		       Bool opening, IcePointer *ARG_UNUSED(watch_data))
{
#define FUNC_NAME "iceWatchFD"
  if (opening) {
    if (IceSMfd != -1) { /* shouldn't happen */
      scwm_msg(WARN, FUNC_NAME,
	       "TOO MANY ICE CONNECTIONS -- not supported\n");
    } else {
      IceSMfd = IceConnectionNumber(conn);
    }
  } else {
    if (IceConnectionNumber(conn) == IceSMfd)
      IceSMfd = -1;
  }
}
#undef FUNC_NAME

SCM_DEFINE(SM_error_message, "SM-error-message", 0, 0, 0,
	  (),
"Return a string, describing why session management is not available.\n\
Only valid, if `SM-register' returned #f.")
#define FUNC_NAME s_SM_error_message
{
  return gh_str02scm(SMerror);
}
#undef FUNC_NAME

SCM_DEFINE(SM_register, "SM-register", 0, 0, 0,
	  (),
"Register Scwm with the session manager, and return the client id.\n\
The return value is either an id string, or #f if the session manager could not be\n\
initialized - `SM-error-message' can be used to get more information in this\n\
case.\n\
If Scwm is already registered, this function just returns the client id.")
#define FUNC_NAME s_SM_register
{
  SmcCallbacks smcall;
  char *SmcNewId;

  if (SMconn != NULL)
    return gh_str02scm(SmcId);

  XA_SM_CLIENT_ID = XInternAtom(dpy, "SM_CLIENT_ID", False);

  if (IceAddConnectionWatch(&iceWatchFD, NULL) == 0) {
    scwm_msg(WARN, FUNC_NAME , SMerror);
    strncpy(SMerror, "IceAddConnectionWatch failed.", sizeof(SMerror));
    return SCM_BOOL_F;
  }

  smcall.save_yourself.callback = &saveYourself;
  smcall.save_yourself.client_data = NULL;
  smcall.die.callback = &die;
  smcall.die.client_data = NULL;
  smcall.save_complete.callback = &saveComplete;
  smcall.save_complete.client_data = NULL;
  smcall.shutdown_cancelled.callback = &shutdownCancelled;
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
                                  sizeof(SMerror), SMerror)) == NULL)
  {
    SmcId = NULL;
    return SCM_BOOL_F;
  }
  SmcId = SmcNewId;
  IceSMconn = SmcGetIceConnection(SMconn);
  setSMProperties();
  return gh_str02scm(SmcId);
}
#undef FUNC_NAME

/* cleanly exit the session.
   Don't let the SM restart us if automatic_restart is 0. */
void doneSM(int automatic_restart)
{
  CARD8 restartStyle = SmRestartIfRunning;
  SmPropValue restartStyleVal;
  SmProp restartStyleProp;
  SmProp *props[1];

  if (!SMconn)
    return;

  restartStyleVal.length = 1;
  restartStyleVal.value = &restartStyle;
  restartStyleProp.name = SmRestartStyleHint;
  restartStyleProp.type = SmCARD8;
  restartStyleProp.num_vals = 1;
  restartStyleProp.vals = &restartStyleVal;
  props[0] = &restartStyleProp;
  
  if (!automatic_restart)
    SmcSetProperties(SMconn, sizeof(props)/sizeof(props[0]), props);
  SmcCloseConnection(SMconn, 0, NULL);
}

void initSM()
{
#ifndef SCM_MAGIC_SNARFER
#include "session-manager.x"
#endif

  if (SmcId)
    loadMyself();
}

#endif /* HAVE_LIBSM_LIBICE */


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

