/* $Id$
 * session-manager.c
 * Copyright 1998, Greg J. Badros and Robert Bihlmeyer
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

#define SESSION_MANAGER_IMPLEMENTATION

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "session-manager.h"
#include "scwm.h"
#include "screen.h"
#include "shutdown.h"
#include "xproperty.h"
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <endian.h>
#if   __BYTE_ORDER == __LITTLE_ENDIAN
# define hton32(x) ((x >> 24)|((0xff0000 & x) >> 8)|((0xff00 & x) << 8)| \
			((0xff & x) << 24))
# define ntoh32(x) hton32(x)
#elif __BYTE_ORDER == __BIG_ENDIAN
# define hton32(x) (x)
# define ntoh32(x) (x)
#else
#error Unknown __BYTE_ORDER
#endif

/* encapsulates state information of a window that is saved across sessions */
typedef struct SMWindowData_ {
  struct SMWindowData_ *next;
  char *id;
  char *name;
  XClassHint class;
  INT32 x, y, w, h;
} SMWindowData;

SMWindowData *SMData;		/* the head of a list of SMWindowData el's */

/* write a 32-bit quantity in network byte order */
static void writeI32(FILE *fd, CARD32 x)
{
  x = hton32(x);
  fwrite(&x, sizeof(x), 1, fd);
}

/* read a 32-bit quantity in network byte order */
static CARD32 readI32(FILE *fd)
{
  CARD32 x;

  fread(&x, sizeof(x), 1, fd);
  return ntoh32(x);
}

/*  write a zero-terminated string */
static void writeString(FILE *fd, char *s)
{
  CARD32 len = strlen(s);

  writeI32(fd, len);
  if (len)
    fwrite(s, len, 1, fd);
}

/* read a string, zero-terminate and return it */
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

/* write out the state of a ScwmWindow */
static void writeWindow(FILE *fd, ScwmWindow *psw)
{
  Atom type;
  int fmt;
  unsigned long len;
  char *clientId;

  clientId = GetXProperty(psw->w, XA_SM_CLIENT_ID, False,
		     &type, &fmt, &len);
  if (clientId) {
    if (type == XA_STRING && fmt == 8) {
      writeString(fd, clientId);
      writeString(fd, psw->name);
      writeString(fd, psw->classhint.res_name);
      writeString(fd, psw->classhint.res_class);
      writeI32(fd, psw->frame_x);
      writeI32(fd, psw->frame_y);
      writeI32(fd, psw->frame_width);
      writeI32(fd, psw->frame_height);
    }
    XFree(clientId);
  }
}

/* read state info about a window */
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
  return d;
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
static void saveYourself2(SmcConn conn, SmPointer client_data)
{
#define FUNC_NAME "saveYourself2"
  ScwmWindow *psw;
  char *savename = statefile();
  FILE *save;
  int successful = True;

  scwm_msg(DBG, FUNC_NAME, "Dumping to `%s'", savename);
  if ((save = fopen(savename, "w")) != NULL) {
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
  FREE(savename);
#undef FUNC_NAME
}

/* load a number of window states to be used for restarted clients */
static void loadMyself()
{
#define FUNC_NAME "loadMyself"
  SMWindowData *d;
  char *loadname = statefile();
  FILE *load;

  scwm_msg(DBG, FUNC_NAME, "Restoring from `%s'", loadname);
  if ((load = fopen(loadname, "r")) != NULL) {
    SMData = NULL;
    d = readWindow(load);
    d->next = SMData;
    SMData = d;
    fclose(load);
  } else {
    scwm_msg(WARN, FUNC_NAME, "Could not read `%s' (%s)",
	     loadname, strerror(errno));
  }
  FREE(loadname);
#undef FUNC_NAME
}

/* if the ScwmWindow matches a window from the previous session,
   initialize its state with the saved information */
void restoreWindowState(ScwmWindow *psw)
{
  /* do nothing yet */
}

static
void saveYourself(SmcConn conn, SmPointer client_data, int save_type,
		  Bool shutdown, int interact_style, Bool fast)
{
  SmcRequestSaveYourselfPhase2(conn, &saveYourself2, NULL);
}

static
void shutdownCancelled(SmcConn conn, SmPointer client_data)
{
}

static
void saveComplete(SmcConn conn, SmPointer client_data)
{
}

static
void die(SmcConn conn, SmPointer client_data)
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
  SmPropValue userIDVal = { strlen(UserName), (SmPointer)UserName };
  SmPropValue cwdVal;
  SmPropValue *restartVal = NEWC(g_argc+2, SmPropValue);
  SmPropValue restartStyleVal = { 1, &restartStyle };
  SmPropValue discardVal[] = { { 2, "rm" }, { 2, "-f" }, { 0, NULL } };
  SmProp userIDProp = { SmUserID, SmARRAY8, 1, &userIDVal };
  SmProp cwdProp = { SmCurrentDirectory, SmARRAY8, 1, &cwdVal };
  SmProp programProp = { SmProgram, SmARRAY8, 1, restartVal };
  SmProp restartProp = { SmRestartCommand, SmLISTofARRAY8, 0, restartVal };
  SmProp cloneProp = { SmCloneCommand, SmLISTofARRAY8, 0, restartVal };
  SmProp discardProp = { SmDiscardCommand, SmLISTofARRAY8, sizeof(discardVal)/
			 sizeof(discardVal[0]), discardVal };
  SmProp restartStyleProp = { SmRestartStyleHint, SmCARD8,
			      1, &restartStyleVal };
  SmProp *props[] = {
    &cwdProp,
    &programProp,
    &userIDProp,
    &restartProp,
    &cloneProp,
    &discardProp,
    &restartStyleProp
  };
  int i, j;

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
  restartProp.num_vals=i;
  discardVal[2].value=statefile();
  discardVal[2].length=strlen(discardVal[2].value);
  SmcSetProperties(SMconn, sizeof(props)/sizeof(props[0]), props);
  FREE(discardVal[2].value);
  FREE(restartVal);
  FREE(cwdVal.value);
}

static void iceWatchFD(IceConn conn, IcePointer client_data,
		       Bool opening, IcePointer *watch_data)
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

void initSM()
{
  char error_str[256];
  SmcCallbacks smcall;
  char *SmcNewId;

  XA_SM_CLIENT_ID = XInternAtom(dpy, "SM_CLIENT_ID", False);

  if (SmcId)
    loadMyself();

  if (IceAddConnectionWatch(&iceWatchFD, NULL) == 0) {
    scwm_msg(WARN,"initSM","IceAddConnectionWatch failed.");
    return ;
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
                                  sizeof(error_str), error_str)) == NULL)
    {
      scwm_msg(WARN,"initSM","session manager initialization failed: %s\n",
	       error_str);
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
  SmProp *props[] = { &restartStyleProp };

  if (!SMconn)
    return;
  if (!automatic_restart)
    SmcSetProperties(SMconn, sizeof(props)/sizeof(props[0]), props);
  SmcCloseConnection(SMconn, 0, NULL);
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
