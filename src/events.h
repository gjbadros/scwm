/* $Id$
 * events.h
 * Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
 */


#ifndef EVENTS_H
#define EVENTS_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <libguile.h>

#include "scwm.h"

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h> /* for file descriptors */
#endif

#undef EXTERN
#undef EXTERN_SET
#ifdef EVENTS_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

EXTERN SCM *pscm_configure_request_handled;

extern Window w_for_scwmexec_response;  

extern XContext ExposeWindowProcContext;
typedef int (*ExposeProc)(Window);

void init_input_hooks();
void run_input_hooks(fd_set *in_fdset);
void add_hook_fds_to_set(fd_set *in_fdset, int *fd_width);
SCM add_input_hook (SCM fd, SCM thunk);

void DispatchEvent(void);

void InitEventHandlerJumpTable(void);

int NextScwmEvent(Display *dpy, XEvent *event, Bool fNoBlock);
int NoEventsScwmUpdate(Bool fNoBlock);

void HandlePaging(int, int, int *, int *, int *, int *, Bool);

void HandleHardFocus(struct ScwmWindow * t);

void HandleMappingNotify(void);
void HandleEvents(void);
void HandleExpose(void);
void HandleFocusIn(void);
void HandleDestroyNotify(void);
void HandleMapRequest(void);
void HandleMapRequestKeepRaised(Window keepraised);
void HandleMapNotify(void);
void HandleUnmapNotify(void);
void HandleButtonPress(void);
void HandleEnterNotify(void);
void HandleLeaveNotify(void);
void HandleConfigureRequest(void);
void HandleClientMessage(void);
void HandlePropertyNotify(void);
void HandleKeyPress(void);
void HandleKeyRelease(void);
void HandleVisibilityNotify(void);
void CoerceEnterNotifyOnCurrentWindow();

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

