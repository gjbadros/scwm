/* $Id$
 * events.h
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */


#ifndef EVENTS_H
#define EVENTS_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <libguile.h>

#include "scwm.h"

void init_input_hooks();
void run_input_hooks(fd_set *in_fdset);
void add_hook_fds_to_set(fd_set *in_fdset, int *fd_width);
SCM add_input_hook (SCM fd, SCM thunk);

void DispatchEvent(void);

void InitEventHandlerJumpTable(void);

int XNextEvent_orTimeout(Display * dpy, XEvent * event);

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
void HandleVisibilityNotify(void);
void CoerceEnterNotifyOnCurrentWindow();

void init_events();

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
