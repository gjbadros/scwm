#ifndef EVENTS_H
#define EVENTS_H
#include <libguile.h>

#include "scwm.h"

SCM send_button_press(SCM button, SCM modifier, SCM win,
		      SCM button_press_p, SCM button_release_p,
		      SCM propagate_p);

SCM send_key_press(SCM key, SCM win,
		   SCM button_press_p, SCM button_release_p,
		   SCM propagate_p);

void init_input_hooks();
void run_input_hooks(fd_set *in_fdset);
void add_hook_fds_to_set(fd_set *in_fdset, int *fd_width);
SCM add_input_hook (SCM fd, SCM thunk);

void DispatchEvent(void);

void InitEventHandlerJumpTable(void);

int XNextEvent_orTimeout(Display * dpy, XEvent * event);

void HandlePaging(int, int, int *, int *, int *, int *, Bool);

void HandleHardFocus(ScwmWindow * t);

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


#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
