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

void HandleHardFocus(ScwmWindow * t);

void init_input_hooks();
void run_input_hooks(fd_set *in_fdset);
void add_hook_fds_to_set(fd_set *in_fdset);
SCM add_input_hook (SCM fd, SCM thunk);


#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
