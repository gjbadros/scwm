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

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
