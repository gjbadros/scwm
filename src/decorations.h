#ifndef DECORATIONS_H
#define DECORATIONS_H

#include "scwm.h"

Bool check_allowed_function(enum wm_client_functions function, ScwmWindow * t);
void SelectDecor(ScwmWindow * t, int border_width, int resize_width);
void GetOlHints(ScwmWindow * t);
void GetMwmHints(ScwmWindow * t);

#endif /* DECORATIONS_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
