/* $Id$
 * decorations.h
 * Copyright (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef DECORATIONS_H
#define DECORATIONS_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "scwm.h"
#include "window_fwd.h"

Bool check_allowed_function(enum wm_client_functions function, ScwmWindow * t);
void SelectDecor(ScwmWindow * t, int border_width, int resize_width);
void GetOlHints(ScwmWindow * t);
void GetMwmHints(ScwmWindow * t);

#endif /* DECORATIONS_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

