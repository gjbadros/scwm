/* $Id$
 * placement.h
 * Copyright (C) 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef PLACEMENT_H
#define PLACEMENT_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "window_fwd.h"

Bool PlaceWindow(struct ScwmWindow *psw);
void GetGravityOffsets(ScwmWindow *psw);
void SetPswGravity(ScwmWindow *psw, int grav);

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

