/* $Id$
 * focus.h
 * Copyright 1997, 1998
 * Maciej Stachowiak and Greg J. Badros
 */

#ifndef FOCUS_H_
#define FOCUS_H_


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "scwm.h"
#include "screen.h"

extern Time lastTimestamp;

void SetFocus(Window, ScwmWindow *, Bool FocusByMouse);
void Unfocus();
Bool StashEventTime(XEvent * ev);

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
