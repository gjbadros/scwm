/* $Id$
 * focus.h
 * Copyright 1997, 1998
 * Maciej Stachowiak and Greg J. Badros
 */

#ifndef FOCUS_H_
#define FOCUS_H_


#include <config.h>
#include "scwm.h"
#include "screen.h"

extern Time lastTimestamp;

void SetFocus(Window, ScwmWindow *, Bool FocusByMouse);
void Unfocus();
Bool StashEventTime(XEvent * ev);

#endif
