/* $Id$
 * focus.h
 * Copyright 1997, 1998
 * Maciej Stachowiak and Greg J. Badros
 */

#ifndef FOCUS_H_
#define FOCUS_H_


#include <config.h>
#include "screen.h"

#include "scwm.h"
void SetFocus(Window, ScwmWindow *, Bool FocusByMouse);
void Unfocus();

#endif
