/* $Id$
 * move.h
 * Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
 */

#ifndef MOVE_H
#define MOVE_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/X.h>
#include <X11/Xlib.h>

#include "window_fwd.h"


void moveLoop(ScwmWindow *psw,
              int XOffset, int YOffset, int Width,
	      int Height, int *FinalX, int *FinalY, Bool fOpaque);


void Keyboard_shortcuts(XEvent * Event, int ReturnEvent, 
                        const ScwmWindow *psw, Bool fResize);

Window WFrameOrIcon(ScwmWindow *psw);
Bool InteractiveMove(ScwmWindow *psw, Bool fOpaque, int *FinalX, int *FinalY);

#endif MOVE_H

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

